(ns blaze.db.impl.search-param.quantity
  (:require
    [blaze.anomaly :refer [if-ok when-ok]]
    [blaze.coll.core :as coll]
    [blaze.db.bytes :as bytes]
    [blaze.db.impl.byte-string :as bs]
    [blaze.db.impl.codec :as codec]
    [blaze.db.impl.index.resource-as-of :as rao]
    [blaze.db.impl.protocols :as p]
    [blaze.db.impl.search-param.util :as u]
    [blaze.db.search-param-registry :as sr]
    [blaze.fhir-path :as fhir-path]
    [blaze.fhir.spec :as fhir-spec]
    [blaze.fhir.spec.type :as type]
    [blaze.fhir.spec.type.system :as system]
    [clojure.string :as str]
    [cognitect.anomalies :as anom]
    [taoensso.timbre :as log]))


(set! *warn-on-reflection* true)


(defmulti quantity-index-entries
  "Returns index entries for `value` from a resource.

  The supplied function `entries-fn` takes a unit and a value and is used to
  create the actual index entries. Multiple such `entries-fn` results can be
  combined to one coll of entries."
  {:arglists '([url entries-fn value])}
  (fn [_ _ value] (fhir-spec/fhir-type value)))


(defmethod quantity-index-entries :fhir/Quantity
  [_ entries-fn {:keys [value system code unit]}]
  (let [value (type/value value)
        system (type/value system)
        code (type/value code)
        unit (type/value unit)]
    (cond-> []
      value
      (into (entries-fn nil value))
      code
      (into (entries-fn code value))
      (and unit (not= unit code))
      (into (entries-fn unit value))
      (and system code)
      (into (entries-fn (str system "|" code) value)))))


(defmethod quantity-index-entries :default
  [url _ value]
  (log/warn (u/format-skip-indexing-msg value url "quantity")))


(defn invalid-decimal-value-msg [code value]
  (format "Invalid decimal value `%s` in search parameter `%s`." value code))


(defn unsupported-prefix-msg [code op]
  (format "Unsupported prefix `%s` in search parameter `%s`." (name op) code))


(defn- resource-value
  "Returns the value of the resource with `tid` and `id` according to the
  search parameter with `c-hash` starting with `prefix`.

  The `prefix` is important, because resources have more than one index entry
  and so more than one value per search parameter. Different unit
  representations and other possible prefixes from composite search parameters
  are responsible for the multiple values."
  [{:keys [rsvi] :as context} c-hash tid id prefix]
  (let [handle (rao/resource-handle context tid id)]
    (u/get-next-value rsvi handle c-hash prefix)))


(defn- id-start-key [context c-hash tid prefix start-id]
  (let [start-value (resource-value context c-hash tid start-id prefix)]
    (assert start-value)
    (codec/sp-value-resource-key c-hash tid start-value start-id)))


(defn- eq-ge-start-key
  "Returns the key at with equal or greater equal collections will start."
  [context c-hash tid prefix value start-id]
  (if start-id
    (id-start-key context c-hash tid prefix start-id)
    (codec/sp-value-resource-key c-hash tid value)))


(defn- take-while-less-equal [c-hash tid value]
  (let [prefix-key (codec/sp-value-resource-key c-hash tid value)]
    (take-while (fn [[prefix]] (bytes/<= prefix prefix-key)))))


(defn- eq-keys
  "Returns a reducible collection of decoded SearchParamValueResource keys of
  values between `lower-bound` and `upper-bound` starting at `start-id`
  (optional).

  Decoded keys consist of the triple [prefix id hash-prefix].

  The `prefix` is a fix prefix of `value` which all found values have to have."
  [{:keys [svri] :as context} c-hash tid lower-bound-prefix lower-bound
   upper-bound start-id]
  (coll/eduction
    (take-while-less-equal c-hash tid upper-bound)
    (u/sp-value-resource-keys svri (eq-ge-start-key context c-hash tid
                                                    lower-bound-prefix
                                                    lower-bound start-id))))


(defn- gt-start-key [context c-hash tid prefix value start-id]
  (if start-id
    (id-start-key context c-hash tid prefix start-id)
    (codec/sp-value-resource-key-for-prev c-hash tid value)))


(defn- take-while-same-prefix [c-hash tid prefix]
  (let [prefix-key (codec/sp-value-resource-key c-hash tid prefix)]
    (take-while (fn [[prefix]] (bytes/starts-with? prefix prefix-key)))))


(defn- gt-keys
  "Returns a reducible collection of decoded SearchParamValueResource keys of
  values greater than `value` starting at `start-id` (optional).

  Decoded keys consist of the triple [prefix id hash-prefix].

  The `prefix` is a fix prefix of `value` which all found values have to have."
  [{:keys [svri] :as context} c-hash tid prefix value start-id]
  (coll/eduction
    (take-while-same-prefix c-hash tid prefix)
    (u/sp-value-resource-keys svri (gt-start-key context c-hash tid prefix value
                                                 start-id))))


(defn- id-start-key-for-prev [context c-hash tid prefix start-id]
  (let [start-value (resource-value context c-hash tid start-id prefix)]
    (assert start-value)
    (codec/sp-value-resource-key-for-prev c-hash tid start-value start-id)))


(defn- lt-start-key [context c-hash tid prefix value start-id]
  (if start-id
    (id-start-key-for-prev context c-hash tid prefix start-id)
    (codec/sp-value-resource-key c-hash tid value)))


(defn- lt-keys
  "Returns a reducible collection of decoded SearchParamValueResource keys of
  values less than `value` starting at `start-id` (optional).

  Decoded keys consist of the triple [prefix id hash-prefix].

  The `prefix` is a fix prefix of `value` which all found values have to have."
  [{:keys [svri] :as context} c-hash tid prefix value start-id]
  (coll/eduction
    (take-while-same-prefix c-hash tid prefix)
    (u/sp-value-resource-keys-prev svri (lt-start-key context c-hash tid prefix
                                                      value start-id))))


(defn- ge-keys
  "Returns a reducible collection of decoded SearchParamValueResource keys of
  values greater or equal to `value` starting at `start-id` (optional).

  Decoded keys consist of the triple [prefix id hash-prefix].

  The `prefix` is a fix prefix of `value` which all found values have to have."
  [{:keys [svri] :as context} c-hash tid prefix value start-id]
  (coll/eduction
    (take-while-same-prefix c-hash tid prefix)
    (u/sp-value-resource-keys svri (eq-ge-start-key context c-hash tid prefix
                                                    value start-id))))


(defn- le-start-key [context c-hash tid prefix value start-id]
  (if start-id
    (id-start-key-for-prev context c-hash tid prefix start-id)
    (codec/sp-value-resource-key-for-prev c-hash tid value)))


(defn- le-keys
  "Returns a reducible collection of decoded SearchParamValueResource keys of
  values less or equal to `value` starting at `start-id` (optional).

  Decoded keys consist of the triple [prefix id hash-prefix].

  The `prefix` is a fix prefix of `value` which all found values have to have."
  [{:keys [svri] :as context} c-hash tid prefix value start-id]
  (coll/eduction
    (take-while-same-prefix c-hash tid prefix)
    (u/sp-value-resource-keys-prev svri (le-start-key context c-hash tid prefix
                                                      value start-id))))


(defn resource-keys
  "Returns a reducible collection of decoded SearchParamValueResource keys of
  values according to `op` and values starting at `start-id` (optional).

  Decoded keys consist of the triple [prefix id hash-prefix].

  The `prefix-length` is the length of the fix prefix which all found values
  have to have."
  [context c-hash tid prefix-length [op lower-bound exact-value upper-bound]
   start-id]
  (case op
    :eq (eq-keys context c-hash tid (bs/subs lower-bound 0 prefix-length)
                 lower-bound upper-bound start-id)
    :gt (gt-keys context c-hash tid (bs/subs exact-value 0 prefix-length)
                 exact-value start-id)
    :lt (lt-keys context c-hash tid (bs/subs exact-value 0 prefix-length)
                 exact-value start-id)
    :ge (ge-keys context c-hash tid (bs/subs exact-value 0 prefix-length)
                 exact-value start-id)
    :le (le-keys context c-hash tid (bs/subs exact-value 0 prefix-length)
                 exact-value start-id)))


(defn- resource-sp-value [{:keys [rsvi]} {:keys [tid id hash]} c-hash value]
  (let [start-key (codec/resource-sp-value-key tid (codec/id-bytes id) hash c-hash value)]
    (second (coll/first (u/resource-sp-value-keys rsvi start-key)))))


(defn eq-matches? [context c-hash resource-handle lower-bound upper-bound]
  (when-let [value (resource-sp-value context resource-handle c-hash lower-bound)]
    (bs/<= value upper-bound)))


(defn gt-matches? [context c-hash resource-handle prefix value]
  (when-let [found-value (resource-sp-value context resource-handle c-hash value)]
    (and (bs/starts-with? found-value prefix)
         (bs/> found-value value))))


(defn- resource-sp-value-prev [{:keys [rsvi]} {:keys [tid id hash]} c-hash value]
  (let [start-key (codec/resource-sp-value-key tid (codec/id-bytes id) hash c-hash value)]
    (second (coll/first (u/resource-sp-value-keys-prev rsvi start-key)))))


(defn lt-matches? [context c-hash resource-handle prefix value]
  (when-let [found-value (resource-sp-value-prev context resource-handle c-hash
                                                 value)]
    (and (bs/starts-with? found-value prefix)
         (bs/< found-value value))))


(defn ge-matches? [context c-hash resource-handle prefix value]
  (when-let [found-value (resource-sp-value context resource-handle c-hash value)]
    (bs/starts-with? found-value prefix)))


(defn le-matches? [context c-hash resource-handle prefix value]
  (when-let [found-value (resource-sp-value-prev context resource-handle c-hash
                                                 value)]
    (bs/starts-with? found-value prefix)))


(defn matches?
  [context c-hash resource-handle prefix-length [op lower-bound exact-value
                                                 upper-bound]]
  (case op
    :eq (eq-matches? context c-hash resource-handle lower-bound upper-bound)
    :gt (gt-matches? context c-hash resource-handle
                     (bs/subs exact-value 0 prefix-length) exact-value)
    :lt (lt-matches? context c-hash resource-handle
                     (bs/subs exact-value 0 prefix-length) exact-value)
    :ge (ge-matches? context c-hash resource-handle
                     (bs/subs exact-value 0 prefix-length) exact-value)
    :le (le-matches? context c-hash resource-handle
                     (bs/subs exact-value 0 prefix-length) exact-value)))


(defrecord SearchParamQuantity [name url type base code c-hash expression]
  p/SearchParam
  (-compile-value [_ _ value]
    (let [[op value-and-unit] (u/separate-op value)
          [value unit] (str/split value-and-unit #"\|" 2)]
      (if-ok [decimal-value (system/parse-decimal value)]
        (let [delta (.movePointLeft 0.5M (.scale ^BigDecimal decimal-value))]
          (case op
            (:eq :gt :lt :ge :le)
            [op
             (codec/quantity unit (- decimal-value delta))
             (codec/quantity unit decimal-value)
             (codec/quantity unit (+ decimal-value delta))]
            {::anom/category ::anom/unsupported
             ::category ::unsupported-prefix
             ::unsupported-prefix op
             ::anom/message (unsupported-prefix-msg code op)}))
        (assoc decimal-value
          ::category ::invalid-decimal-value
          ::anom/message (invalid-decimal-value-msg code value)))))

  (-resource-handles [_ context tid _ value start-id]
    (coll/eduction
      (u/resource-handle-mapper context tid)
      (resource-keys context c-hash tid codec/v-hash-size value start-id)))

  (-compartment-keys [_ context compartment tid compiled-value]
    (let [{co-c-hash :c-hash co-res-id :res-id} compartment
          start-key (codec/compartment-search-param-value-key
                      co-c-hash co-res-id c-hash tid compiled-value)]
      (u/prefix-keys (:csvri context) start-key)))

  (-matches? [_ context resource-handle _ values]
    (some #(matches? context c-hash resource-handle codec/v-hash-size %) values))

  (-index-values [search-param resolver resource]
    (when-ok [values (fhir-path/eval resolver expression resource)]
      (p/-compile-index-values search-param values)))

  (-compile-index-values [_ values]
    (into
      []
      (mapcat
        #(quantity-index-entries
           url
           (fn [unit value]
             [[nil (codec/quantity unit value)]])
           %))
      values)))


(defmethod sr/search-param "quantity"
  [_ {:keys [name url type base code expression]}]
  (if expression
    (when-ok [expression (fhir-path/compile expression)]
      (->SearchParamQuantity name url type base code (codec/c-hash code)
                             expression))
    {::anom/category ::anom/unsupported
     ::anom/message (u/missing-expression-msg url)}))

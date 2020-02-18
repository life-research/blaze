(ns blaze.fhir.operation.evaluate-measure.measure
  (:require
    [blaze.datomic.pull :as pull]
    [blaze.datomic.util :as datomic-util]
    [blaze.datomic.value :as value]
    [blaze.cql-translator :as cql-translator]
    [blaze.elm.code :refer [code?]]
    [blaze.elm.compiler :as compiler]
    [blaze.fhir.operation.evaluate-measure.cql :as cql]
    [blaze.fhir.operation.evaluate-measure.spec :as spec]
    [blaze.handler.fhir.util :as fhir-util]
    [blaze.terminology-service :refer [term-service?]]
    [clojure.spec.alpha :as s]
    [cognitect.anomalies :as anom]
    [datomic-spec.core :as ds]
    [prometheus.alpha :as prom]
    [reitit.core :as reitit]
    [taoensso.timbre :as log])
  (:import
    [datomic Entity]
    [java.nio.charset Charset]
    [java.time OffsetDateTime]
    [java.time.temporal Temporal]))


(prom/defhistogram compile-duration-seconds
  "$evaluate-measure compiling latencies in seconds."
  {:namespace "fhir"
   :subsystem "evaluate_measure"}
  (take 12 (iterate #(* 2 %) 0.001)))


(prom/defhistogram evaluate-duration-seconds
  "$evaluate-measure evaluating latencies in seconds."
  {:namespace "fhir"
   :subsystem "evaluate_measure"}
  (take 12 (iterate #(* 2 %) 0.04)))


(def ^:private ^Charset utf-8 (Charset/forName "utf8"))


(defn- extract-cql-code [{:Library/keys [url content]}]
  (if-let [{:Attachment/keys [data]} (first content)]
    (if data
      (String. ^bytes (value/read data) utf-8)
      {::anom/category ::anom/incorrect
       ::anom/message
       (str "Missing embedded data of first attachment in library with "
            "canonical URI `" url "`.")
       :fhir/issue "value"})
    {::anom/category ::anom/incorrect
     ::anom/message
     (str "Missing content in library with canonical URI `" url "`.")
     :fhir/issue "value"}))


(defn- compile-library [db term-service {:Library/keys [id] :as library}]
  (log/debug "Compile library with ID:" id)
  (let [cql-code (extract-cql-code library)]
    (if (::anom/category cql-code)
      cql-code
      (let [library (cql-translator/translate cql-code :locators? true)]
        (case (::anom/category library)
          ::anom/incorrect
          (assoc library
            :fhir/issue "value"
            :fhir.issue/expression "Measure.library")
          (compiler/compile-library db term-service library {}))))))


(defn- compile-primary-library*
  [db term-service measure]
  (if-let [library-ref (first (:Measure/library measure))]
    (if-let [library (datomic-util/resource-by db :Library/url library-ref)]
      (compile-library db term-service library)
      {::anom/category ::anom/incorrect
       ::anom/message
       (str "Can't find the library with canonical URI `" library-ref "`.")
       :fhir/issue "value"
       :fhir.issue/expression "Measure.library"})
    {::anom/category ::anom/unsupported
     ::anom/message "Missing primary library. Currently only CQL expressions together with one primary library are supported."
     :fhir/issue "not-supported"
     :fhir.issue/expression "Measure.library"
     :measure measure}))


(defn- pull-codeable-concept [db concept]
  (pull/pull-non-primitive db :CodeableConcept concept))


(defn- compile-primary-library
  "Returns the primary library from `measure` in compiled form or an anomaly
  on errors."
  [db term-service measure]
  (with-open [_ (prom/timer compile-duration-seconds)]
    (compile-primary-library* db term-service measure)))


(defn- evaluate-population
  {:arglists '([db now library subject groupIdx populationIdx population])}
  [db now library subject groupIdx populationIdx
   {{:Expression/keys [language expression]} :Measure.group.population/criteria
    :Measure.group.population/keys [code]}]
  (cond
    (not= "text/cql" (:code/code language))
    {::anom/category ::anom/unsupported
     ::anom/message (str "Unsupported language `" (:code/code language) "`.")
     :fhir/issue "not-supported"
     :fhir.issue/expression
     (format "Measure.group[%d].population[%d].criteria.language"
             groupIdx populationIdx)}

    (nil? expression)
    {::anom/category ::anom/incorrect
     ::anom/message "Missing expression."
     :fhir/issue "required"
     :fhir.issue/expression
     (format "Measure.group[%d].population[%d].criteria.expression"
             groupIdx populationIdx)}

    :else
    (cond->
      {"count" (cql/evaluate-expression db now library subject expression)}
      code
      (assoc "code" (pull-codeable-concept db code)))))


(defn- fhir-code? [x]
  (and (instance? Entity x) (= "code" (datomic-util/entity-type x))))


(defn- pull [stratum-value]
  (cond
    (code? stratum-value)
    (:code stratum-value)
    (fhir-code? stratum-value)
    (:code/code stratum-value)
    :else
    (str stratum-value)))


(defn- evaluate-single-stratifier
  {:arglists
   '([db now library subject groupIdx populations stratifierIdx stratifier])}
  [db now library subject groupIdx populations stratifierIdx
   {{:Expression/keys [language expression]} :Measure.group.stratifier/criteria
    :Measure.group.stratifier/keys [code]}]
  (cond
    (not= "text/cql" (:code/code language))
    {::anom/category ::anom/unsupported
     ::anom/message (str "Unsupported language `" (:code/code language) "`.")
     :fhir/issue "not-supported"
     :fhir.issue/expression
     (format "Measure.group[%d].stratifier[%d].criteria.language"
             groupIdx stratifierIdx)}

    (nil? expression)
    {::anom/category ::anom/incorrect
     ::anom/message "Missing expression."
     :fhir/issue "required"
     :fhir.issue/expression
     (format "Measure.group[%d].stratifier[%d].criteria.expression"
             groupIdx stratifierIdx)}

    :else
    (let [stratums (cql/calc-stratums
                     db now library subject
                     (-> populations first :Measure.group.population/criteria
                         :Expression/expression)
                     expression)]
      (if (::anom/category stratums)
        stratums
        (cond->
          {"stratum"
           (->> stratums
                (mapv
                  (fn [[stratum-value count]]
                    [(pull stratum-value) count]))
                (sort-by first)
                (mapv
                  (fn [[stratum-value count]]
                    {"value" {"text" stratum-value}
                     "population"
                     [{"code"
                       (pull-codeable-concept
                         db (-> populations first :Measure.group.population/code))
                       "count" count}]})))}
          code
          (assoc "code" [(pull-codeable-concept db code)]))))))


(defn- extract-stratifier-component
  "Extracts code and expression-name from `stratifier-component`."
  {:arglists '([groupIdx stratifierIdx componentIdx stratifier-component])}
  [groupIdx stratifierIdx componentIdx
   {{:Expression/keys [language expression]}
    :Measure.group.stratifier.component/criteria
    :Measure.group.stratifier.component/keys [code]}]
  (cond
    (nil? code)
    {::anom/category ::anom/incorrect
     ::anom/message "Missing code."
     :fhir/issue "required"
     :fhir.issue/expression
     (format "Measure.group[%d].stratifier[%d].component[%d].code"
             groupIdx stratifierIdx componentIdx)}

    (not= "text/cql" (:code/code language))
    {::anom/category ::anom/unsupported
     ::anom/message (str "Unsupported language `" (:code/code language) "`.")
     :fhir/issue "not-supported"
     :fhir.issue/expression
     (format "Measure.group[%d].stratifier[%d].component[%d].criteria.language"
             groupIdx stratifierIdx componentIdx)}

    (nil? expression)
    {::anom/category ::anom/incorrect
     ::anom/message "Missing expression."
     :fhir/issue "required"
     :fhir.issue/expression
     (format "Measure.group[%d].stratifier[%d].component[%d].criteria.expression"
             groupIdx stratifierIdx componentIdx)}

    :else [code expression]))


(defn- extract-stratifier-components
  "Extracts code and expression-name from each of `stratifier-components`."
  [groupIdx stratifierIdx stratifier-components]
  (transduce
    (map-indexed vector)
    (completing
      (fn [results [idx component]]
        (let [result (extract-stratifier-component
                       groupIdx stratifierIdx idx component)]
          (if (::anom/category result)
            (reduced result)
            (let [[code expression-name] result]
              (-> results
                  (update :codes conj code)
                  (update :expression-names conj expression-name)))))))
    {:codes []
     :expression-names []}
    stratifier-components))


(defn- evaluate-multi-component-stratifier
  {:arglists
   '([db now library subject groupIdx populations stratifierIdx stratifier])}
  [db now library subject groupIdx populations stratifierIdx
   {:Measure.group.stratifier/keys [component]}]
  (let [results (extract-stratifier-components groupIdx stratifierIdx component)]
    (if (::anom/category results)
      results
      (let [{:keys [codes expression-names]} results
            stratums (cql/calc-mult-component-stratums
                       db now library subject
                       (-> populations first :Measure.group.population/criteria
                           :Expression/expression)
                       expression-names)]
        (if (::anom/category stratums)
          stratums
          (let [codes (mapv #(pull-codeable-concept db %) codes)]
            {"code" codes
             "stratum"
             (into
               []
               (map
                 (fn [[stratum-values count]]
                   {"component"
                    (mapv
                      (fn [code value]
                        {"code" code
                         "value" {"text" (pull value)}})
                      codes
                      stratum-values)
                    "population"
                    [{"code"
                      (pull-codeable-concept
                        db (-> populations first :Measure.group.population/code))
                      "count" count}]}))
               stratums)}))))))


(defn- evaluate-stratifier
  {:arglists
   '([db now library subject groupIdx populations stratifierIdx stratifier])}
  [db now library subject groupIdx populations stratifierIdx
   {:Measure.group.stratifier/keys [component] :as stratifier}]
  (if (seq component)
    (evaluate-multi-component-stratifier
      db now library subject groupIdx populations stratifierIdx stratifier)
    (evaluate-single-stratifier
      db now library subject groupIdx populations stratifierIdx stratifier)))


(defn- evaluate-populations [db now library subject groupIdx populations]
  (transduce
    (map-indexed (partial evaluate-population db now library subject groupIdx))
    (completing
      (fn [res evaluated-population]
        (if (::anom/category evaluated-population)
          (reduced evaluated-population)
          (conj res evaluated-population))))
    []
    populations))


(defn- evaluate-stratifiers
  [db now library subject groupIdx populations stratifiers]
  (transduce
    (map-indexed
      (partial evaluate-stratifier db now library subject groupIdx populations))
    (completing
      (fn [res evaluated-stratifier]
        (if (::anom/category evaluated-stratifier)
          (reduced evaluated-stratifier)
          (conj res evaluated-stratifier))))
    []
    stratifiers))


(defn- evaluate-group
  {:arglists '([db now library subject groupIdx group])}
  [db now library subject groupIdx
   {:Measure.group/keys [code population stratifier]}]
  (let [evaluated-populations
        (evaluate-populations db now library subject groupIdx population)
        evaluated-stratifiers
        (evaluate-stratifiers
          db now library subject groupIdx population stratifier)]
    (cond
      (::anom/category evaluated-populations)
      evaluated-populations

      (::anom/category evaluated-stratifiers)
      evaluated-stratifiers

      :else
      (cond-> {}
        code
        (assoc "code" code)

        (seq evaluated-populations)
        (assoc "population" evaluated-populations)

        (seq evaluated-stratifiers)
        (assoc "stratifier" evaluated-stratifiers)))))


(defn- evaluate-groups* [db now library subject groups]
  (transduce
    (map-indexed (partial evaluate-group db now library subject))
    (completing
      (fn [res evaluated-group]
        (if (::anom/category evaluated-group)
          (reduced evaluated-group)
          (conj res evaluated-group))))
    []
    groups))


(defn- evaluate-groups [db now library subject groups]
  (with-open [_ (prom/timer evaluate-duration-seconds)]
    (evaluate-groups* db now library subject groups)))


(defn- canonical [router {:Measure/keys [id url version]}]
  (if url
    (cond-> url version (str "|" version))
    (fhir-util/instance-url router "Measure" id)))


(defn- subject-code
  [{{:CodeableConcept/keys [coding]} :Measure/subjectCodeableConcept}]
  (or (-> coding first :Coding/code :code/code) "Patient"))


(defn- temporal? [x]
  (instance? Temporal x))


(s/fdef evaluate-measure
  :args
  (s/cat
    :now #(instance? OffsetDateTime %)
    :db ::ds/db
    :term-service term-service?
    :router reitit/router?
    :period (s/tuple temporal? temporal?)
    :measure ::spec/measure-entity))

(defn evaluate-measure
  "Evaluates `measure` inside `period` in `db` with evaluation time of `now`.

  Returns an already completed MeasureReport which isn't persisted or an anomaly
  in case of errors."
  {:arglists '([now db term-service router period measure])}
  [now db term-service router [start end] {groups :Measure/group :as measure}]
  (let [library (compile-primary-library db term-service measure)]
    (if (::anom/category library)
      library
      (let [evaluated-groups
            (evaluate-groups db now library (subject-code measure) groups)]
        (if (::anom/category evaluated-groups)
          evaluated-groups
          (cond->
            {"resourceType" "MeasureReport"
             "status" "complete"
             "type" "summary"
             "measure" (canonical router measure)
             "date" (pull/to-json now)
             "period"
             {"start" (pull/to-json start)
              "end" (pull/to-json end)}}

            (seq evaluated-groups)
            (assoc "group" evaluated-groups)))))))

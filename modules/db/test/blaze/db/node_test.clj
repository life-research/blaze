(ns blaze.db.node-test
  (:require
    [blaze.anomaly :refer [ex-anom]]
    [blaze.async.comp :as ac]
    [blaze.async.comp-spec]
    [blaze.db.api :as d]
    [blaze.db.api-spec]
    [blaze.db.impl.db-spec]
    [blaze.db.impl.index.tx-success :as tx-success]
    [blaze.db.kv :as kv]
    [blaze.db.kv.mem :refer [new-mem-kv-store]]
    [blaze.db.kv.mem-spec]
    [blaze.db.node :as node]
    [blaze.db.node-spec]
    [blaze.db.node.resource-indexer :as resource-indexer]
    [blaze.db.resource-handle-cache]
    [blaze.db.resource-store :as rs]
    [blaze.db.resource-store.kv :as rs-kv :refer [new-kv-resource-store]]
    [blaze.db.search-param-registry :as sr]
    [blaze.db.tx-cache]
    [blaze.db.tx-log-spec]
    [blaze.db.tx-log.local :refer [new-local-tx-log]]
    [blaze.db.tx-log.local-spec]
    [blaze.executors :as ex]
    [clojure.spec.test.alpha :as st]
    [clojure.test :as test :refer [deftest is testing]]
    [cognitect.anomalies :as anom]
    [integrant.core :as ig]
    [java-time :as time]
    [juxt.iota :refer [given]]
    [taoensso.timbre :as log])
  (:import
    [com.github.benmanes.caffeine.cache Caffeine]
    [java.time Clock Instant ZoneId]
    [java.util.concurrent ExecutorService]))


(st/instrument)
(log/set-level! :trace)


(defn- fixture [f]
  (st/instrument)
  (f)
  (st/unstrument))


(test/use-fixtures :each fixture)


(def ^:private search-param-registry (sr/init-search-param-registry))


;; TODO: with this shared executor, it's not possible to run test in parallel
(def ^:private local-tx-log-executor
  (ex/single-thread-executor "local-tx-log"))


;; TODO: with this shared executor, it's not possible to run test in parallel
(def ^:private indexer-executor
  (ex/single-thread-executor "indexer"))


(def ^:private resource-store-executor
  (ex/single-thread-executor "resource-store"))


(defn new-index-kv-store []
  (new-mem-kv-store
    {:search-param-value-index nil
     :resource-value-index nil
     :compartment-search-param-value-index nil
     :compartment-resource-type-index nil
     :active-search-params nil
     :tx-success-index {:reverse-comparator? true}
     :tx-error-index nil
     :t-by-instant-index {:reverse-comparator? true}
     :resource-as-of-index nil
     :type-as-of-index nil
     :system-as-of-index nil
     :type-stats-index nil
     :system-stats-index nil}))


(def clock (Clock/fixed Instant/EPOCH (ZoneId/of "UTC")))


(defn- tx-cache [index-kv-store]
  (.build (Caffeine/newBuilder) (tx-success/cache-loader index-kv-store)))


(defn new-node-with [{:keys [resource-store]}]
  (let [tx-log (new-local-tx-log (new-mem-kv-store) clock local-tx-log-executor)
        resource-handle-cache (.build (Caffeine/newBuilder))
        index-kv-store (new-index-kv-store)]
    (node/new-node tx-log resource-handle-cache (tx-cache index-kv-store)
                   indexer-executor index-kv-store resource-store
                   search-param-registry (time/millis 10))))


(defn new-node []
  (new-node-with
    {:resource-store
     (new-kv-resource-store (new-mem-kv-store) resource-store-executor)}))


(defn new-resource-store-failing-on-get []
  (reify
    rs/ResourceLookup
    (-get [_ _]
      (ac/failed-future (ex-anom {::anom/category ::anom/fault})))
    (-multi-get [_ _]
      (ac/failed-future (ex-anom {::anom/category ::anom/fault})))
    rs/ResourceStore
    (-put [_ _]
      (ac/completed-future nil))))


(deftest transact-test
  (testing "with slow transaction result fetching"
    (testing "create"
      (testing "one Patient"
        (with-open [node (new-node)]
          @(-> (node/submit-tx node [[:create {:fhir/type :fhir/Patient :id "0"}]])
               (ac/then-compose
                 (fn [t]
                   (Thread/sleep 100)
                   (node/tx-result node t))))

          (given @(d/pull node (d/resource-handle (d/db node) "Patient" "0"))
            :fhir/type := :fhir/Patient
            :id := "0"
            [:meta :versionId] := #fhir/id"1"
            [meta :blaze.db/op] := :create))))

    (testing "with failing resource storage"
      (testing "on get"
        (with-open [node (new-node-with
                           {:resource-store (new-resource-store-failing-on-get)})]

          (try
            @(-> (node/submit-tx node [[:put {:fhir/type :fhir/Patient :id "0"}]])
                 (ac/then-compose
                   (fn [t]
                     (Thread/sleep 100)
                     (node/tx-result node t))))
            (catch Exception e
              (given (ex-data (ex-cause e))
                ::anom/category := ::anom/fault))))))

    (testing "with failing resource indexer"
      (with-redefs
        [resource-indexer/index-resources
         (fn [_ _ _]
           (ac/failed-future (ex-anom {::anom/category ::anom/fault ::x ::y})))]
        (with-open [node (new-node)]
          (try
            @(-> (node/submit-tx node [[:put {:fhir/type :fhir/Patient :id "0"}]])
                 (ac/then-compose
                   (fn [t]
                     (Thread/sleep 100)
                     (node/tx-result node t))))
            (catch Exception e
              (given (ex-data (ex-cause e))
                ::anom/category := ::anom/fault
                ::x ::y))))))))


(defmethod ig/init-key ::filled-index-kv-store
  [_ {:keys [column-families]}]
  (let [store (new-mem-kv-store column-families)]
    (kv/put! store [(tx-success/index-entry 1 Instant/EPOCH)])
    store))


(defn init-system
  [& {:keys [index-kv-store-key] :or {index-kv-store-key :blaze.db.kv/mem}}]
  (ig/init
    {:blaze.db/node
     {:tx-log (ig/ref :blaze.db.tx-log/local)
      :resource-handle-cache (ig/ref :blaze.db/resource-handle-cache)
      :tx-cache (ig/ref :blaze.db/tx-cache)
      :indexer-executor (ig/ref ::node/indexer-executor)
      :kv-store (ig/ref :blaze.db/index-kv-store)
      :resource-store (ig/ref :blaze.db/resource-store)
      :search-param-registry (ig/ref :blaze.db/search-param-registry)}
     :blaze.db.tx-log/local
     {:kv-store (ig/ref :blaze.db/transaction-kv-store)}
     [:blaze.db.kv/mem :blaze.db/transaction-kv-store]
     {:column-families {}}
     :blaze.db/resource-handle-cache {}
     :blaze.db/tx-cache
     {:kv-store (ig/ref :blaze.db/index-kv-store)}
     ::node/indexer-executor {}
     [index-kv-store-key :blaze.db/index-kv-store]
     {:column-families
      {:search-param-value-index nil
       :resource-value-index nil
       :compartment-search-param-value-index nil
       :compartment-resource-type-index nil
       :active-search-params nil
       :tx-success-index {:reverse-comparator? true}
       :tx-error-index nil
       :t-by-instant-index {:reverse-comparator? true}
       :resource-as-of-index nil
       :type-as-of-index nil
       :system-as-of-index nil
       :type-stats-index nil
       :system-stats-index nil}}
     ::rs/kv
     {:kv-store (ig/ref :blaze.db/resource-kv-store)
      :executor (ig/ref ::rs-kv/executor)}
     ::rs-kv/executor {}
     [:blaze.db.kv/mem :blaze.db/resource-kv-store]
     {:column-families {}}
     :blaze.db/search-param-registry {}}))


(deftest init-test
  (testing "with empty stores"
    (let [system (init-system)]
      (is (= 0 (d/basis-t (d/db (:blaze.db/node system)))))
      (ig/halt! system)))

  (testing "with filled stores"
    (let [system (init-system :index-kv-store-key ::filled-index-kv-store)]
      (is (= 1 (d/basis-t (d/db (:blaze.db/node system)))))
      (ig/halt! system))))


(deftest indexer-executor-test
  (let [system (ig/init {::node/indexer-executor {}})]
    (is (instance? ExecutorService (::node/indexer-executor system)))
    (ig/halt! system)))

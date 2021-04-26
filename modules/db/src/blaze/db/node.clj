(ns blaze.db.node
  "Local Database Node"
  (:require
    [blaze.anomaly :refer [when-ok ex-anom]]
    [blaze.async.comp :as ac]
    [blaze.db.api :as d]
    [blaze.db.impl.batch-db :as batch-db]
    [blaze.db.impl.codec :as codec]
    [blaze.db.impl.db :as db]
    [blaze.db.impl.index.t-by-instant :as ti]
    [blaze.db.impl.index.tx-error :as te]
    [blaze.db.impl.index.tx-success :as tsi]
    [blaze.db.impl.protocols :as p]
    [blaze.db.impl.search-param :as search-param]
    [blaze.db.kv :as kv]
    [blaze.db.node.resource-indexer :as resource-indexer :refer [new-resource-indexer]]
    [blaze.db.node.transaction :as tx]
    [blaze.db.node.tx-indexer :as tx-indexer]
    [blaze.db.node.tx-indexer.verify :as tx-indexer-verify]
    [blaze.db.node.validation :as validation]
    [blaze.db.resource-store :as rs]
    [blaze.db.search-param-registry :as sr]
    [blaze.db.search-param-registry.spec]
    [blaze.db.tx-log :as tx-log]
    [blaze.executors :as ex]
    [blaze.fhir.spec.type :as type]
    [blaze.module :refer [reg-collector]]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [cognitect.anomalies :as anom]
    [integrant.core :as ig]
    [java-time :as jt]
    [prometheus.alpha :as prom :refer [defhistogram]]
    [taoensso.timbre :as log])
  (:import
    [java.io Closeable]
    [java.util.concurrent TimeUnit ExecutorService CompletableFuture]))


(set! *warn-on-reflection* true)


(defhistogram duration-seconds
  "Node durations."
  {:namespace "blaze"
   :subsystem "db_node"
   :name "duration_seconds"}
  (take 16 (iterate #(* 2 %) 0.0001))
  "op")


(defhistogram transaction-sizes
  "Number of transaction commands per transaction."
  {:namespace "blaze"
   :subsystem "db_node"
   :name "transaction_sizes"}
  (take 16 (iterate #(* 2 %) 1)))


(defn- resolve-search-param [search-param-registry type code]
  (if-let [search-param (sr/get search-param-registry code type)]
    search-param
    {::anom/category ::anom/not-found
     ::anom/message (format "The search-param with code `%s` and type `%s` was not found." code type)}))


(defn- resolve-search-params [search-param-registry type clauses lenient?]
  (reduce
    (fn [ret [code & values]]
      (let [[code modifier] (str/split code #":" 2)
            values (distinct values)
            res (resolve-search-param search-param-registry type code)]
        (if (::anom/category res)
          (if lenient? ret (reduced res))
          (let [compiled-values (search-param/compile-values res modifier values)]
            (if (::anom/category compiled-values)
              (reduced compiled-values)
              (conj ret [res modifier values compiled-values]))))))
    []
    clauses))


(defn- add-watcher!
  "Adds a watcher to `state` and returns a CompletableFuture that will complete
  with nil if `t` is reached or complete exceptionally in case the indexing
  errored."
  [state t]
  (let [future (ac/future)]
    (add-watch
      state future
      (fn [future state _ {:keys [e] new-t :t new-error-t :error-t}]
        (cond
          (<= t (max new-t new-error-t))
          (do (ac/complete! future nil)
              (remove-watch state future))

          e
          (do (ac/complete-exceptionally! future e)
              (remove-watch state future)))))
    future))


(defn- load-tx-result [{:keys [tx-cache kv-store] :as node} t]
  (if (tsi/tx tx-cache t)
    (ac/completed-future (db/db node t))
    (if-let [anomaly (te/tx-error kv-store t)]
      (ac/failed-future
        (ex-info (format "Transaction with point in time %d errored." t)
                 anomaly))
      (ac/failed-future
        (ex-anom
          {::anom/category ::anom/fault
           ::anom/message
           (format "Can't find transaction result with point in time of %d." t)})))))


(defn- index-tx [db-before tx-data]
  (with-open [_ (prom/timer duration-seconds "index-transactions")]
    (tx-indexer/index-tx db-before tx-data)))


(defn- store-tx-entries [kv-store entries]
  (with-open [_ (prom/timer duration-seconds "store-tx-entries")]
    (kv/put! kv-store entries)))


(defn- advance-t! [state t]
  (log/trace "advance state to t =" t)
  (swap! state assoc :t t))


(defn- advance-error-t! [state t]
  (log/trace "advance state to error-t =" t)
  (swap! state assoc :error-t t))


(defn- hashes [tx-cmds]
  (mapv :hash tx-cmds))


(defn- index-resources
  [{:keys [tx-resource-cache resource-store resource-indexer]} t tx-cmds]
  (if-let [resources (@tx-resource-cache t)]
    (resource-indexer/index-resources resource-indexer resources)
    (-> (rs/multi-get resource-store (hashes tx-cmds))
        (ac/then-compose #(resource-indexer/index-resources resource-indexer %)))))


(defn- tx-success-entries [t instant]
  [(tsi/index-entry t instant)
   (ti/index-entry instant t)])


(defn- index-tx-data
  [{:keys [kv-store state tx-resource-cache] :as node}
   {:keys [t instant tx-cmds] :as tx-data}]
  (log/trace "index transaction with t =" t "and" (count tx-cmds) "command(s)")
  (prom/observe! transaction-sizes (count tx-cmds))
  (let [timer (prom/timer duration-seconds "index-resources")
        future (index-resources node t tx-cmds)
        result (index-tx (d/db node) tx-data)]
    (swap! tx-resource-cache dissoc t)
    (if (::anom/category result)
      (do (kv/put! kv-store [(te/index-entry t result)])
          (advance-error-t! state t))
      (do (store-tx-entries kv-store result)
          (try
            (log/trace "wait until resources are indexed...")
            (ac/join future)
            (log/trace "done indexing all resources")
            (catch Exception e
              (log/error "Error while resource indexing: " (ex-message (ex-cause e)))
              (throw e))
            (finally
              (prom/observe-duration! timer)))
          (kv/put! kv-store (tx-success-entries t instant))
          (advance-t! state t)))))


(defn- poll [node queue poll-timeout]
  (log/trace "poll transaction queue")
  (reduce #(index-tx-data node %2) nil (tx-log/poll queue poll-timeout)))


(defn- max-t [state]
  (let [{:keys [t error-t]} state]
    (max t error-t)))


(defn- enhance-resource-meta [meta t {:blaze.db.tx/keys [instant]}]
  (-> (or meta #fhir/Meta{})
      (assoc :versionId (type/->Id (str t)))
      (assoc :lastUpdated instant)))


(defn- mk-meta [{:keys [t num-changes op] :as handle} tx]
  (-> (meta handle)
      (assoc :blaze.db/t t)
      (assoc :blaze.db/num-changes num-changes)
      (assoc :blaze.db/op op)
      (assoc :blaze.db/tx tx)))


(defn- enhance-resource [tx-cache {:keys [t] :as handle} resource]
  (let [tx (tsi/tx tx-cache t)]
    (-> (update resource :meta enhance-resource-meta t tx)
        (with-meta (mk-meta handle tx)))))


(defrecord Node [tx-log rh-cache tx-cache kv-store resource-store
                 search-param-registry resource-indexer state tx-resource-cache
                 run? poll-timeout finished]
  p/Node
  (-db [node]
    (db/db node (:t @state)))

  (-sync [node t]
    (if (<= t (:t @state))
      (ac/completed-future (d/db node))
      (-> (add-watcher! state t)
          (ac/then-apply (fn [_] (db/db node t))))))

  (-submit-tx [_ tx-ops]
    (log/trace "submit" (count tx-ops) "tx-ops")
    (let [res (validation/validate-ops tx-ops)]
      (if (::anom/category res)
        (CompletableFuture/failedFuture (ex-info "" res))
        (let [[tx-cmds entries] (tx/prepare-ops tx-ops)]
          (-> (rs/put resource-store entries)
              (ac/then-compose (fn [_] (tx-log/submit tx-log tx-cmds)))
              (ac/then-apply
                (fn [t]
                  (swap! tx-resource-cache assoc t entries)
                  t)))))))

  (-tx-result [node t]
    (let [watcher (add-watcher! state t)
          current-state @state
          current-t (max-t current-state)]
      (log/trace "call tx-result: t =" t "current-t =" current-t)
      (cond
        (<= t current-t)
        (do (remove-watch state watcher)
            (load-tx-result node t))

        (:e current-state)
        (do (remove-watch state watcher)
            (ac/failed-future (:e current-state)))

        :else
        (ac/then-compose watcher (fn [_] (load-tx-result node t))))))

  p/Tx
  (-tx [_ t]
    (tsi/tx tx-cache t))

  p/QueryCompiler
  (-compile-type-query [_ type clauses]
    (when-ok [clauses (resolve-search-params search-param-registry type clauses
                                             false)]
      (batch-db/->TypeQuery (codec/tid type) (seq clauses))))

  (-compile-type-query-lenient [_ type clauses]
    (if-let [clauses (seq (resolve-search-params search-param-registry type
                                                 clauses true))]
      (batch-db/->TypeQuery (codec/tid type) clauses)
      (batch-db/->EmptyTypeQuery (codec/tid type))))

  (-compile-system-query [_ clauses]
    (when-ok [clauses (resolve-search-params search-param-registry "Resource"
                                             clauses false)]
      (batch-db/->SystemQuery (seq clauses))))

  (-compile-compartment-query [_ code type clauses]
    (when-ok [clauses (resolve-search-params search-param-registry type clauses
                                             false)]
      (batch-db/->CompartmentQuery (codec/c-hash code) (codec/tid type)
                                   (seq clauses))))

  (-compile-compartment-query-lenient [_ code type clauses]
    (if-let [clauses (seq (resolve-search-params search-param-registry type clauses
                                                 true))]
      (batch-db/->CompartmentQuery (codec/c-hash code) (codec/tid type)
                                   clauses)
      (batch-db/->EmptyCompartmentQuery (codec/c-hash code) (codec/tid type))))

  p/Pull
  (-pull [_ resource-handle]
    (-> (rs/get resource-store (:hash resource-handle))
        (ac/then-apply #(enhance-resource tx-cache resource-handle %))))

  (-pull-content [_ resource-handle]
    (-> (rs/get resource-store (:hash resource-handle))
        (ac/then-apply #(with-meta % (meta resource-handle)))))

  (-pull-many [_ resource-handles]
    (-> (rs/multi-get resource-store (mapv :hash resource-handles))
        (ac/then-apply
          (fn [resources]
            (mapv
              (fn [{:keys [hash] :as resource-handle}]
                (enhance-resource tx-cache resource-handle (get resources hash)))
              resource-handles)))))

  Runnable
  (run [node]
    (try
      (let [offset (inc (:t @state))]
        (log/trace "enter indexer and open transaction queue with offset =" offset)
        (with-open [queue (tx-log/new-queue tx-log offset)]
          (while @run?
            (try
              (poll node queue poll-timeout)
              (catch Exception e
                (swap! state assoc :e e)
                (throw e))))))
      (finally
        (ac/complete! finished true)
        (log/trace "exit indexer"))))

  Closeable
  (close [_]
    (log/trace "start closing")
    (vreset! run? false)
    @finished
    (log/trace "done closing")))


(defn- execute [executor node error]
  (-> (CompletableFuture/runAsync node executor)
      (ac/exceptionally
        (fn [e]
          (log/error "Error while indexing:" (ex-message (ex-cause e)))
          (swap! error (ex-cause e))))))


(defn new-node
  "Creates a new local database node."
  [tx-log resource-handle-cache tx-cache indexer-executor kv-store
   resource-store search-param-registry poll-timeout]
  (let [resource-indexer (new-resource-indexer search-param-registry kv-store)
        indexer-abort-reason (atom nil)
        node (->Node tx-log resource-handle-cache tx-cache kv-store
                     resource-store search-param-registry resource-indexer
                     (atom {:t (or (tsi/last-t kv-store) 0)
                            :error-t 0})
                     (atom {})
                     (volatile! true)
                     poll-timeout
                     (ac/future))]
    (execute indexer-executor node indexer-abort-reason)
    node))


(s/def ::indexer-executor
  ex/executor?)


(defmethod ig/pre-init-spec :blaze.db/node [_]
  (s/keys
    :req-un
    [:blaze.db/tx-log
     :blaze.db/resource-handle-cache
     :blaze.db/tx-cache
     ::indexer-executor
     :blaze.db/kv-store
     :blaze.db/resource-store
     :blaze.db/search-param-registry]))


(defmethod ig/init-key :blaze.db/node
  [_ {:keys [tx-log resource-handle-cache tx-cache indexer-executor kv-store
             resource-store search-param-registry]}]
  (log/info "Open local database node")
  (new-node tx-log resource-handle-cache tx-cache indexer-executor kv-store
            resource-store search-param-registry (jt/seconds 1)))


(defmethod ig/halt-key! :blaze.db/node
  [_ node]
  (log/info "Close local database node")
  (.close ^Closeable node))


(defmethod ig/init-key ::indexer-executor
  [_ _]
  (log/info "Init indexer executor")
  (ex/single-thread-executor "indexer"))


(defmethod ig/halt-key! ::indexer-executor
  [_ ^ExecutorService executor]
  (log/info "Stopping indexer executor...")
  (.shutdown executor)
  (if (.awaitTermination executor 10 TimeUnit/SECONDS)
    (log/info "Indexer executor was stopped successfully")
    (log/warn "Got timeout while stopping the indexer executor")))


(reg-collector ::duration-seconds
  duration-seconds)


(reg-collector ::transaction-sizes
  transaction-sizes)


(reg-collector :blaze.db.node.resource-indexer/duration-seconds
  resource-indexer/duration-seconds)


(reg-collector ::tx-indexer/duration-seconds
  tx-indexer-verify/duration-seconds)

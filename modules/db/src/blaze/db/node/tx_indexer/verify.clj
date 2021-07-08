(ns blaze.db.node.tx-indexer.verify
  (:require
    [blaze.anomaly :refer [throw-anom]]
    [blaze.db.api :as d]
    [blaze.db.impl.codec :as codec]
    [blaze.db.impl.index.rts-as-of :as rts]
    [blaze.db.impl.index.system-stats :as system-stats]
    [blaze.db.impl.index.type-stats :as type-stats]
    [blaze.db.kv.spec]
    [clojure.string :as str]
    [cognitect.anomalies :as anom]
    [prometheus.alpha :as prom :refer [defhistogram]]
    [taoensso.timbre :as log])
  (:import
    [clojure.lang ExceptionInfo IReduceInit]))


(set! *warn-on-reflection* true)


(defhistogram duration-seconds
  "Durations in transaction indexer."
  {:namespace "blaze"
   :subsystem "db"
   :name "tx_indexer_duration_seconds"}
  (take 16 (iterate #(* 2 %) 0.00001))
  "op")


(defmulti resolve-id
  "Resolves all identifiers a conditional transaction command.

  Throws an anomaly on conflicts."
  {:arglists '([db-before res cmd])}
  (fn [_ _ {:keys [op]}] op))


(defn- existing-resource-handles [db type clauses]
  (into [] (take 2) (d/type-query db type clauses)))


(defn- clauses->query-params [clauses]
  (->> clauses
       (map (fn [[param & values]] (str param "=" (str/join "," values))))
       (str/join "&")))


(defn- format-handle [type {:keys [id t]}]
  (format "%s/%s/_history/%s" type id t))


(defn- multiple-existing-resources-msg [type clauses [h1 h2]]
  (format "Conditional create of a %s with query `%s` failed because at least the two matches `%s` and `%s` were found."
          type (clauses->query-params clauses) (format-handle type h1)
          (format-handle type h2)))


(defn- throw-multiple-existing-resources-anomaly [type clauses handles]
  (throw-anom
    ::anom/conflict
    (multiple-existing-resources-msg type clauses handles)
    :http/status 412))


(defmethod resolve-id "create"
  [db-before res {:keys [type if-none-exist] :as cmd}]
  (let [[h1 h2] (some->> if-none-exist (existing-resource-handles db-before type))]
    (cond
      h2
      (throw-multiple-existing-resources-anomaly type if-none-exist [h1 h2])
      h1
      (let [{:keys [id]} h1]
        (log/trace (format "Resolved id `%s` for %s with query `%s`." id type
                           (clauses->query-params if-none-exist)))
        (-> (update res :cmds conj (assoc cmd :op "hold" :id id))
            (update :ref-mappings assoc [type (:id cmd)] [type id])))
      :else
      (update res :cmds conj cmd))))


(defmethod resolve-id :default
  [_ res cmd]
  (update res :cmds conj cmd))


(defn- map-refs [ref-mappings {:keys [refs] :as cmd}]
  (cond-> cmd refs (update :refs (partial mapv #(ref-mappings % %)))))


(defn- resolve-ids
  "Resolves all identifiers from conditional transaction commands.

  Throws an anomaly on conflicts."
  [db-before cmds]
  (let [{:keys [cmds ref-mappings]} (reduce (partial resolve-id db-before) {:cmds [] :ref-mappings {}} cmds)]
    (mapv #(map-refs ref-mappings %) cmds)))


(defmulti format-command :op)


(defmethod format-command "hold" [{:keys [type id if-none-exist]}]
  (format "create %s?%s (resolved to id %s)" type
          (clauses->query-params if-none-exist) id))


(defmethod format-command :default [{:keys [op type id]}]
  (format "%s %s/%s" op type id))


(defn- throw-duplicate-command-anomaly [cmd existing-cmd]
  (throw-anom
    ::anom/conflict
    (format "Duplicate transaction commands `%s` and `%s`." (format-command cmd)
            (format-command existing-cmd))))


(defn- detect-duplicate-commands! [cmds]
  (reduce
    (fn [index {:keys [type id] :as cmd}]
      (if-let [existing-cmd (get index [type id])]
        (throw-duplicate-command-anomaly cmd existing-cmd)
        (assoc index [type id] cmd)))
    {}
    cmds))


(defmulti verify-tx-cmd
  "Verifies one transaction command. Returns `res` with added index entries and
  statistics of the transaction outcome.

  Throws an anomaly on conflicts."
  {:arglists '([db-before t res cmd])}
  (fn [_ _ _ {:keys [op]}] op))


(defn- verify-tx-cmd-create-msg [type id]
  (format "verify-tx-cmd :create %s/%s" type id))


(defn- id-collision-msg [type id]
  (format "Resource `%s/%s` already exists and can't be created again." type id))


(defn- check-id-collision! [db type id]
  (when (d/resource-handle db type id)
    (throw-anom ::anom/conflict (id-collision-msg type id))))


(defn- index-entries [tid id t hash num-changes op]
  (rts/index-entries tid (codec/id-byte-string id) t hash num-changes op))


(defmethod verify-tx-cmd "create"
  [db-before t res {:keys [type id hash]}]
  (log/trace (verify-tx-cmd-create-msg type id))
  (with-open [_ (prom/timer duration-seconds "verify-create")]
    (check-id-collision! db-before type id)
    (let [tid (codec/tid type)]
      (-> res
          (update :entries into (index-entries tid id t hash 1 :create))
          (update :new-resources conj [type id])
          (update-in [:stats tid :num-changes] (fnil inc 0))
          (update-in [:stats tid :total] (fnil inc 0))))))


(defn- verify-tx-cmd-put-msg [type id matches]
  (if matches
    (format "verify-tx-cmd :put %s/%s matches-t: %d" type id matches)
    (format "verify-tx-cmd :put %s/%s" type id)))


(defn- throw-precondition-failed [if-match type id]
  (throw-anom
    ::anom/conflict
    (format "Precondition `W/\"%d\"` failed on `%s/%s`." if-match type id)
    :http/status 412))


(defmethod verify-tx-cmd "put"
  [db-before t res {:keys [type id hash if-match]}]
  (log/trace (verify-tx-cmd-put-msg type id if-match))
  (with-open [_ (prom/timer duration-seconds "verify-put")]
    (let [tid (codec/tid type)
          {:keys [num-changes op] :or {num-changes 0} old-t :t}
          (d/resource-handle db-before type id)]
      (if (or (nil? if-match) (= if-match old-t))
        (cond->
          (-> res
              (update :entries into (index-entries tid id t hash (inc num-changes) :put))
              (update :new-resources conj [type id])
              (update-in [:stats tid :num-changes] (fnil inc 0)))
          (or (nil? old-t) (identical? :delete op))
          (update-in [:stats tid :total] (fnil inc 0)))
        (throw-precondition-failed if-match type id)))))


(defmethod verify-tx-cmd "delete"
  [db-before t res {:keys [type id hash]}]
  (log/trace "verify-tx-cmd :delete" (str type "/" id))
  (with-open [_ (prom/timer duration-seconds "verify-delete")]
    (let [tid (codec/tid type)
          {:keys [num-changes] :or {num-changes 0}}
          (d/resource-handle db-before type id)]
      (-> res
          (update :entries into (index-entries tid id t hash (inc num-changes) :delete))
          (update :del-resources conj [type id])
          (update-in [:stats tid :num-changes] (fnil inc 0))
          (update-in [:stats tid :total] (fnil dec 0))))))


(defmethod verify-tx-cmd :default
  [_ _ res _]
  res)


(defn- verify-tx-cmds** [db-before t tx-cmds]
  (reduce
    (partial verify-tx-cmd db-before t)
    {:entries []
     :new-resources #{}
     :del-resources #{}}
    tx-cmds))


(defn- type-stat-entry!
  [iter t new-t [tid increments]]
  (let [current-stats (type-stats/get! iter tid t)]
    (type-stats/index-entry tid new-t (merge-with + current-stats increments))))


(defn- conj-type-stats [entries {{:keys [snapshot t]} :context} new-t stats]
  (with-open [_ (prom/timer duration-seconds "type-stats")
              iter (type-stats/new-iterator snapshot)]
    (into entries (map #(type-stat-entry! iter t new-t %)) stats)))


(defn- system-stats [{{:keys [snapshot t]} :context} new-t stats]
  (with-open [_ (prom/timer duration-seconds "system-stats")
              iter (system-stats/new-iterator snapshot)]
    (let [current-stats (system-stats/get! iter t)]
      (system-stats/index-entry new-t (apply merge-with + current-stats (vals stats))))))


(defn- post-process-res [db-before t {:keys [entries stats]}]
  (cond-> (conj-type-stats entries db-before t stats)
    stats
    (conj (system-stats db-before t stats))))


(defn- resource-exists? [db type id]
  (when-let [{:keys [op]} (d/resource-handle db type id)]
    (not (identical? :delete op))))


(defn- ref-integrity-del-msg [src-type src-id type id]
  (format "Referential integrity violated. Resource `%s/%s` should be deleted but is referenced from `%s/%s`."
          type id src-type src-id))


(defn- throw-referential-integrity-anomaly-delete [src-type src-id type id]
  (throw-anom ::anom/conflict (ref-integrity-del-msg src-type src-id type id)))


(defn- ref-integrity-msg [type id]
  (format "Referential integrity violated. Resource `%s/%s` doesn't exist."
          type id))


(defn- throw-referential-integrity-anomaly [type id]
  (throw-anom ::anom/conflict (ref-integrity-msg type id)))


(defn- check-referential-integrity*!
  [db new-resources del-resources src-type src-id references]
  (.reduce
    ^IReduceInit references
    (fn [_ [type id :as reference]]
      (cond
        (contains? del-resources reference)
        (throw-referential-integrity-anomaly-delete src-type src-id type id)

        (and (not (contains? new-resources reference))
             (not (resource-exists? db type id)))
        (throw-referential-integrity-anomaly type id)))
    nil))


(defn- check-referential-integrity!
  [db {:keys [new-resources del-resources]} cmds]
  (.reduce
    ^IReduceInit cmds
    (fn [_ {:keys [type id refs]}]
      (when refs
        (check-referential-integrity*!
          db new-resources del-resources type id refs)))
    nil))


(defn- verify-tx-cmds* [db-before t cmds]
  (try
    (let [cmds (resolve-ids db-before cmds)]
      (detect-duplicate-commands! cmds)
      (let [res (verify-tx-cmds** db-before t cmds)]
        (check-referential-integrity! db-before res cmds)
        (post-process-res db-before t res)))
    (catch ExceptionInfo e
      (if (::anom/category (ex-data e))
        (ex-data e)
        (throw e)))))


(defn verify-tx-cmds
  "Verifies transaction commands. Returns index entries of the transaction
  outcome if it is successful or an anomaly if it fails.

  The `t` is for the new transaction to commit."
  [db-before t cmds]
  (with-open [_ (prom/timer duration-seconds "verify-tx-cmds")
              batch-db-before (d/new-batch-db db-before)]
    (verify-tx-cmds* batch-db-before t cmds)))

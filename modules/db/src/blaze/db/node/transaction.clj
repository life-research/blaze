(ns blaze.db.node.transaction
  (:require
    [blaze.db.impl.codec :as codec]
    [blaze.fhir.hash :as hash]
    [blaze.fhir.hash-spec]
    [blaze.fhir.spec :as fhir-spec]
    [blaze.fhir.spec.type :as type]))


(defmulti prepare-op first)


(defmethod prepare-op :create
  [[op resource clauses]]
  (let [hash (hash/generate resource)
        refs (type/references resource)]
    {:hash-resource
     [hash resource]
     :blaze.db/tx-cmd
     (cond->
       {:op (name op)
        :type (name (fhir-spec/fhir-type resource))
        :id (:id resource)
        :hash hash}
       (seq refs)
       (assoc :refs refs)
       (seq clauses)
       (assoc :if-none-exist clauses))}))


(defmethod prepare-op :put
  [[op resource matches]]
  (let [hash (hash/generate resource)
        refs (type/references resource)]
    {:hash-resource
     [hash resource]
     :blaze.db/tx-cmd
     (cond->
       {:op (name op)
        :type (name (fhir-spec/fhir-type resource))
        :id (:id resource)
        :hash hash}
       (seq refs)
       (assoc :refs refs)
       matches
       (assoc :if-match matches))}))


(defmethod prepare-op :delete
  [[_ type id]]
  (let [resource (codec/deleted-resource type id)
        hash (hash/generate resource)]
    {:hash-resource
     [hash resource]
     :blaze.db/tx-cmd
     {:op "delete"
      :type (name (fhir-spec/fhir-type resource))
      :id (:id resource)
      :hash hash}}))


(def ^:private split
  (juxt #(mapv :blaze.db/tx-cmd %)
        #(into {} (map :hash-resource) %)))


(defn prepare-ops
  "Returns a tuple of :blaze.db/tx-cmds and a map of hash to resource."
  [tx-ops]
  (split (mapv prepare-op tx-ops)))

(ns blaze.db.spec
  (:require
    [blaze.db.impl.index.resource-handle :as rh]
    [blaze.db.impl.protocols :as p]
    [blaze.db.node.protocols :as np]
    [blaze.db.resource-store.spec]
    [blaze.db.tx-log.spec]
    [clojure.spec.alpha :as s])
  (:import
    [com.github.benmanes.caffeine.cache Cache LoadingCache]))


(s/def :blaze.db/node
  #(satisfies? np/Node %))


(s/def :blaze.db/resource-handle-cache
  #(instance? Cache %))


(s/def :blaze.db/tx-cache
  #(instance? LoadingCache %))


(s/def :blaze.db/resource-cache
  :blaze.db/resource-store)


(s/def :blaze.db/op
  #{:create :put :delete})


(s/def :blaze.db/num-changes
  nat-int?)


(s/def :blaze.db/db
  #(satisfies? p/Db %))


(s/def :blaze.db.tx/instant
  inst?)


(s/def :blaze.db/tx
  (s/keys :req [:blaze.db/t :blaze.db.tx/instant]))


(s/def :blaze.db/resource-handle
  rh/resource-handle?)


(s/def :blaze.db/query
  some?)


(s/def :blaze.db.query/clause
  (s/coll-of string? :min-count 2))


(defmulti tx-op "Transaction operator" first)


(defmethod tx-op :create [_]
  (s/cat :op #{:create}
         :resource :blaze/resource
         :if-none-exist (s/? (s/coll-of :blaze.db.query/clause :min-count 1))))


(defmethod tx-op :put [_]
  (s/cat :op #{:put}
         :resource :blaze/resource
         :matches (s/? :blaze.db/t)))


(defmethod tx-op :delete [_]
  (s/cat :op #{:delete}
         :type :fhir.type/name
         :id :blaze.resource/id))


(s/def :blaze.db/tx-op
  (s/multi-spec tx-op first))


(s/def :blaze.db/tx-ops
  (s/coll-of :blaze.db/tx-op :kind vector? :min-count 1))

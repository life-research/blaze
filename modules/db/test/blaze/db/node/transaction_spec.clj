(ns blaze.db.node.transaction-spec
  (:require
    [blaze.db.node.transaction :as tx]
    [blaze.db.tx-log.spec]
    [blaze.fhir.hash.spec]
    [blaze.fhir.spec.spec]
    [clojure.spec.alpha :as s]))


(s/fdef tx/prepare-ops
  :args (s/cat :tx-ops :blaze.db/tx-ops)
  :ret (s/tuple :blaze.db/tx-cmds (s/map-of :blaze.resource/hash :blaze/resource)))

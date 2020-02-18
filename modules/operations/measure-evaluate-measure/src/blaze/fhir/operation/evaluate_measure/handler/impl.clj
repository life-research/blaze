(ns blaze.fhir.operation.evaluate-measure.handler.impl
  (:require
    [blaze.datomic.util :as db]
    [blaze.executors :refer [executor?]]
    [blaze.fhir.operation.evaluate-measure.measure :refer [evaluate-measure]]
    [blaze.fhir.response.create :as response]
    [blaze.handler.fhir.util :as fhir-util]
    [blaze.handler.util :as handler-util]
    [blaze.terminology-service :refer [term-service?]]
    [clojure.spec.alpha :as s]
    [cognitect.anomalies :as anom]
    [datomic.api :as d]
    [datomic-spec.core :as ds]
    [manifold.deferred :as md]
    [reitit.core :as reitit]
    [ring.util.response :as ring])
  (:import
    [java.time Clock OffsetDateTime]))


(defn- now [clock]
  (OffsetDateTime/now ^Clock clock))


(defn- handle
  [clock transaction-executor conn term-service db executor
   {::reitit/keys [router] :keys [request-method headers]
    {:strs [periodStart periodEnd]} :params}
   measure]
  (let [period [periodStart periodEnd]]
    (-> (md/future-with executor
          (evaluate-measure (now clock) db term-service router period measure))
        (md/chain'
          (fn [result]
            (if (::anom/category result)
              (handler-util/error-response result)
              (cond
                (= :get request-method)
                (ring/response result)

                (= :post request-method)
                (let [id (str (d/squuid))
                      return-preference (handler-util/preference headers "return")]
                  (-> (fhir-util/upsert-resource
                        transaction-executor
                        conn
                        term-service
                        db
                        :server-assigned-id
                        (assoc result "id" id))
                      (md/chain'
                        #(response/build-created-response
                           router return-preference (:db-after %) "MeasureReport" id))
                      (md/catch' handler-util/error-response))))))))))


(defn- find-measure
  [db {{:keys [id]} :path-params {:strs [measure]} :params}]
  (cond
    id
    (db/resource db "Measure" id)

    measure
    (db/resource-by db :Measure/url measure)))


(s/fdef handler
  :args
  (s/cat
    :clock #(instance? Clock %)
    :transaction-executor executor?
    :conn ::ds/conn
    :term-service term-service?
    :executor executor?))

(defn handler [clock transaction-executor conn term-service executor]
  (fn [request]
    (let [db (d/db conn)]
      (if-let [measure (find-measure db request)]
        (if (db/deleted? measure)
          (-> (handler-util/operation-outcome
                {:fhir/issue "deleted"})
              (ring/response)
              (ring/status 410))
          (handle
            clock
            transaction-executor
            conn
            term-service
            db
            executor
            request
            measure))
        (handler-util/error-response
          {::anom/category ::anom/not-found
           :fhir/issue "not-found"})))))

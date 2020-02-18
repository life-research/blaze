(ns blaze.fhir.operation.evaluate-measure.handler.impl-test
  (:require
    [blaze.datomic.test-util :as datomic-test-util]
    [blaze.executors :as ex :refer [executor?]]
    [blaze.fhir.operation.evaluate-measure.handler.impl :refer [handler]]
    [blaze.fhir.operation.evaluate-measure.measure-test :as measure-test]
    [blaze.fhir.response.create :as fhir-response-create]
    [blaze.handler.fhir.util :as fhir-util]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [clojure.test :as test :refer [deftest is testing]]
    [reitit.core :as reitit]
    [taoensso.timbre :as log])
  (:import
    [java.time Clock Instant ZoneOffset OffsetDateTime]))


(defn- fixture [f]
  (st/instrument)
  (st/instrument
    [`handler]
    {:spec
     {`handler
      (s/fspec
        :args
        (s/cat
          :clock #(instance? Clock %)
          :transaction-executor executor?
          :conn #{::conn}
          :term-service #{::term-service}
          :executor executor?))}})
  (datomic-test-util/stub-db ::conn ::db)
  (log/with-merged-config {:level :error} (f))
  (st/unstrument))


(test/use-fixtures :each fixture)


(def base-uri "http://localhost:8080")
(def clock (Clock/fixed Instant/EPOCH (ZoneOffset/ofHours 0)))
(def now (OffsetDateTime/ofInstant Instant/EPOCH (ZoneOffset/ofHours 0)))
(defonce transaction-executor (ex/single-thread-executor))
(defonce executor (ex/single-thread-executor))


(defn stub-upsert-resource
  [conn term-service db creation-mode resource tx-result]
  (st/instrument
    [`fhir-util/upsert-resource]
    {:spec
     {`fhir-util/upsert-resource
      (s/fspec
        :args
        (s/cat
          :transaction-executor #{transaction-executor}
          :conn #{conn}
          :term-service #{term-service}
          :db #{db}
          :creation-mode #{creation-mode}
          :resource #{resource})
        :ret #{tx-result})}
     :stub
     #{`fhir-util/upsert-resource}}))


(defn stub-build-created-response
  [router return-preference-spec db type id response]
  (st/instrument
    [`fhir-response-create/build-created-response]
    {:spec
     {`fhir-response-create/build-created-response
      (s/fspec
        :args (s/cat :router #{router} :return-preference return-preference-spec
                     :db #{db} :type #{type} :id #{id})
        :ret #{response})}
     :stub
     #{`fhir-response-create/build-created-response}}))


(deftest handler-test
  (testing "Returns Not Found on Non-Existing Measure"
    (testing "on type endpoint"
      (datomic-test-util/stub-resource ::db #{"Measure"} #{"0"} nil?)

      (let [{:keys [status body]}
            ((handler clock transaction-executor ::conn ::term-service executor)
             {:path-params {:id "0"}})]

        (is (= 404 status))

        (is (= "OperationOutcome" (:resourceType body)))

        (is (= "error" (-> body :issue first :severity)))

        (is (= "not-found" (-> body :issue first :code)))))

    (testing "on instance endpoint"
      (datomic-test-util/stub-resource-by
        ::db #{:Measure/url} #{"url-181501"} nil?)

      (let [{:keys [status body]}
            ((handler clock transaction-executor ::conn ::term-service executor)
             {:params {"measure" "url-181501"}})]

        (is (= 404 status))

        (is (= "OperationOutcome" (:resourceType body)))

        (is (= "error" (-> body :issue first :severity)))

        (is (= "not-found" (-> body :issue first :code))))))


  (testing "Returns Gone on Deleted Resource"
    (datomic-test-util/stub-resource ::db #{"Measure"} #{"0"} #{::measure})
    (datomic-test-util/stub-deleted? ::measure true?)

    (let [{:keys [status body]}
          ((handler clock transaction-executor ::conn ::term-service executor)
           {:path-params {:id "0"}})]

      (is (= 410 status))

      (is (= "OperationOutcome" (:resourceType body)))

      (is (= "error" (-> body :issue first :severity)))

      (is (= "deleted" (-> body :issue first :code)))))


  (testing "Success"
    (testing "on type endpoint"
      (datomic-test-util/stub-resource-by
        ::db #{:Measure/url} #{"url-181501"} #{::measure})
      (datomic-test-util/stub-deleted? ::measure false?)

      (testing "as GET request"
        (measure-test/stub-evaluate-measure
          now ::db ::term-service ::router "2014" "2015" ::measure ::measure-report)

        (let [{:keys [status body]}
              @((handler
                  clock transaction-executor ::conn ::term-service executor)
                {::reitit/router ::router
                 :request-method :get
                 :params
                 {"measure" "url-181501"
                  "periodStart" "2014"
                  "periodEnd" "2015"}})]

          (is (= 200 status))

          (is (= ::measure-report body))))

      (testing "as POST request"
        (measure-test/stub-evaluate-measure
          now ::db ::term-service ::router "2014" "2015" ::measure {})
        (datomic-test-util/stub-squuid "0")
        (stub-upsert-resource
          ::conn ::term-service ::db :server-assigned-id {"id" "0"}
          {:db-after ::db-after})
        (stub-build-created-response
          ::router nil? ::db-after "MeasureReport" "0" ::response)

        (is (= ::response
               @((handler
                   clock
                   transaction-executor
                   ::conn
                   ::term-service
                   executor)
                 {::reitit/router ::router
                  :request-method :post
                  :params
                  {"measure" "url-181501"
                   "periodStart" "2014"
                   "periodEnd" "2015"}})))))

    (testing "on instance endpoint"
      (datomic-test-util/stub-resource ::db #{"Measure"} #{"0"} #{::measure})
      (datomic-test-util/stub-deleted? ::measure false?)

      (testing "as GET request"
        (measure-test/stub-evaluate-measure
          now ::db ::term-service ::router "2014" "2015" ::measure ::measure-report)

        (let [{:keys [status body]}
              @((handler
                  clock transaction-executor ::conn ::term-service executor)
                {::reitit/router ::router
                 :request-method :get
                 :path-params {:id "0"}
                 :params
                 {"periodStart" "2014"
                  "periodEnd" "2015"}})]

          (is (= 200 status))

          (is (= ::measure-report body))))

      (testing "as POST request"
        (measure-test/stub-evaluate-measure
          now ::db ::term-service ::router "2014" "2015" ::measure {})
        (datomic-test-util/stub-squuid "0")
        (stub-upsert-resource
          ::conn ::term-service ::db :server-assigned-id {"id" "0"}
          {:db-after ::db-after})
        (stub-build-created-response
          ::router nil? ::db-after "MeasureReport" "0" ::response)

        (is (= ::response
               @((handler
                   clock
                   transaction-executor
                   ::conn
                   ::term-service
                   executor)
                 {::reitit/router ::router
                  :request-method :post
                  :path-params {:id "0"}
                  :params
                  {"periodStart" "2014"
                   "periodEnd" "2015"}})))))))

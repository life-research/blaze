(ns blaze.interaction.update-test
  "Specifications relevant for the FHIR update interaction:

  https://www.hl7.org/fhir/http.html#update
  https://www.hl7.org/fhir/operationoutcome.html
  https://www.hl7.org/fhir/http.html#ops"
  (:require
    [blaze.db.api-stub :refer [mem-node-with]]
    [blaze.executors :as ex]
    [blaze.fhir.response.create-spec]
    [blaze.fhir.spec.type]
    [blaze.interaction.update]
    [blaze.interaction.update-spec]
    [blaze.log]
    [clojure.spec.test.alpha :as st]
    [clojure.test :as test :refer [deftest is testing]]
    [integrant.core :as ig]
    [juxt.iota :refer [given]]
    [reitit.core :as reitit]
    [taoensso.timbre :as log])
  (:import
    [java.time Instant]))


(st/instrument)
(log/set-level! :trace)


(defn- fixture [f]
  (st/instrument)
  (f)
  (st/unstrument))


(test/use-fixtures :each fixture)


(def executor (ex/single-thread-executor))


(defn- handler [node]
  (-> (ig/init
        {:blaze.interaction/update
         {:node node
          :executor executor}})
      (:blaze.interaction/update)))


(def ^:private base-url "base-url-134013")


(def ^:private router
  (reitit/router
    [["/Patient" {:name :Patient/type}]]
    {:syntax :bracket}))


(defn- handler-with [txs]
  (fn [request]
    (with-open [node (mem-node-with txs)]
      @((handler node)
        (assoc request
          :blaze/base-url base-url
          ::reitit/router router)))))


(def ^:private operation-outcome
  #fhir/uri"http://terminology.hl7.org/CodeSystem/operation-outcome")


(deftest handler-test
  (testing "on missing body"
    (let [{:keys [status body]}
          ((handler-with [])
            {:path-params {:id "0"}
             ::reitit/match {:data {:fhir.resource/type "Patient"}}})]

      (testing "returns error"
        (is (= 400 status))

        (given body
          :fhir/type := :fhir/OperationOutcome
          [:issue 0 :severity] := #fhir/code"error"
          [:issue 0 :code] := #fhir/code"invalid"
          [:issue 0 :diagnostics] := "Missing HTTP body."))))

  (testing "on type mismatch"
    (let [{:keys [status body]}
          ((handler-with [])
            {:path-params {:id "0"}
             ::reitit/match {:data {:fhir.resource/type "Patient"}}
             :body {:fhir/type :fhir/Observation}})]

      (testing "returns error"
        (is (= 400 status))

        (given body
          :fhir/type := :fhir/OperationOutcome
          [:issue 0 :severity] := #fhir/code"error"
          [:issue 0 :code] := #fhir/code"invariant"
          [:issue 0 :details :coding 0 :system] := operation-outcome
          [:issue 0 :details :coding 0 :code] := #fhir/code"MSG_RESOURCE_TYPE_MISMATCH"
          [:issue 0 :diagnostics] := "Invalid update interaction of a Observation at a Patient endpoint."))))

  (testing "on missing id"
    (let [{:keys [status body]}
          ((handler-with [])
            {:path-params {:id "0"}
             ::reitit/match {:data {:fhir.resource/type "Patient"}}
             :body {:fhir/type :fhir/Patient}})]

      (testing "returns error"
        (is (= 400 status))

        (given body
          :fhir/type := :fhir/OperationOutcome
          [:issue 0 :severity] := #fhir/code"error"
          [:issue 0 :code] := #fhir/code"required"
          [:issue 0 :details :coding 0 :system] := operation-outcome
          [:issue 0 :details :coding 0 :code] := #fhir/code"MSG_RESOURCE_ID_MISSING"
          [:issue 0 :diagnostics] := "Missing resource id."))))


  (testing "on ID mismatch"
    (let [{:keys [status body]}
          ((handler-with [])
            {:path-params {:id "0"}
             ::reitit/match {:data {:fhir.resource/type "Patient"}}
             :body {:fhir/type :fhir/Patient :id "1"}})]

      (testing "returns error"
        (is (= 400 status))

        (given body
          :fhir/type := :fhir/OperationOutcome
          [:issue 0 :severity] := #fhir/code"error"
          [:issue 0 :code] := #fhir/code"invariant"
          [:issue 0 :details :coding 0 :system] := operation-outcome
          [:issue 0 :details :coding 0 :code] := #fhir/code"MSG_RESOURCE_ID_MISMATCH"
          [:issue 0 :diagnostics] := "The resource id `1` doesn't match the endpoints id `0`."))))

  (testing "on optimistic locking failure"
    (let [{:keys [status body]}
          ((handler-with [[[:create {:fhir/type :fhir/Patient :id "0"}]]
                          [[:put {:fhir/type :fhir/Patient :id "0"}]]])
           {:path-params {:id "0"}
            ::reitit/match {:data {:fhir.resource/type "Patient"}}
            :headers {"if-match" "W/\"1\""}
            :body {:fhir/type :fhir/Patient :id "0"}})]

      (testing "returns error"
        (is (= 412 status))

        (given body
          :fhir/type := :fhir/OperationOutcome
          [:issue 0 :severity] := #fhir/code"error"
          [:issue 0 :code] := #fhir/code"conflict"
          [:issue 0 :diagnostics] := "Precondition `W/\"1\"` failed on `Patient/0`."))))

  (testing "on violated referential integrity"
    (let [{:keys [status body]}
          ((handler-with [])
            {:path-params {:id "0"}
             ::reitit/match {:data {:fhir.resource/type "Observation"}}
             :body {:fhir/type :fhir/Observation :id "0"
                    :subject #fhir/Reference{:reference "Patient/0"}}})]

      (testing "returns error"
        (is (= 409 status))

        (given body
          :fhir/type := :fhir/OperationOutcome
          [:issue 0 :severity] := #fhir/code"error"
          [:issue 0 :code] := #fhir/code"conflict"
          [:issue 0 :diagnostics] := "Referential integrity violated. Resource `Patient/0` doesn't exist."))))

  (testing "on newly created resource"
    (testing "with no Prefer header"
      (let [{:keys [status headers body]}
            ((handler-with [])
              {:path-params {:id "0"}
               ::reitit/match {:data {:fhir.resource/type "Patient"}}
               :body {:fhir/type :fhir/Patient :id "0"}})]

        (testing "Returns 201"
          (is (= 201 status)))

        (testing "Location header"
          (is (= "base-url-134013/Patient/0/_history/1" (get headers "Location"))))

        (testing "Transaction time in Last-Modified header"
          (is (= "Thu, 1 Jan 1970 00:00:00 GMT" (get headers "Last-Modified"))))

        (testing "VersionId in ETag header"
          (is (= "W/\"1\"" (get headers "ETag"))))

        (testing "Location header"
          (is (= "base-url-134013/Patient/0/_history/1" (get headers "Location"))))

        (testing "Contains the resource as body"
          (given body
            :fhir/type := :fhir/Patient
            :id := "0"
            [:meta :versionId] := #fhir/id"1"
            [:meta :lastUpdated] := Instant/EPOCH))))

    (testing "with return=minimal Prefer header"
      (let [{:keys [status headers body]}
            ((handler-with [])
              {:path-params {:id "0"}
               ::reitit/match {:data {:fhir.resource/type "Patient"}}
               :headers {"prefer" "return=minimal"}
               :body {:fhir/type :fhir/Patient :id "0"}})]

        (testing "Returns 201"
          (is (= 201 status)))

        (testing "Location header"
          (is (= "base-url-134013/Patient/0/_history/1" (get headers "Location"))))

        (testing "Transaction time in Last-Modified header"
          (is (= "Thu, 1 Jan 1970 00:00:00 GMT" (get headers "Last-Modified"))))

        (testing "VersionId in ETag header"
          (is (= "W/\"1\"" (get headers "ETag"))))

        (testing "Location header"
          (is (= "base-url-134013/Patient/0/_history/1" (get headers "Location"))))

        (testing "Contains no body"
          (is (nil? body)))))

    (testing "with return=representation Prefer header"
      (let [{:keys [status headers body]}
            ((handler-with [])
              {:path-params {:id "0"}
               ::reitit/match {:data {:fhir.resource/type "Patient"}}
               :headers {"prefer" "return=representation"}
               :body {:fhir/type :fhir/Patient :id "0"}})]

        (testing "Returns 201"
          (is (= 201 status)))

        (testing "Location header"
          (is (= "base-url-134013/Patient/0/_history/1" (get headers "Location"))))

        (testing "Transaction time in Last-Modified header"
          (is (= "Thu, 1 Jan 1970 00:00:00 GMT" (get headers "Last-Modified"))))

        (testing "VersionId in ETag header"
          (is (= "W/\"1\"" (get headers "ETag"))))

        (testing "Location header"
          (is (= "base-url-134013/Patient/0/_history/1" (get headers "Location"))))

        (testing "Contains body"
          (given body
            :fhir/type := :fhir/Patient
            :id := "0")))))

  (testing "on recreated, previously deleted resource"
    (testing "with no Prefer header"
      (let [{:keys [status headers body]}
            ((handler-with [[[:create {:fhir/type :fhir/Patient :id "0"}]]
                            [[:delete "Patient" "0"]]])
              {:path-params {:id "0"}
               ::reitit/match {:data {:fhir.resource/type "Patient"}}
               :body {:fhir/type :fhir/Patient :id "0"}})]

        (testing "Returns 201"
          (is (= 201 status)))

        (testing "Location header"
          (is (= "base-url-134013/Patient/0/_history/3" (get headers "Location"))))

        (testing "Transaction time in Last-Modified header"
          (is (= "Thu, 1 Jan 1970 00:00:00 GMT" (get headers "Last-Modified"))))

        (testing "VersionId in ETag header"
          (is (= "W/\"3\"" (get headers "ETag"))))

        (testing "Location header"
          (is (= "base-url-134013/Patient/0/_history/3" (get headers "Location"))))

        (testing "Contains the resource as body"
          (given body
            :fhir/type := :fhir/Patient
            :id := "0"
            [:meta :versionId] := #fhir/id"3"
            [:meta :lastUpdated] := Instant/EPOCH)))))


  (testing "on successful update of an existing resource"
    (testing "with no Prefer header"
      (let [{:keys [status headers body]}
            ((handler-with
                [[[:create {:fhir/type :fhir/Patient :id "0"}]]])
              {:path-params {:id "0"}
               ::reitit/match {:data {:fhir.resource/type "Patient"}}
               :body {:fhir/type :fhir/Patient :id "0"
                      :birthDate #fhir/date"2020"}})]

        (testing "Returns 200"
          (is (= 200 status)))

        (testing "Transaction time in Last-Modified header"
          (is (= "Thu, 1 Jan 1970 00:00:00 GMT" (get headers "Last-Modified"))))

        (testing "VersionId in ETag header"
          (is (= "W/\"2\"" (get headers "ETag"))))

        (testing "Contains the resource as body"
          (given body
            :fhir/type := :fhir/Patient
            :id := "0"
            :birthDate := #fhir/date"2020"
            [:meta :versionId] := #fhir/id"2"
            [:meta :lastUpdated] := Instant/EPOCH))))))

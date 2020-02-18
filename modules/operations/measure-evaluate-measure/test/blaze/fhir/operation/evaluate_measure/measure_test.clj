(ns blaze.fhir.operation.evaluate-measure.measure-test
  (:require
    [blaze.bundle :as bundle]
    [blaze.cql-translator :as cql-translator]
    [blaze.datomic.test-util :as datomic-test-util]
    [blaze.datomic.value :as value]
    [blaze.elm.compiler :as compiler]
    [blaze.fhir.operation.evaluate-measure.cql :as cql]
    [blaze.fhir.operation.evaluate-measure.measure :refer [evaluate-measure]]
    [blaze.handler.fhir.util :as fhir-util]
    [blaze.terminology-service :as ts]
    [cheshire.core :as json]
    [cheshire.parse :refer [*use-bigdecimals?*]]
    [clojure.spec.alpha :as s]
    [clojure.spec.test.alpha :as st]
    [clojure.test :as test :refer [are deftest is testing]]
    [cognitect.anomalies :as anom]
    [datomic.api :as d]
    [manifold.deferred :as md]
    [juxt.iota :refer [given]]
    [taoensso.timbre :as log])
  (:import
    [java.time Instant OffsetDateTime ZoneOffset Year]
    [java.util Base64]))


(defn fixture [f]
  (st/instrument)
  (log/with-merged-config {:level :error} (f))
  (st/unstrument))


(test/use-fixtures :each fixture)


(defn stub-instance-url [router type id url]
  (st/instrument
    [`fhir-util/instance-url]
    {:spec
     {`fhir-util/instance-url
      (s/fspec
        :args (s/cat :router #{router} :type #{type} :id #{id})
        :ret #{url})}
     :stub
     #{`fhir-util/instance-url}}))


(defn stub-cql-translate [code library]
  (st/instrument
    [`cql-translator/translate]
    {:spec
     {`cql-translator/translate
      (s/fspec
        :args (s/cat :cql #{code} :opts (s/* some?))
        :ret #{library})}
     :stub
     #{`cql-translator/translate}}))


(defn stub-compile-library [db term-service library compiled-library]
  (st/instrument
    [`compiler/compile-library]
    {:spec
     {`compiler/compile-library
      (s/fspec
        :args (s/cat :db #{db} :term-service #{term-service}
                     :library #{library} :opts #{{}})
        :ret #{compiled-library})}
     :stub
     #{`compiler/compile-library}}))


(defn stub-evaluate-expression [db now library subject expression-name result]
  (st/instrument
    [`cql/evaluate-expression]
    {:spec
     {`cql/evaluate-expression
      (s/fspec
        :args (s/cat :db #{db} :now #{now} :library #{library}
                     :subject #{subject}
                     :expression-name #{expression-name})
        :ret #{result})}
     :stub
     #{`cql/evaluate-expression}}))


(defn stub-calc-stratums
  [db now library subject population-expression-name expression-name result]
  (st/instrument
    [`cql/calc-stratums]
    {:spec
     {`cql/calc-stratums
      (s/fspec
        :args (s/cat :db #{db} :now #{now} :library #{library}
                     :subject #{subject}
                     :population-expression-name #{population-expression-name}
                     :expression-name #{expression-name})
        :ret #{result})}
     :stub
     #{`cql/calc-stratums}}))


(defn stub-calc-mult-component-stratums
  [db now library subject population-expression-name expression-names result]
  (st/instrument
    [`cql/calc-mult-component-stratums]
    {:spec
     {`cql/calc-mult-component-stratums
      (s/fspec
        :args (s/cat :db #{db} :now #{now} :library #{library}
                     :subject #{subject}
                     :population-expression-name #{population-expression-name}
                     :expression-names #{expression-names})
        :ret #{result})}
     :stub
     #{`cql/calc-mult-component-stratums}}))


(def now (OffsetDateTime/ofInstant Instant/EPOCH (ZoneOffset/ofHours 0)))


(defn- scoring-concept [code]
  {:CodeableConcept/coding
   [{:Coding/code
     {:code/system "http://terminology.hl7.org/CodeSystem/measure-scoring"
      :code/code code}}]})


(defn- population-concept [code]
  {:CodeableConcept/coding
   [{:Coding/code
     {:code/system "http://terminology.hl7.org/CodeSystem/measure-population"
      :code/code code}}]})


(defn- attachment [content-type data]
  {:Attachment/contentType content-type
   :Attachment/data (value/write (.getBytes data "utf-8"))})


(defn- cql-expression [expr]
  {:Expression/language {:code/code "text/cql"}
   :Expression/expression expr})


(defn- fhirpath-expression [expr]
  {:Expression/language {:code/code "text/fhirpath"}
   :Expression/expression expr})


(deftest evaluate-measure-test
  (st/unstrument `evaluate-measure)

  (testing "Missing primary library"
    (given (evaluate-measure now ::db ::term-service ::router [::start ::end] {})
      ::anom/category := ::anom/unsupported
      ::anom/message := "Missing primary library. Currently only CQL expressions together with one primary library are supported."
      :fhir/issue := "not-supported"
      :fhir.issue/expression := "Measure.library"))

  (testing "Unsupported criteria language"
    (let [population-concept (population-concept "initial-population")
          population
          #:Measure.group.population
              {:code population-concept
               :criteria (fhirpath-expression "Patient.gender")}
          group
          #:Measure.group{:population [population]}
          measure
          #:Measure
              {:library ["http://foo"]
               :group [group]}
          library
          #:Library
              {:url "http://foo"
               :content [(attachment "text/cql" "library-data")]}]

      (datomic-test-util/stub-resource-by
        ::db #{:Library/url} #{"http://foo"} #{library})
      (stub-cql-translate "library-data" ::elm-library)
      (stub-compile-library ::db ::term-service ::elm-library ::compiled-library)

      (given (evaluate-measure now ::db ::term-service ::router [::start ::end] measure)
        ::anom/category := ::anom/unsupported
        ::anom/message := "Unsupported language `text/fhirpath`."
        :fhir/issue := "not-supported"
        :fhir.issue/expression := "Measure.group[0].population[0].criteria.language")))

  (testing "Missing criteria expression"
    (let [population-concept (population-concept "initial-population")
          population
          #:Measure.group.population
              {:code population-concept
               :criteria (cql-expression nil)}
          group
          #:Measure.group{:population [population]}
          measure
          #:Measure
              {:library ["http://foo"]
               :group [group]}
          library
          #:Library
              {:url "http://foo"
               :content [(attachment "text/cql" "library-data")]}]

      (datomic-test-util/stub-resource-by
        ::db #{:Library/url} #{"http://foo"} #{library})
      (stub-cql-translate "library-data" ::elm-library)
      (stub-compile-library ::db ::term-service ::elm-library ::compiled-library)

      (given (evaluate-measure now ::db ::term-service ::router [::start ::end] measure)
        ::anom/category := ::anom/incorrect
        ::anom/message := "Missing expression."
        :fhir/issue := "required"
        :fhir.issue/expression := "Measure.group[0].population[0].criteria.expression")))


  (testing "Cohort scoring"
    (let [population-concept (population-concept "initial-population")
          population
          #:Measure.group.population
              {:code population-concept
               :criteria (cql-expression "InInitialPopulation")}
          group
          #:Measure.group{:population [population]}
          measure
          #:Measure
              {:id "0"
               :scoring (scoring-concept "cohort")
               :library ["http://foo"]
               :group [group]}
          library
          #:Library
              {:url "http://foo"
               :content [(attachment "text/cql" "library-data")]}]

      (stub-instance-url ::router "Measure" "0" ::measure-url)
      (datomic-test-util/stub-resource-by
        ::db #{:Library/url} #{"http://foo"} #{library})
      (stub-cql-translate "library-data" ::elm-library)
      (stub-compile-library ::db ::term-service ::elm-library ::compiled-library)
      (stub-evaluate-expression
        ::db now ::compiled-library "Patient" "InInitialPopulation" 1)
      (datomic-test-util/stub-pull-non-primitive
        ::db :CodeableConcept population-concept ::population-concept)

      (let [measure-report (evaluate-measure now ::db ::term-service ::router [::start ::end] measure)]

        (is (= "MeasureReport" (get measure-report "resourceType")))

        (is (= "complete" (get measure-report "status")))

        (is (= "summary" (get measure-report "type")))

        (is (= ::measure-url (get measure-report "measure")))

        (is (= "1970-01-01T00:00:00Z" (get measure-report "date")))

        (is (= ::start (-> measure-report (get "period") (get "start"))))

        (is (= ::end (-> measure-report (get "period") (get "end"))))

        (is (= ::population-concept
               (-> measure-report
                   (get "group") first
                   (get "population") first
                   (get "code"))))

        (is (= 1
               (-> measure-report
                   (get "group") first
                   (get "population") first
                   (get "count")))))))

  (testing "Cohort scoring with stratifiers"
    (let [population-concept (population-concept "initial-population")
          population
          #:Measure.group.population
              {:code population-concept
               :criteria (cql-expression "InInitialPopulation")}
          stratifier-concept {:CodeableConcept/text "gender"}
          stratifier
          #:Measure.group.stratifier
              {:code stratifier-concept
               :criteria (cql-expression "Gender")}
          group
          #:Measure.group
              {:population [population]
               :stratifier [stratifier]}
          measure
          #:Measure
              {:id "0"
               :scoring (scoring-concept "cohort")
               :library ["http://foo"]
               :group [group]}
          library
          #:Library
              {:url "http://foo"
               :content [(attachment "text/cql" "library-data")]}]

      (stub-instance-url ::router "Measure" "0" ::measure-url)
      (datomic-test-util/stub-resource-by
        ::db #{:Library/url} #{"http://foo"} #{library})
      (stub-cql-translate "library-data" ::elm-library)
      (stub-compile-library ::db ::term-service ::elm-library ::compiled-library)
      (stub-evaluate-expression
        ::db now ::compiled-library "Patient" "InInitialPopulation" 1)
      (stub-calc-stratums
        ::db now ::compiled-library "Patient" "InInitialPopulation" "Gender"
        {"male" 1})
      (datomic-test-util/stub-pull-non-primitive-fn
        ::db :CodeableConcept #{population-concept stratifier-concept}
        (fn [_ _ value]
          (condp = value
            population-concept ::population-concept
            stratifier-concept ::stratifier-concept)))

      (let [measure-report (evaluate-measure now ::db ::term-service ::router [::start ::end] measure)]
        (is (= "MeasureReport" (get measure-report "resourceType")))

        (is (= ::stratifier-concept
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "code") first)))

        (is (= "male"
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "stratum") first
                   (get "value")
                   (get "text"))))

        (is (= ::population-concept
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "stratum") first
                   (get "population") first
                   (get "code"))))

        (is (= 1
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "stratum") first
                   (get "population") first
                   (get "count")))))))

  (testing "Cohort scoring with stratifiers with multiple components"
    (let [population-concept (population-concept "initial-population")
          population
          #:Measure.group.population
              {:code population-concept
               :criteria (cql-expression "InInitialPopulation")}
          stratifier-concept-gender {:CodeableConcept/text "gender"}
          stratifier-concept-age {:CodeableConcept/text "age"}
          stratifier-component-gender
          #:Measure.group.stratifier.component
              {:code stratifier-concept-gender
               :criteria (cql-expression "Gender")}
          stratifier-component-age
          #:Measure.group.stratifier.component
              {:code stratifier-concept-age
               :criteria (cql-expression "Age")}
          stratifier
          #:Measure.group.stratifier
              {:component
               [stratifier-component-gender
                stratifier-component-age]}
          group
          #:Measure.group
              {:population [population]
               :stratifier [stratifier]}
          measure
          #:Measure
              {:id "0"
               :scoring (scoring-concept "cohort")
               :library ["http://foo"]
               :group [group]}
          library
          #:Library
              {:url "http://foo"
               :content [(attachment "text/cql" "library-data")]}]

      (stub-instance-url ::router "Measure" "0" ::measure-url)
      (datomic-test-util/stub-resource-by
        ::db #{:Library/url} #{"http://foo"} #{library})
      (stub-cql-translate "library-data" ::elm-library)
      (stub-compile-library ::db ::term-service ::elm-library ::compiled-library)
      (stub-evaluate-expression
        ::db now ::compiled-library "Patient" "InInitialPopulation" 1)
      (stub-calc-mult-component-stratums
        ::db now ::compiled-library "Patient" "InInitialPopulation"
        ["Gender" "Age"] {["male" 30] 1})
      (datomic-test-util/stub-pull-non-primitive-fn
        ::db :CodeableConcept
        #{population-concept stratifier-concept-gender stratifier-concept-age}
        (fn [_ _ value]
          (condp = value
            population-concept ::population-concept
            stratifier-concept-gender ::stratifier-concept-gender
            stratifier-concept-age ::stratifier-concept-age)))

      (let [measure-report (evaluate-measure now ::db ::term-service ::router [::start ::end] measure)]
        (is (= "MeasureReport" (get measure-report "resourceType")))

        (is (= [::stratifier-concept-gender ::stratifier-concept-age]
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "code"))))

        (is (= ::stratifier-concept-gender
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "stratum") first
                   (get "component") first
                   (get "code"))))

        (is (= "male"
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "stratum") first
                   (get "component") first
                   (get "value")
                   (get "text"))))

        (is (= ::stratifier-concept-age
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "stratum") first
                   (get "component") second
                   (get "code"))))

        (is (= "30"
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "stratum") first
                   (get "component") second
                   (get "value")
                   (get "text"))))

        (is (= ::population-concept
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "stratum") first
                   (get "population") first
                   (get "code"))))

        (is (= 1
               (-> measure-report
                   (get "group") first
                   (get "stratifier") first
                   (get "stratum") first
                   (get "population") first
                   (get "count"))))))))


(defn stub-evaluate-measure
  [now db term-service router period-start period-end measure measure-report]
  (st/instrument
    [`evaluate-measure]
    {:spec
     {`evaluate-measure
      (s/fspec
        :args (s/cat :now #{now} :db #{db} :term-service #{term-service}
                     :router #{router}
                     :period (s/tuple #{period-start} #{period-end})
                     :measure #{measure})
        :ret #{measure-report})}
     :stub
     #{`evaluate-measure}}))


(defonce db (d/db (st/with-instrument-disabled (datomic-test-util/connect))))


(def term-service
  (reify ts/TermService
    (-expand-value-set [_ {:keys [url]}]
      (case url
        "http://hl7.org/fhir/ValueSet/administrative-gender"
        (md/success-deferred
          {:expansion
           {:contains
            [{:system "http://hl7.org/fhir/administrative-gender"
              :code "male"}
             {:system "http://hl7.org/fhir/administrative-gender"
              :code "female"}]}})
        "http://hl7.org/fhir/ValueSet/publication-status"
        (md/success-deferred
          {:expansion
           {:contains
            [{:system "http://hl7.org/fhir/publication-status"
              :code "draft"
              :display "Draft"}
             {:system "http://hl7.org/fhir/publication-status"
              :code "active"
              :display "Active"}
             {:system "http://hl7.org/fhir/publication-status"
              :code "retired"
              :display "Retired"}
             {:system "http://hl7.org/fhir/publication-status"
              :code "unknown"
              :display "Unknown"}]}})
        "http://hl7.org/fhir/ValueSet/expression-language"
        (md/success-deferred
          {:expansion
           {:contains
            [{:system "http://hl7.org/fhir/expression-language"
              :code "text/cql"
              :display "CQL"}
             {:system "http://hl7.org/fhir/expression-language"
              :code "text/fhirpath"
              :display "FHIRPath"}
             {:system "http://hl7.org/fhir/expression-language"
              :code "application/x-fhir-query"
              :display "FHIR Query"}]}})
        "http://hl7.org/fhir/ValueSet/observation-status"
        (md/success-deferred
          {:expansion
           {:contains
            [{:system "http://hl7.org/fhir/observation-status"
              :code "registered"
              :display "Registered"}
             {:system "http://hl7.org/fhir/observation-status"
              :code "preliminary"
              :display "Preliminary"}
             {:system "http://hl7.org/fhir/observation-status"
              :code "final"
              :display "Final"}
             {:system "http://hl7.org/fhir/observation-status"
              :code "amended"
              :display "Amended"}
             {:system "http://hl7.org/fhir/observation-status"
              :code "corrected"
              :display "Corrected"}
             {:system "http://hl7.org/fhir/observation-status"
              :code "cancelled"
              :display "Cancelled"}
             {:system "http://hl7.org/fhir/observation-status"
              :code "entered-in-error"
              :display "Entered in Error"}
             {:system "http://hl7.org/fhir/observation-status"
              :code "unknown"
              :display "Unknown"}]}})
        "https://fhir.bbmri.de/ValueSet/icd-10-gm-c25"
        (md/success-deferred
          {:expansion
           {:contains
            [{:system "http://fhir.de/CodeSystem/dimdi/icd-10-gm"
              :code "C25.0"
              :version "2020"}
             {:system "http://fhir.de/CodeSystem/dimdi/icd-10-gm"
              :code "C25.1"
              :version "2020"}
             {:system "http://fhir.de/CodeSystem/dimdi/icd-10-gm"
              :code "C25.2"
              :version "2020"}
             {:system "http://fhir.de/CodeSystem/dimdi/icd-10-gm"
              :code "C25.3"
              :version "2020"}
             {:system "http://fhir.de/CodeSystem/dimdi/icd-10-gm"
              :code "C25.4"
              :version "2020"}
             {:system "http://fhir.de/CodeSystem/dimdi/icd-10-gm"
              :code "C25.7"
              :version "2020"}
             {:system "http://fhir.de/CodeSystem/dimdi/icd-10-gm"
              :code "C25.8"
              :version "2020"}
             {:system "http://fhir.de/CodeSystem/dimdi/icd-10-gm"
              :code "C25.9"
              :version "2020"}]}})))))


(defn- db-with [{:strs [entry]}]
  (let [entries @(bundle/annotate-codes term-service db entry)
        {db :db-after} (d/with db (bundle/code-tx-data db entries))]
    (:db-after (d/with db (bundle/tx-data db entries)))))


(defn- slurp-resource [name]
  (let [cl (.getContextClassLoader (Thread/currentThread))
        name (str "blaze/fhir/operation/evaluate_measure/" name)
        res (.getResource cl name)]
    (slurp res)))


(defn- b64-encode [s]
  (.encodeToString (Base64/getEncoder) (.getBytes s)))


(defn- library [query]
  {"resource"
   {"resourceType" "Library"
    "id" "0"
    "url" "0"
    "content"
    [{"contentType" "text/cql"
      "data" (b64-encode query)}]}
   "request"
   {"method" "PUT"
    "url" "Library/0"}})


(defn- read-data [name]
  (let [raw (slurp-resource (str name "-data.json"))
        bundle (binding [*use-bigdecimals?* true] (json/parse-string raw))
        library (library (slurp-resource (str name "-query.cql")))]
    (update bundle "entry" conj library)))


(defn- evaluate [name]
  (cql/clear-resource-cache!)
  (let [db (db-with (read-data name))
        period [(Year/of 2000) (Year/of 2020)]]
    (evaluate-measure now db term-service ::router period (d/entity db [:Measure/id "0"]))))


(defn- first-population-count [measure-report]
  (if (::anom/category measure-report)
    (prn measure-report)
    (-> measure-report
        (get "group") first
        (get "population") first
        (get "count"))))


(defn- first-stratifier-stratums [measure-report]
  (if (::anom/category measure-report)
    (prn measure-report)
    (-> measure-report
        (get "group") first
        (get "stratifier") first
        (get "stratum"))))


(deftest integration-test
  (st/unstrument `evaluate-measure)
  (stub-instance-url ::router "Measure" "0" ::measure-url)

  (are [name count] (= count (first-population-count (evaluate name)))
    "q1" 1
    "q2" 1
    "q3" 1
    "q4" 1
    "q5" 1
    "q6" 1
    "q7" 1
    "q8" 1
    "q9" 1
    "q10" 1
    "q11" 1
    "q12" 1
    "q13" 1
    "q14" 1
    "q15" 1
    "q16" 1
    "q17" 2
    "q18" 1
    "q24" 1
    "q27" 1)

  (given (first-stratifier-stratums (evaluate "q19-stratifier-ageclass"))
    [0 "value" "text"] := "10"
    [0 "population" 0 "count"] := 1
    [1 "value" "text"] := "70"
    [1 "population" 0 "count"] := 2)

  (given (first-stratifier-stratums (evaluate "q20-stratifier-city"))
    [0 "value" "text"] := "Jena"
    [0 "population" 0 "count"] := 3
    [1 "value" "text"] := "Leipzig"
    [1 "population" 0 "count"] := 1)

  (given (first-stratifier-stratums (evaluate "q21-stratifier-city-of-only-women"))
    [0 "value" "text"] := "Jena"
    [0 "population" 0 "count"] := 2)

  (is (= ::anom/incorrect (::anom/category (evaluate "q22-stratifier-multiple-cities-fail"))))

  (given
    (->> (first-stratifier-stratums (evaluate "q23-stratifier-ageclass-and-gender"))
         (map (fn [c] (update c "component" (fn [x] (sort-by #(get-in % ["code" "text"]) x)))))
         (sort-by (fn [{:strs [component]}] (mapv #(get-in % ["value" "text"]) component))))
    [0 "component" 0 "value" "text"] := "10"
    [0 "component" 1 "value" "text"] := "male"
    [0 "population" 0 "count"] := 1
    [1 "component" 0 "value" "text"] := "70"
    [1 "component" 1 "value" "text"] := "female"
    [1 "population" 0 "count"] := 2
    [2 "component" 0 "value" "text"] := "70"
    [2 "component" 1 "value" "text"] := "male"
    [2 "population" 0 "count"] := 1)

  (given (first-stratifier-stratums (evaluate "q25-stratifier-collection"))
    [0 "value" "text"] := "Organization/collection-0"
    [0 "population" 0 "count"] := 1
    [1 "value" "text"] := "Organization/collection-1"
    [1 "population" 0 "count"] := 1)

  (given (first-stratifier-stratums (evaluate "q26-stratifier-bmi"))
    [0 "value" "text"] := ""
    [0 "population" 0 "count"] := 2
    [1 "value" "text"] := "37"
    [1 "population" 0 "count"] := 1))

(comment
  (require 'hashp.core)
  (evaluate "q26-stratifier-bmi")
  (clojure.repl/pst)
  )

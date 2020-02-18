(ns blaze.elm.compiler.property-test
  (:require
    [blaze.datomic.quantity :as datomic-quantity]
    [blaze.datomic.test-util :as datomic-test-util]
    [blaze.elm.compiler.property
     :refer [attr scope-expr scope-runtime-type-expr]]
    [blaze.elm.compiler.protocols :refer [-eval]]
    [clojure.spec.test.alpha :as st]
    [clojure.test :as test :refer [are deftest is]]))


(defn fixture [f]
  (st/instrument)
  (f)
  (st/unstrument))


(test/use-fixtures :each fixture)


(deftest attr-test
  (are [elm kw] (= kw (attr elm))
    {:resultTypeName "{http://hl7.org/fhir}Specimen.Collection"
     :path "collection"
     :scope "S"
     :type "Property"
     :life/scopes #{"S"}
     :life/source-type "{http://hl7.org/fhir}Specimen"}
    :Specimen/collection

    {:path "collected"
     :type "Property"
     :resultTypeSpecifier
     {:type "ChoiceTypeSpecifier"
      :choice [{:name "{http://hl7.org/fhir}dateTime"
                :type "NamedTypeSpecifier"}
               {:name "{http://hl7.org/fhir}Period"
                :type "NamedTypeSpecifier"}]}
     :source
     {:resultTypeName "{http://hl7.org/fhir}Specimen.Collection"
      :path "collection"
      :scope "S"
      :type "Property"
      :life/scopes #{"S"}
      :life/source-type "{http://hl7.org/fhir}Specimen"}}
    :Specimen.collection/collected

    {:resultTypeName "{http://hl7.org/fhir}decimal"
     :path "value"
     :type "Property"
     :source
     {:resultTypeName "{http://hl7.org/fhir}Quantity"
      :type "Quantity" :value 1}}
    datomic-quantity/value

    {:resultTypeName "{http://hl7.org/fhir}decimal"
     :path "unit"
     :type "Property"
     :source
     {:resultTypeName "{http://hl7.org/fhir}Quantity"
      :type "Quantity" :value 1}}
    datomic-quantity/unit

    {:resultTypeName "{http://hl7.org/fhir}decimal"
     :path "system"
     :type "Property"
     :source
     {:resultTypeName "{http://hl7.org/fhir}Quantity"
      :type "Quantity" :value 1}}
    datomic-quantity/system

    {:resultTypeName "{http://hl7.org/fhir}decimal"
     :path "code"
     :type "Property"
     :source
     {:resultTypeName "{http://hl7.org/fhir}Quantity"
      :type "Quantity" :value 1}}
    datomic-quantity/code))


(deftest scope-expr-test
  (is
    (=
      (-eval
        (scope-expr "C" :CodeableConcept/coding)
        nil
        nil
        {"C" {:CodeableConcept/coding "foo"}})
      "foo")))


(deftest scope-runtime-type-expr-test
  (let [entity {:CodeableConcept/coding "foo"}]
    (datomic-test-util/stub-entity-type entity "CodeableConcept")
    (is
      (=
        (-eval
          (scope-runtime-type-expr "C" "coding")
          nil
          nil
          {"C" entity})
        "foo"))))

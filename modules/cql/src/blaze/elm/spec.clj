(ns blaze.elm.spec
  (:require
    [blaze.elm.quantity :refer [format-unit]]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.spec.gen.alpha :as gen]
    [cuerdas.core :as str])
  (:import
    [java.time LocalDate]
    [javax.measure.spi ServiceProvider SystemOfUnits]
    [tec.units.indriya.unit TransformedUnit]))


(def temporal-keywords
  "CQL temporal keywords as units for temporal quantities."
  (->> ["year" "month" "week" "day" "hour" "minute" "second" "millisecond"]
       (into #{} (mapcat #(vector % (str % "s"))))))


(def ^SystemOfUnits ucum-service
  (.getSystemOfUnits (.getSystemOfUnitsService (ServiceProvider/current)) "UCUM"))


(def defined-units
  "All defined units from ucum-service."
  (into #{} (comp (remove (comp #{TransformedUnit} class))
                  (map format-unit))
        (.getUnits ucum-service)))


(s/def :elm/alias
  string?)


(s/def :elm/accessLevel
  string?)


;; Execution context of an expression.
;;
;; Is used by the environment to determine whether or not to filter the data
;; returned from retrieves based on the current context.
;;
;; Examples: Patient, Practitioner or Unfiltered
(s/def :elm/expression-execution-context
  string?)


(s/def :elm/libraryName
  string?)


(s/def :elm/name
  string?)


(s/def :elm/id
  string?)


(s/def :elm/version
  string?)


(s/def :elm/path
  string?)


(s/def :elm/direction
  #{"asc" "ascending" "desc" "descending"})


(s/def :elm/scope
  string?)


(s/def :elm/type
  string?)


(s/def :elm/value
  string?)


(s/def :elm/valueType
  string?)


(s/def :elm/signature
  (s/coll-of :elm/type-specifier))


(defn expression-dispatch [{:keys [type]}]
  (when type
    (keyword "elm.spec.type" (str/kebab type))))


(defmulti expression expression-dispatch)


(s/def :elm/expression
  (s/multi-spec expression :type))


(s/def :elm/source
  :elm/expression)


(s/def :elm/startIndex
  :elm/expression)


(s/def :elm/endIndex
  :elm/expression)


(s/def :elm/condition
  :elm/expression)


(s/def :elm/element
  :elm/expression)


(s/def :elm/separator
  :elm/expression)


(s/def :elm/pattern
  :elm/expression)


(s/def :elm/string
  :elm/expression)


(s/def :elm/stringToSplit
  :elm/expression)


(s/def :elm/separatorPattern
  :elm/expression)


(s/def :elm/length
  :elm/expression)


(defmulti sort-by-item :type)


(s/def :elm/sort-by-item
  (s/multi-spec sort-by-item :type))


(s/def :elm.sort/by
  (s/coll-of :elm/sort-by-item :min-count 1))


(s/def :elm/orderBy
  :elm/sort-by-item)



;; 1. Simple Values

;; 1.1. Literal
(defmethod expression :elm.spec.type/literal [_]
  (s/keys :req-un [:elm/valueType] :opt-un [:elm/value]))


(s/def :elm.literal/type
  #{"Literal"})


(s/def :elm.integer/valueType
  #{"{urn:hl7-org:elm-types:r1}Integer"})


(s/def :elm.integer/value
  (s/with-gen string? #(gen/fmap str (gen/int))))


(s/def :elm/integer
  (s/keys :req-un [:elm.literal/type :elm.integer/valueType :elm.integer/value]))


(s/def :elm.decimal/valueType
  #{"{urn:hl7-org:elm-types:r1}Decimal"})

(defn decimal-value []
  (->> (gen/large-integer)
       (gen/fmap #(BigDecimal/valueOf ^long % 8))))


(s/def :elm.decimal/value
  (s/with-gen string? #(gen/fmap str (decimal-value))))


(s/def :elm/decimal
  (s/keys :req-un [:elm.literal/type :elm.decimal/valueType :elm.decimal/value]))


(defn non-zero-decimal-value []
  (gen/fmap str (gen/such-that (complement zero?) (decimal-value))))


(s/def :elm/non-zero-decimal
  (s/with-gen :elm/decimal #(s/gen :elm/decimal {:elm.decimal/value non-zero-decimal-value})))


;; 2. Structured Values

;; 2.1. Tuple
(s/def :elm.tuple/name
  string?)


(s/def :elm.tuple/value
  :elm/expression)


(s/def :elm/tuple-element
  (s/keys :req-un [:elm.tuple/name :elm.tuple/value]))


(s/def :elm.tuple/element
  (s/coll-of :elm/tuple-element))


(defmethod expression :elm.spec.type/tuple [_]
  (s/keys :opt-un [:elm.tuple/element]))


;; 2.3. Property
(defmethod expression :elm.spec.type/property [_]
  (s/keys :req-un [:elm/path] :opt-un [:elm/source :elm/scope]))



;; 3. Clinical Values

(s/def :elm/code-system-ref
  (s/keys :opt-un [:elm/name :elm/libraryName]))


;; 3.1. Code
(s/def :elm.code/system
  :elm/code-system-ref)


(s/def :elm.code/code
  string?)


(s/def :elm.code/display
  string?)


(defmethod expression :elm.spec.type/code [_]
  (s/keys :req-un [:elm.code/system :elm.code/code]
          :opt-un [:elm.code/display]))


;; 3.2. CodeDef
(s/def :elm.code-def/codeSystem
  :elm/code-system-ref)


(s/def :elm/code-def
  (s/keys :req-un [:elm/name :elm/id]
          :opt-un [:elm.code-def/codeSystem]))


;; 3.3. CodeRef
(defmethod expression :elm.spec.type/code-ref [_]
  (s/keys :opt-un [:elm/name :elm/libraryName]))


;; 3.4. CodeSystemDef
(s/def :elm/code-system-def
  (s/keys :req-un [:elm/name :elm/id]
          :opt-un [:elm/version]))


;; 3.5. CodeSystemRef
(defmethod expression :elm.spec.type/code-system-ref [_]
  :elm/code-system-ref)


;; 3.9. Quantity
(s/def :elm.quantity/type
  #{"Quantity"})


(s/def :elm.quantity/value
  (s/with-gen number? decimal-value))


(s/def :elm.quantity/unit
  (s/with-gen string? #(s/gen (set/union temporal-keywords defined-units))))


(s/def :elm/quantity
  (s/keys :req-un [:elm.quantity/type :elm.quantity/value]
          :opt-un [:elm.quantity/unit]))


(defmethod expression :elm.spec.type/quantity [_]
  :elm/quantity)


(s/def :elm.quantity.temporal-keyword/unit
  temporal-keywords)


(s/def :elm/period
  (s/keys :req-un [:elm.quantity/type :elm.quantity/value
                   :elm.quantity.temporal-keyword/unit]))


(defn years-unit-gen []
  (s/gen #{"year" "years"}))


(s/def :elm/years
  (s/with-gen
    :elm/period
    #(s/gen :elm/period {:elm.quantity.temporal-keyword/unit years-unit-gen})))


(defn pos-decimal-gen []
  (gen/fmap #(BigDecimal/valueOf ^double %)
            (gen/double* {:infinite? false :NaN? false :min 1 :max 10000})))


(s/def :elm/pos-years
  (s/with-gen
    :elm/period
    #(s/gen :elm/period {:elm.quantity/value pos-decimal-gen
                         :elm.quantity.temporal-keyword/unit years-unit-gen})))


(defn months-unit-gen []
  (s/gen #{"month" "months"}))


(s/def :elm/pos-months
  (s/with-gen
    :elm/period
    #(s/gen :elm/period {:elm.quantity/value pos-decimal-gen
                         :elm.quantity.temporal-keyword/unit months-unit-gen})))


(defn days-unit-gen []
  (s/gen #{"day" "days"}))


(s/def :elm/pos-days
  (s/with-gen
    :elm/period
    #(s/gen :elm/period {:elm.quantity/value pos-decimal-gen
                         :elm.quantity.temporal-keyword/unit days-unit-gen})))


(defn hours-unit-gen []
  (s/gen #{"hour" "hours"}))


(s/def :elm/pos-hours
  (s/with-gen
    :elm/period
    #(s/gen :elm/period {:elm.quantity/value pos-decimal-gen
                         :elm.quantity.temporal-keyword/unit hours-unit-gen})))


;; 3.11 ValueSetDef
(s/def :elm/value-set-def
  (s/keys :req-un [:elm/id]
          :opt-un [:elm/name :elm/version]))


;; 3.12 ValueSetRef
(defmethod expression :elm.spec.type/value-set-ref [_]
  (s/keys :opt-un [:elm/name :elm/libraryName]))



;; 4. Type Specifiers

;; 4.1. TypeSpecifier
(defmulti type-specifier :type)


(s/def :elm/type-specifier
  (s/multi-spec type-specifier :type))


(s/def :elm/elementType
  :elm/type-specifier)


;; 4.2. NamedTypeSpecifier
(s/def :elm/named-type-specifier
  (s/keys :req-un [:elm/name]))


(defmethod type-specifier "NamedTypeSpecifier" [_]
  :elm/named-type-specifier)


;; 4.4. ListTypeSpecifier
(s/def :elm/list-type-specifier
  (s/keys :req-un [:elm/elementType]))


(defmethod type-specifier "ListTypeSpecifier" [_]
  :elm/list-type-specifier)


;; 4.5. TupleTypeSpecifier
(s/def :elm.tuple-element-definition/type
  :elm/type-specifier)


(s/def :elm/tuple-element-definition
  (s/keys :req-un [:elm/name] :opt-un [:elm.tuple-element-definition/type :elm/elementType]))


(s/def :elm.tuple-type-specifier/element
  (s/coll-of :elm/tuple-element-definition))


(s/def :elm/tuple-type-specifier
  (s/keys :req-un [:elm.tuple-type-specifier/element]))


(defmethod type-specifier "TupleTypeSpecifier" [_]
  :elm/tuple-type-specifier)


;; 4.6. ChoiceTypeSpecifier
(s/def :elm.choice-type-specifier/choice
  (s/coll-of :elm/type-specifier))


(s/def :elm/choice-type-specifier
  (s/keys :req-un [:elm.choice-type-specifier/choice]))


(defmethod type-specifier "ChoiceTypeSpecifier" [_]
  :elm/choice-type-specifier)



;; 5. Libraries

;; 5.3. VersionedIdentifier
(s/def :elm.versioned-identifier/id
  string?)


(s/def :elm.versioned-identifier/system
  string?)


(s/def :elm.versioned-identifier/version
  string?)


(s/def :elm/versioned-identifier
  (s/keys
    :opt-un
    [:elm.versioned-identifier/id
     :elm.versioned-identifier/system
     :elm.versioned-identifier/version]))


;; 5.1. Library
(s/def :elm.library/identifier
  :elm/versioned-identifier)


(s/def :elm.library/schemaIdentifier
  :elm/versioned-identifier)


(s/def :elm.library.code-systems/def
  (s/coll-of :elm/code-system-def))


(s/def :elm.library/codeSystems
  (s/keys :req-un [:elm.library.code-systems/def]))


(s/def :elm.library.value-sets/def
  (s/coll-of :elm/value-set-def))


(s/def :elm.library/valueSets
  (s/keys :req-un [:elm.library.value-sets/def]))


(s/def :elm.library.codes/def
  (s/coll-of :elm/code-def))


(s/def :elm.library/codes
  (s/keys :req-un [:elm.library.codes/def]))


(s/def :elm.library.statements/def
  (s/coll-of :elm/expression-def))


(s/def :elm.library/statements
  (s/keys :req-un [:elm.library.statements/def]))


(s/def :elm/library
  (s/keys :req-un [:elm.library/identifier :elm.library/schemaIdentifier]
          :opt-un [:elm.library/codeSystems
                   :elm.library/valueSets
                   :elm.library/codes
                   :elm.library/statements]))



;; 8. Expressions

;; 8.2. OperatorExpression
(defmethod expression :elm.spec.type/operator-expression [_]
  (s/keys :opt-un [:elm/signature]))


;; 8.3. UnaryExpression
(s/def :elm.unary-expression/operand
  :elm/expression)


(defmethod expression :elm.spec.type/unary-expression [_]
  (s/keys :req-un [:elm.unary-expression/operand]))


;; 8.4. BinaryExpression
(s/def :elm.binary-expression/operand
  (s/tuple :elm/expression :elm/expression))


(defmethod expression :elm.spec.type/binary-expression [_]
  (s/keys :req-un [:elm.binary-expression/operand]))


;; 8.5. TernaryExpression
(s/def :elm.ternary-expression/operand
  (s/tuple :elm/expression :elm/expression :elm/expression))


(defmethod expression :elm.spec.type/ternary-expression [_]
  (s/keys :req-un [:elm.ternary-expression/operand]))


;; 8.6. NaryExpression
(s/def :elm.nary-expression/operand
  (s/coll-of :elm/expression))


(defmethod expression :elm.spec.type/nary-expression [_]
  (s/keys :req-un [:elm.nary-expression/operand]))


;; 8.7. AggregateExpression
(s/def :elm.aggregate-expression/source
  :elm/expression)


(defmethod expression :elm.spec.type/aggregate-expression [_]
  (s/keys :req-un [:elm.aggregate-expression/source]
          :opt-un [:elm/signature :elm/path]))



;; 9. Reusing Logic

;; 9.1. ExpressionDef
(s/def :elm.expression-def/context
  :elm/expression-execution-context)


(s/def :elm/expression-def
  (s/keys
    :opt-un
    [:elm/expression
     :elm/name
     :elm.expression-def/context
     :elm/accessLevel]))


;; 9.2. ExpressionRef
(defmethod expression :elm.spec.type/expression-ref [_]
  (s/keys :opt-un [:elm/name :elm/libraryName]))


;; 9.4. FunctionRef
(defmethod expression :elm.spec.type/function-ref [_]
  (s/keys :opt-un [:elm/name :elm/libraryName :elm.nary-expression/operand]))



;; 10. Queries
(s/def :elm.query/source
  (s/coll-of :elm/aliased-query-source :min-count 1))


(s/def :elm.query/relationship
  (s/coll-of
    (s/or :with :elm.query/with
          :with-equiv :elm.query.life/with-equiv
          :without :elm.query/without)))


(s/def :elm.query/suchThat
  :elm/expression)


(s/def :elm.query.life/equivOperand
  (s/tuple :elm/expression :elm/expression))


(s/def :elm.query/where
  :elm/expression)


;; 10.1. Query
(defmethod expression :elm.spec.type/query [_]
  (s/keys :req-un [:elm.query/source]
          :opt-un [:elm.query/relationship :elm.query/where]))


;; 10.2. AliasedQuerySource
(s/def :elm/aliased-query-source
  (s/keys :req-un [:elm/expression :elm/alias]))


;; 10.3. AliasRef
(defmethod expression :elm.spec.type/alias-ref [_]
  (s/keys :opt-un [:elm/name]))


;; 10.4. ByColumn
(defmethod sort-by-item "ByColumn" [_]
  (s/keys :opt-un [:elm/direction :elm/path]))


;; 10.5. ByDirection
(defmethod sort-by-item "ByDirection" [_]
  (s/keys :opt-un [:elm/direction]))


;; 10.6. ByExpression
(defmethod sort-by-item "ByExpression" [_]
  (s/keys :req-un [:elm/expression] :opt-un [:elm/direction]))


;; 10.11. SortClause
(s/def :elm.query/sort-clause
  (s/keys :req-un [:elm.sort/by]))


;; 10.12. With
(s/def :elm.query/with
  (s/keys :req-un [:elm/expression :elm/alias :elm.query/suchThat]))


(s/def :elm.query.life/with-equiv
  (s/keys :req-un [:elm/expression :elm/alias :elm.query.life/equivOperand]
          :opt-un [:elm.query/suchThat]))


;; 10.13. Without
(s/def :elm.query/without
  (s/keys :req-un [:elm/expression :elm/alias :elm.query/suchThat]))



;; 11. External Data

;; 11.1. Retrieve
(s/def :elm.retrieve/codes
  :elm/expression)


(s/def :elm.retrieve/dateRange
  :elm/expression)


;; An expression that, when evaluated, provides the context for the retrieve.
;; The expression evaluates to the resource that will be used as the context
;; for the retrieve.
(s/def :elm.retrieve/context
  :elm/expression)


(s/def :elm.retrieve/dataType
  string?)


(s/def :elm.retrieve/codeProperty
  string?)


(defmethod expression :elm.spec.type/retrieve [_]
  (s/keys
    :req-un
    [:elm.retrieve/dataType]
    :opt-un
    [:elm.retrieve/codes
     :elm.retrieve/dateRange
     :elm.retrieve/context
     :elm.retrieve/codeProperty]))



;; 12. Comparison Operators

;; 12.1. Equal
(derive :elm.spec.type/equal :elm.spec.type/binary-expression)


;; 12.2. Equivalent
(derive :elm.spec.type/equivalent :elm.spec.type/binary-expression)


;; 12.3. Greater
(derive :elm.spec.type/greater :elm.spec.type/binary-expression)


;; 12.4. GreaterOrEqual
(derive :elm.spec.type/greater-or-equal :elm.spec.type/binary-expression)


;; 12.5. Less
(derive :elm.spec.type/less :elm.spec.type/binary-expression)


;; 12.6. LessOrEqual
(derive :elm.spec.type/less-or-equal :elm.spec.type/binary-expression)


;; 12.7. NotEqual
(derive :elm.spec.type/not-equal :elm.spec.type/binary-expression)



;; 13. Logical Operators

;; 13.1. And
(derive :elm.spec.type/and :elm.spec.type/binary-expression)


;; 13.2. Implies
(derive :elm.spec.type/implies :elm.spec.type/binary-expression)


;; 13.3. Not
(derive :elm.spec.type/not :elm.spec.type/unary-expression)


;; 13.4. Or
(derive :elm.spec.type/or :elm.spec.type/binary-expression)


;; 13.4. Xor
(derive :elm.spec.type/xor :elm.spec.type/binary-expression)



;; 14. Nullological Operators

;; 14.1. Null
(defmethod expression :elm.spec.type/null [_]
  (s/keys :opt-un [:elm/valueType]))


;; 14.2. Coalesce
(derive :elm.spec.type/coalesce :elm.spec.type/nary-expression)


;; 14.3. IsFalse
(derive :elm.spec.type/is-false :elm.spec.type/unary-expression)


;; 14.4. IsNull
(derive :elm.spec.type/is-null :elm.spec.type/unary-expression)


;; 14.5. IsTrue
(derive :elm.spec.type/is-true :elm.spec.type/unary-expression)



;; 15. Conditional Operators

(s/def :elm/when
  :elm/expression)


(s/def :elm/then
  :elm/expression)


(s/def :elm/else
  :elm/expression)


;; 15.1. Case
(s/def :elm.case/comparand
  :elm/expression)


(s/def :elm.case/item
  (s/keys :req-un [:elm/when :elm/then]))


(s/def :elm/caseItem
  (s/coll-of :elm.case/item :min-count 1))


(defmethod expression :elm.spec.type/case [_]
  (s/keys :req-un [:elm/caseItem :elm/else] :op-un [:elm.case/comparand]))


;; 15.2. If
(defmethod expression :elm.spec.type/if [_]
  (s/keys :req-un [:elm/condition :elm/then :elm/else]))



;; 16. Arithmetic Operators

;; 16.1. Abs
(derive :elm.spec.type/abs :elm.spec.type/unary-expression)


;; 16.2. Add
(derive :elm.spec.type/add :elm.spec.type/binary-expression)


;; 16.3. Ceiling
(derive :elm.spec.type/ceiling :elm.spec.type/unary-expression)


;; 16.4. Divide
(derive :elm.spec.type/divide :elm.spec.type/binary-expression)


;; 16.5. Exp
(derive :elm.spec.type/exp :elm.spec.type/unary-expression)


;; 16.6. Floor
(derive :elm.spec.type/floor :elm.spec.type/unary-expression)


;; 16.7. Log
(derive :elm.spec.type/log :elm.spec.type/binary-expression)


;; 16.8. Ln
(derive :elm.spec.type/ln :elm.spec.type/unary-expression)


;; 16.9. MaxValue
(defmethod expression :elm.spec.type/max-value [_]
  (s/keys :req-un [:elm/valueType]))


;; 16.10. MinValue
(defmethod expression :elm.spec.type/min-value [_]
  (s/keys :req-un [:elm/valueType]))


;; 16.11. Modulo
(derive :elm.spec.type/modulo :elm.spec.type/binary-expression)


;; 16.12. Multiply
(derive :elm.spec.type/multiply :elm.spec.type/binary-expression)


;; 16.13. Negate
(derive :elm.spec.type/negate :elm.spec.type/unary-expression)


;; 16.14. Power
(derive :elm.spec.type/power :elm.spec.type/binary-expression)


;; 16.15. Predecessor
(derive :elm.spec.type/predecessor :elm.spec.type/unary-expression)


;; 16.16. Round
(s/def :elm.round/precision
  :elm/expression)


(defmethod expression :elm.spec.type/round [_]
  (s/keys :req-un [:elm.unary-expression/operand]
          :opt-un [:elm.round/precision]))


;; 16.17. Subtract
(derive :elm.spec.type/subtract :elm.spec.type/binary-expression)


;; 16.18. Successor
(derive :elm.spec.type/successor :elm.spec.type/unary-expression)


;; 16.19. Truncate
(derive :elm.spec.type/truncate :elm.spec.type/unary-expression)


;; 16.20. TruncatedDivide
(derive :elm.spec.type/truncated-divide :elm.spec.type/binary-expression)



;; 17. String Operators

;; 17.1. Combine
(defmethod expression :elm.spec.type/combine [_]
  (s/keys :req-un [:elm/source] :opt-un [:elm/separator]))


;; 17.2. Concatenate
(derive :elm.spec.type/concatenate :elm.spec.type/nary-expression)


;; 17.3. EndsWith
(derive :elm.spec.type/ends-with :elm.spec.type/binary-expression)


;; 17.6. Indexer
(derive :elm.spec.type/indexer :elm.spec.type/binary-expression)


;; 17.7. LastPositionOf
(defmethod expression :elm.spec.type/last-position-of [_]
  (s/keys :req-un [:elm/pattern :elm/string]))


;; 17.8. Length
(derive :elm.spec.type/length :elm.spec.type/unary-expression)


;; 17.9. Lower
(derive :elm.spec.type/lower :elm.spec.type/unary-expression)


;; 17.10. Matches
(derive :elm.spec.type/matches :elm.spec.type/binary-expression)


;; 17.12. PositionOf
(defmethod expression :elm.spec.type/position-of [_]
  (s/keys :req-un [:elm/pattern :elm/string]))


;; 17.13. ReplaceMatches
(derive :elm.spec.type/replace-matches :elm.spec.type/ternary-expression)


;; 17.14. Split
(defmethod expression :elm.spec.type/split [_]
  (s/keys :req-un [:elm/stringToSplit] :opt-un [:elm/separator]))


;; 17.15. SplitOnMatches
(defmethod expression :elm.spec.type/split-on-matches [_]
  (s/keys :req-un [:elm/stringToSplit :elm/separatorPattern]))


;; 17.16. StartsWith
(derive :elm.spec.type/starts-with :elm.spec.type/binary-expression)


;; 17.17. Substring
(defmethod expression :elm.spec.type/substring [_]
  (s/keys :req-un [:elm/stringToSub :elm/startIndex] :opt-un [:elm/length]))


;; 17.18. Upper
(derive :elm.spec.type/upper :elm.spec.type/unary-expression)



;; 18. Date and Time Operators
;;
;; TODO: why are both cases allowed? See DurationBetween vs. SameAs
(s/def :elm.date/precision
  #{"Year" "Month" "Week" "Day"
    "Hour" "Minute" "Second" "Millisecond"
    "year" "month" "week" "day"
    "hour" "minute" "second" "millisecond"})


;; 18.6. Date
(defn year-value-gen []
  (gen/fmap str (s/gen (s/int-in 1 10000))))


(s/def :elm.date/year
  (s/with-gen :elm/expression #(s/gen :elm/integer {:elm.integer/value year-value-gen})))


(defn month-value-gen []
  (gen/fmap str (s/gen (s/int-in 1 13))))


(s/def :elm.date/month
  (s/with-gen :elm/expression #(s/gen :elm/integer {:elm.integer/value month-value-gen})))


(defn day-value-gen []
  (gen/fmap str (s/gen (s/int-in 1 32))))


(s/def :elm.date/day
  (s/with-gen :elm/expression #(s/gen :elm/integer {:elm.integer/value day-value-gen})))


(s/def :elm.date/type
  #{"Date"})


(s/def :elm/year
  (s/keys :req-un [:elm.date/type :elm.date/year]))


(s/def :elm/literal-year
  :elm/year)


(s/def :elm/year-month
  (s/keys :req-un [:elm.date/type :elm.date/year :elm.date/month]))


(s/def :elm/literal-year-month
  :elm/year-month)

(s/def :elm/date
  (s/keys
    :req-un
    [:elm.date/type
     (or :elm.date/year
         (and :elm.date/year :elm.date/month)
         (and :elm.date/year :elm.date/month :elm.date/day))]))


(s/def :elm/literal-date
  (s/with-gen
    :elm/date
    #(s/gen
       (s/and
         :elm/date
         (fn [{{year :value} :year {month :value} :month {day :value} :day}]
           (or
             (nil? day)
             (try
               (LocalDate/of
                 (Long/parseLong year)
                 (Long/parseLong month)
                 (Long/parseLong day))
               (catch Exception _))))))))


(defmethod expression :elm.spec.type/date [_]
  (s/or :year :elm/year :year-month :elm/year-month :date :elm/date))


;; 18.7. DateFrom
(derive :elm.spec.type/date-from :elm.spec.type/unary-expression)


;; 18.8. DateTime
(defn hour-value-gen []
  (gen/fmap str (s/gen (s/int-in 0 24))))


(s/def :elm/hour
  (s/with-gen :elm/expression #(s/gen :elm/integer {:elm.integer/value hour-value-gen})))


(defn minute-value-gen []
  (gen/fmap str (s/gen (s/int-in 0 60))))


(s/def :elm/minute
  (s/with-gen :elm/expression #(s/gen :elm/integer {:elm.integer/value minute-value-gen})))


(defn second-value-gen []
  (gen/fmap str (s/gen (s/int-in 0 60))))


(s/def :elm/second
  (s/with-gen :elm/expression #(s/gen :elm/integer {:elm.integer/value second-value-gen})))


(defn timezone-offset-gen []
  (->> (gen/double* {:infinite? false :NaN? false :min -18 :max 18})
       (gen/fmap #(BigDecimal/valueOf ^double %))
       (gen/fmap str)))


(s/def :elm/timezone-offset
  (s/with-gen :elm/expression #(s/gen :elm/decimal {:elm.decimal/value timezone-offset-gen})))


(s/def :elm.date-time/type
  #{"DateTime"})


(s/def :elm/date-time
  (s/keys
    :req-un
    [:elm.date-time/type
     (or :elm.date/year
         (and :elm.date/year :elm.date/month)
         (and :elm.date/year :elm.date/month :elm.date/day)
         (and :elm.date/year :elm.date/month :elm.date/day :elm/hour)
         (and :elm.date/year :elm.date/month :elm.date/day :elm/hour
              :elm/timezone-offset)
         (and :elm.date/year :elm.date/month :elm.date/day :elm/hour
              :elm/minute)
         (and :elm.date/year :elm.date/month :elm.date/day :elm/hour
              :elm/minute :elm/timezone-offset)
         (and :elm.date/year :elm.date/month :elm.date/day :elm/hour
              :elm/minute :elm/second)
         (and :elm.date/year :elm.date/month :elm.date/day :elm/hour
              :elm/minute :elm/second :elm/timezone-offset))]))


(s/def :elm/literal-date-time
  (s/with-gen
    :elm/date-time
    #(s/gen
       (s/and
         :elm/date-time
         (fn [{{year :value} :year {month :value} :month {day :value} :day}]
           (or
             (nil? day)
             (try
               (LocalDate/of
                 (Long/parseLong year)
                 (Long/parseLong month)
                 (Long/parseLong day))
               (catch Exception _))))))))


(defmethod expression :elm.spec.type/date-time [_]
  :elm/date-time)


;; 18.9. DateTimeComponentFrom
(defmethod expression :elm.spec.type/date-time-component-from [_]
  (s/keys :req-un [:elm.unary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 18.10. DifferenceBetween
(defmethod expression :elm.spec.type/difference-between [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 18.11. DurationBetween
(defmethod expression :elm.spec.type/duration-between [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 18.13. Now
(derive :elm.spec.type/now :elm.spec.type/operator-expression)


;; 18.14. SameAs
(defmethod expression :elm.spec.type/same-as [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 18.15. SameOrBefore
(defmethod expression :elm.spec.type/same-or-before [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 18.16. SameOrAfter
(defmethod expression :elm.spec.type/same-or-after [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 18.18. Time
(s/def :elm.time/type
  #{"Time"})


(s/def :elm/time
  (s/keys
    :req-un
    [:elm.time/type
     (or :elm/hour
         (and :elm/hour :elm/minute)
         (and :elm/hour :elm/minute :elm/second))]))


(s/def :elm/literal-time
  :elm/time)


(defmethod expression :elm.spec.type/time [_]
  :elm/time)


;; 18.19. TimeFrom
(derive :elm.spec.type/time-from :elm.spec.type/unary-expression)


;; 18.20. TimezoneOffsetFrom
(derive :elm.spec.type/timezone-offset-from :elm.spec.type/unary-expression)


;; 18.21. TimeOfDay
(derive :elm.spec.type/time-of-day :elm.spec.type/operator-expression)


;; 18.22. Today
(derive :elm.spec.type/today :elm.spec.type/operator-expression)



;; 19. Interval Operators

;; 19.1. Interval
(s/def :elm.interval/low
  :elm/expression)


(s/def :elm.interval/lowClosedExpression
  :elm/expression)


(s/def :elm.interval/high
  :elm/expression)


(s/def :elm.interval/highClosedExpression
  :elm/expression)


(s/def :elm.interval/lowClosed
  boolean?)


(s/def :elm.interval/highClosed
  boolean?)


(defmethod expression :elm.spec.type/interval [_]
  (s/keys
    :opt-un
    [:elm.interval/low
     :elm.interval/lowClosedExpression
     :elm.interval/high
     :elm.interval/highClosedExpression
     :elm.interval/lowClosed
     :elm.interval/highClosed]))


;; 19.2. After
(defmethod expression :elm.spec.type/after [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.3. Before
(defmethod expression :elm.spec.type/before [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.4. Collapse
(derive :elm.spec.type/collapse :elm.spec.type/binary-expression)


;; 19.5. Contains
(defmethod expression :elm.spec.type/contains [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.6. End
(derive :elm.spec.type/end :elm.spec.type/unary-expression)


;; 19.7. Ends
(defmethod expression :elm.spec.type/ends [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.10. Except
(derive :elm.spec.type/except :elm.spec.type/nary-expression)


;; 19.11. Expand
(derive :elm.spec.type/expand :elm.spec.type/binary-expression)


;; 19.12. In
(defmethod expression :elm.spec.type/in [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.13. Includes
(defmethod expression :elm.spec.type/includes [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.14. IncludedIn
(defmethod expression :elm.spec.type/included-in [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.15. Intersect
(derive :elm.spec.type/intersect :elm.spec.type/nary-expression)


;; 19.16. Meets
(defmethod expression :elm.spec.type/meets [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.17. MeetsBefore
(defmethod expression :elm.spec.type/meets-before [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.18. MeetsAfter
(defmethod expression :elm.spec.type/meets-after [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.20. Overlaps
(defmethod expression :elm.spec.type/overlaps [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.21. OverlapsBefore
(defmethod expression :elm.spec.type/overlaps-before [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.22. OverlapsAfter
(defmethod expression :elm.spec.type/overlaps-after [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.23. PointFrom
(derive :elm.spec.type/point-from :elm.spec.type/unary-expression)


;; 19.24. ProperContains
(defmethod expression :elm.spec.type/proper-contains [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.25. ProperIn
(defmethod expression :elm.spec.type/proper-in [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.26. ProperIncludes
(defmethod expression :elm.spec.type/proper-includes [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.27. ProperIncludedIn
(defmethod expression :elm.spec.type/proper-included-in [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.28. Size
(derive :elm.spec.type/size :elm.spec.type/unary-expression)


;; 19.29. Start
(derive :elm.spec.type/start :elm.spec.type/unary-expression)


;; 19.30. Starts
(defmethod expression :elm.spec.type/starts [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 19.31. Union
(derive :elm.spec.type/union :elm.spec.type/nary-expression)


;; 19.32. Width
(derive :elm.spec.type/width :elm.spec.type/unary-expression)



;; 20. List Operators

;; 20.1. List
(s/def :elm.list/typeSpecifier
  :elm/type-specifier)


(s/def :elm.list/element
  (s/coll-of :elm/expression))


(defmethod expression :elm.spec.type/list [_]
  (s/keys :opt-un [:elm.list/typeSpecifier :elm.list/element]))


;; 20.3. Current
(defmethod expression :elm.spec.type/current [_]
  (s/keys :opt-un [:elm/scope]))


;; 20.4. Distinct
(derive :elm.spec.type/distinct :elm.spec.type/unary-expression)


;; 20.8. Exists
(derive :elm.spec.type/exists :elm.spec.type/unary-expression)


;; 20.9. Filter
(defmethod expression :elm.spec.type/filter [_]
  (s/keys :req-un [:elm/source :elm/condition] :opt-un [:elm/scope]))


;; 20.10. First
(defmethod expression :elm.spec.type/first [_]
  (s/keys :req-un [:elm/source] :opt-un [:elm/orderBy]))


;; 20.11. Flatten
(derive :elm.spec.type/flatten :elm.spec.type/unary-expression)


;; 20.12. ForEach
(defmethod expression :elm.spec.type/for-each [_]
  (s/keys :req-un [:elm/source :elm/element] :opt-un [:elm/scope]))


;; 20.16. IndexOf
(defmethod expression :elm.spec.type/index-of [_]
  (s/keys :req-un [:elm/source :elm/element]))


;; 20.18. Last
(defmethod expression :elm.spec.type/last [_]
  (s/keys :req-un [:elm/source] :opt-un [:elm/orderBy]))


;; 20.24. Repeat
(defmethod expression :elm.spec.type/repeat [_]
  (s/keys :req-un [:elm/source :elm/element] :opt-un [:elm/scope]))


;; 20.25. SingletonFrom
(derive :elm.spec.type/singleton-from :elm.spec.type/unary-expression)


;; 20.26. Slice
;;
;; Postel's Law: there are defaults for startIndex and endIndex, so they can be
;;               optional.
(defmethod expression :elm.spec.type/slice [_]
  (s/keys :req-un [:elm/source] :opt-un [:elm/startIndex :elm/endIndex]))


;; 20.27. Sort
(defmethod expression :elm.spec.type/sort [_]
  (s/keys :req-un [:elm.sort/by]))



;; 21. Aggregate Operators

;; 21.1. AllTrue
(derive :elm.spec.type/all-true :elm.spec.type/aggregate-expression)


;; 21.2. AnyTrue
(derive :elm.spec.type/any-true :elm.spec.type/aggregate-expression)


;; 21.3. Avg
(derive :elm.spec.type/avg :elm.spec.type/aggregate-expression)


;; 21.4. Count
(derive :elm.spec.type/count :elm.spec.type/aggregate-expression)


;; 21.5. GeometricMean
(derive :elm.spec.type/geometric-mean :elm.spec.type/aggregate-expression)


;; 21.6. Product
(derive :elm.spec.type/product :elm.spec.type/aggregate-expression)


;; 21.7. Max
(derive :elm.spec.type/max :elm.spec.type/aggregate-expression)


;; 21.8. Median
(derive :elm.spec.type/median :elm.spec.type/aggregate-expression)


;; 21.9. Min
(derive :elm.spec.type/min :elm.spec.type/aggregate-expression)


;; 21.10. Mode
(derive :elm.spec.type/mode :elm.spec.type/aggregate-expression)


;; 21.11. PopulationVariance
(derive :elm.spec.type/population-variance :elm.spec.type/aggregate-expression)


;; 21.12. PopulationStdDev
(derive :elm.spec.type/population-std-dev :elm.spec.type/aggregate-expression)


;; 21.13. Sum
(derive :elm.spec.type/sum :elm.spec.type/aggregate-expression)


;; 21.14. StdDev
(derive :elm.spec.type/std-dev :elm.spec.type/aggregate-expression)


;; 21.15. Variance
(derive :elm.spec.type/variance :elm.spec.type/aggregate-expression)



;; 22. Type Operators

;; 22.1. TODO As
(derive :elm.spec.type/as :elm.spec.type/unary-expression)


;; 22.2. TODO CanConvert
(derive :elm.spec.type/can-convert :elm.spec.type/unary-expression)


;; 22.3. CanConvertQuantity
(derive :elm.spec.type/can-convert-quantity :elm.spec.type/binary-expression)


;; 22.4. Children
(defmethod expression :elm.spec.type/children [_]
  (s/keys :req-un [:elm/source]))


;; 22.5. TODO Convert
(derive :elm.spec.type/convert :elm.spec.type/unary-expression)


;; 22.6. ConvertQuantity
(derive :elm.spec.type/convert-quantity :elm.spec.type/binary-expression)


;; 22.7. ConvertsToBoolean
(derive :elm.spec.type/converts-to-boolean :elm.spec.type/unary-expression)


;; 22.8. ConvertsToDate
(derive :elm.spec.type/converts-to-date :elm.spec.type/unary-expression)


;; 22.9. ConvertsToDateTime
(derive :elm.spec.type/converts-to-date-time :elm.spec.type/unary-expression)


;; 22.10. ConvertsToDecimal
(derive :elm.spec.type/converts-to-decimal :elm.spec.type/unary-expression)


;; 22.11. ConvertsToInteger
(derive :elm.spec.type/converts-to-integer :elm.spec.type/unary-expression)


;; 22.12. ConvertsToQuantity
(derive :elm.spec.type/converts-to-quantity :elm.spec.type/unary-expression)


;; 22.13. ConvertsToRatio
(derive :elm.spec.type/converts-to-ratio :elm.spec.type/unary-expression)


;; 22.14. ConvertsToString
(derive :elm.spec.type/converts-to-string :elm.spec.type/unary-expression)


;; 22.15. ConvertsToTime
(derive :elm.spec.type/converts-to-time :elm.spec.type/unary-expression)


;; 22.16. Descendents
(defmethod expression :elm.spec.type/descendents [_]
  (s/keys :req-un [:elm/source]))


;; 22.17. TODO Is
(derive :elm.spec.type/is :elm.spec.type/unary-expression)


;; 22.18. ToBoolean
(derive :elm.spec.type/to-boolean :elm.spec.type/unary-expression)


;; 22.19. ToChars
(derive :elm.spec.type/to-chars :elm.spec.type/unary-expression)


;; 22.20. ToConcept
(derive :elm.spec.type/to-concept :elm.spec.type/unary-expression)


;; 22.21. ToDate
(derive :elm.spec.type/to-date :elm.spec.type/unary-expression)


;; 22.22. ToDateTime
(derive :elm.spec.type/to-date-time :elm.spec.type/unary-expression)


;; 22.23. ToDecimal
(derive :elm.spec.type/to-decimal :elm.spec.type/unary-expression)


;; 22.24. ToInteger
(derive :elm.spec.type/to-integer :elm.spec.type/unary-expression)


;; 22.25. ToList
(derive :elm.spec.type/to-list :elm.spec.type/unary-expression)


;; 22.26. ToQuantity
(derive :elm.spec.type/to-quantity :elm.spec.type/unary-expression)


;; 22.27. ToRatio
(derive :elm.spec.type/to-ratio :elm.spec.type/unary-expression)


;; 22.28. ToString
(derive :elm.spec.type/to-string :elm.spec.type/unary-expression)


;; 22.29. ToTime
(derive :elm.spec.type/to-time :elm.spec.type/unary-expression)



;; 23. Clinical Operators

;; 23.3. CalculateAge
(defmethod expression :elm.spec.type/calculate-age [_]
  (s/keys :req-un [:elm.unary-expression/operand]
          :opt-un [:elm.date/precision]))


;; 23.4. CalculateAgeAt
(defmethod expression :elm.spec.type/calculate-age-at [_]
  (s/keys :req-un [:elm.binary-expression/operand]
          :opt-un [:elm.date/precision]))

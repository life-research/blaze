(ns blaze.anomaly
  (:require
    [cognitect.anomalies :as anom]))


(defn ex-anom
  "Creates an ExceptionInfo with `message` and an anomaly build from `kvs`,
  `category` and `message` as data."
  [anom]
  (ex-info (::anom/message anom "") anom))


(defn throw-anom
  "Throws an ExceptionInfo build with `ex-anom`."
  [category message & {:as kvs}]
  (throw (ex-anom (assoc kvs ::anom/category category ::anom/message message))))


(defmacro when-ok [[binding-form form] & body]
  `(let [res# ~form]
     (if (::anom/category res#)
       res#
       (let [~binding-form res#] ~@body))))


(defmacro if-failed [[binding-form form] then else]
  `(let [res# ~form]
     (if (::anom/category res#)
       (let [~binding-form res#] ~then)
       ~else)))


(defmacro if-ok [[sym form] then else]
  `(let [~sym ~form]
     (if (::anom/category ~sym)
       ~else
       ~then)))


(defn conj-anom
  ([xs]
   xs)
  ([xs x]
   (if (::anom/category x)
     (reduced x)
     (conj xs x))))


(defn exceptionally [x f]
  (if (::anom/category x) (f x) x))

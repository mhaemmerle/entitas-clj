(ns entitas-clj.macros
  (:require [clojure.pprint :refer [pprint]]
            [camel-snake-kebab :refer [->kebab-case]]))

(defmacro with-time [& body]
  (let [fname (first (flatten body))]
    `(let [start-time# (.getTime (js/Date.))
           result# ~@body
           end-time# (.getTime (js/Date.))]
       (.log js/console ~fname "---" (- end-time# start-time#) "ms")
       result#)))

(defmacro defcomponent [ctype & fields]
  (let [fq-ctype (str (ns-name *ns*) "." ctype)
        ctype-string (str (->kebab-case ctype) "-type")]
    `(do
       (def ~(symbol ctype-string) ~fq-ctype)
       (defrecord ~ctype [~@fields]
         ~'entitas-clj.component/TypeInfo
         (~'get-type [~'this] ~fq-ctype)))))

;; (pprint (macroexpand-1 '(defcomponent ShipContainer)))

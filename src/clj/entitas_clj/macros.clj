(ns entitas-clj.macros)

(defmacro with-time [& body]
  (let [fname (first (flatten body))]
    `(let [start-time# (.getTime (js/Date.))
           result# ~@body
           end-time# (.getTime (js/Date.))]
       (.log js/console ~fname "---" (- end-time# start-time#) "ms")
       result#)))

(defmacro defcomponent [ctype & fields]
  (let [fq-ctype (str (ns-name *ns*) "." ctype)]
    `(defrecord ~ctype [~@fields]
       ~'entitas-clj.component/TypeInfo
       (~'get-type [~'this] ~fq-ctype))))

;; (clojure.pprint/pprint (macroexpand-1 '(defcomponent Hull)))

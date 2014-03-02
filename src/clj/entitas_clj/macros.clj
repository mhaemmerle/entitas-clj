(ns entitas-clj.macros)

(defmacro with-time [& body]
  (let [fname (first (flatten body))]
    `(let [start-time# (.getTime (js/Date.))
           result# ~@body
           end-time# (.getTime (js/Date.))]
       (.log js/console ~fname "---" (- end-time# start-time#) "ms")
       result#)))

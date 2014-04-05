(ns entitas-clj.component)

(defn create
  ([type]
     (create type nil))
  ([type data]
     {:type type :data data}))

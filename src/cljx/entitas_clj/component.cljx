(ns entitas-clj.component)

(defprotocol TypeInfo
  (get-type [this] nil))

(ns entitas-clj.entity
  (:require [clojure.set :refer [subset?]]
            [entitas-clj.component :refer [get-type]]))

;; FIXME refactor component mapping fun
(defn create [id & components]
  (let [component-map (into {} (map (fn [component] [(get-type component) component]) components))
        ctypes (set (map get-type components))]
    (atom {:id id
           :creation-index nil
           :components component-map
           :ctypes ctypes})))

(defn has-component-of-type [entity ctype]
  (contains? (:ctypes @entity) ctype))

(defn has-components-of-types [entity ctypes]
  (subset? ctypes (:ctypes @entity)))

(defn component-of-type [entity ctype]
  (get-in @entity [:components ctype]))

(defn contains-component [entity component]
  (not (nil? (component-of-type entity (get-type component)))))

(defn- do-add [entity component]
  (let [ctype (get-type component)]
    (swap! entity #(-> %
                       (update-in ,, [:ctypes] conj ctype)
                       (assoc-in ,, [:components ctype] component))))
  entity)

(defn add-component [entity component]
  (if (not (has-component-of-type entity (get-type component)))
    (do-add entity component)
    entity))

(defn exchange-component [entity component]
  (do-add entity component))

(defn remove-component-of-type [entity ctype]
  (when (has-component-of-type entity ctype)
    (swap! entity (fn [a]
                    (update-in a [:ctypes] #(set (remove #{ctype} %)))
                    (update-in a [:components] #(dissoc % ctype)))))
  entity)

(defn remove-component [entity component]
  (remove-component-of-type entity (get-type component)))

(ns entitas-clj.entity
  (:require [clojure.set :refer [subset?]]))

(defn create [id & comps]
  (let [components (into {} (map (fn [{:keys [type] :as comp}] [type comp]) comps))
        ctypes (set (map :type comps))]
    (atom {:id id
           :creation-index nil
           :components components
           :ctypes ctypes})))

(defn has-component-of-type [entity ctype]
  (contains? (:ctypes @entity) ctype))

(defn has-components-of-types [entity ctypes]
  (subset? ctypes (:ctypes @entity)))

(defn component-of-type [entity ctype]
  (get-in @entity [:components ctype]))

(defn data-for-component [entity ctype]
  (:data (component-of-type entity ctype)))

(defn contains-component [entity component]
  (not (nil? (component-of-type entity (:type component)))))

(defn- do-add [entity component]
  (let [ctype (:type component)]
    (swap! entity #(-> %
                       (update-in ,, [:ctypes] conj ctype)
                       (assoc-in ,, [:components ctype] component))))
  entity)

(defn add-component [entity component]
  (if (not (has-component-of-type entity (:type component)))
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

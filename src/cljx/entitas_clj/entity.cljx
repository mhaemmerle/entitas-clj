(ns entitas-clj.entity
  (:require [clojure.set :refer [subset?]]))

(defn new [id]
  {:id id
   :creation-index nil
   :components {}
   :ctypes #{}})

(defn has-component-of-type [entity ctype]
  (contains? (:ctypes entity) ctype))

(defn has-components-of-types [entity ctypes]
  (subset? ctypes (:ctypes entity)))

(defn component-of-type [entity ctype]
  (get-in entity [:components ctype]))

(defn contains-component [entity component]
  (not (nil? (component-of-type entity (:type component)))))

(defn- do-add [entity component]
  (let [ctype (:type component)]
    (-> entity
        (update-in ,, [:ctypes] conj ctype)
        (assoc-in ,, [:components ctype] component))))

(defn add-component [entity component]
  (if (not (has-component-of-type entity (:type component)))
    (do-add entity component)
    entity))

(defn exchange-component [entity component]
  (do-add entity component))

(defn remove-component-of-type [entity ctype]
  (if (has-component-of-type entity ctype)
    (-> entity
        (update-in ,, [:ctypes] #(set (remove #{ctype} %)))
        (update-in ,, [:components] #(dissoc % ctype)))
    entity))

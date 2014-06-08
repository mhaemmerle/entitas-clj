(ns entitas-clj.core
  (:require [entitas-clj.entity :as e]
            [entitas-clj.component :as c :refer [get-type]]
            [entitas-clj.repository :as r]))

#+clj (set! *warn-on-reflection* true)

(defn add-component [repository entity component]
  (let [new-entity (e/add-component entity component)]
    (r/add-component repository (get-type component) new-entity)))

(defn exchange-component [repository entity component]
  (let [new-entity (e/exchange-component entity component)]
    (r/exchange-component repository (get-type component) new-entity)))

(defn remove-component-of-type [repository entity ctype]
  (let [new-entity (e/remove-component-of-type entity ctype)]
    (r/remove-component repository ctype new-entity)))

(defn remove-component [repository entity component]
  (remove-component-of-type repository (get-type component) entity))

(ns entitas-clj.core
  (:require [entitas-clj.entity :as e]
            [entitas-clj.repository :as r]))

#+clj (set! *warn-on-reflection* true)

(defn add-component [repository entity component]
  (let [new-entity (e/add-component entity component)]
    [(r/add-component repository (:type component) new-entity) new-entity]))

(defn exchange-component [repository entity component]
  (let [new-entity (e/exchange-component entity component)]
    [(r/exchange-component repository (:type component) new-entity) new-entity]))

(defn remove-component [repository entity ctype]
  (let [new-entity (e/remove-component-of-type entity ctype)]
    [(r/remove-component repository ctype new-entity) new-entity]))

(defn destroy-entity [repository entity]
  (reduce (fn [[r e] ctype]
            (remove-component r e ctype)) [repository entity] (:ctypes entity)))
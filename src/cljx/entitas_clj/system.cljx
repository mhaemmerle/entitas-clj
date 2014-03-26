(ns entitas-clj.system
  (:refer-clojure :exclude [remove])
  (:require #+clj [clojure.core.async :refer [chan go sliding-buffer put! alts!]]
            #+cljs [cljs.core.async :refer [chan sliding-buffer put! alts!]]
            )
  #+cljs (:require-macros [cljs.core.async.macros :refer [go]])
  )

;; system

(defn create [type execute-fn]
  {:type type
   :active true
   :execute-fn execute-fn
   :activate-fn nil
   :deactivate-fn nil})

;; systems

(defn create-systems []
  [])

(defn add [systems system]
  (conj systems system))

(defn contains [systems system]
  (some #{system} systems))

(defn remove [systems system]
  (vec (clojure.core/remove #{system} systems)))

(defn execute [systems repository]
  (reduce (fn [repo system]
            ;; returns updated repository
            ((:execute-fn system) repository)) repository systems))

(defn activate [systems]
  (vec (map (fn [system]
              ((:activate-fn system) system)
              (assoc system :active true)) systems)))

(defn deactivate [systems]
  (vec (map (fn [system]
              ((:deactivate-fn system) system)
              (assoc system :active false)) systems)))

(defn remove-all [systems]
  [])

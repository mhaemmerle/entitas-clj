(ns entitas-clj.system
  (:refer-clojure :exclude [remove]))

;; system

(defn create []
  [])

(defn create-system []
  {:active true
   :activate-fn nil
   :deactivate-fn nil
   :execute-fn nil})

;; systems

(defn add [systems system]
  (conj systems system))

(defn contains [systems system]
  (some #{system} systems))

(defn remove [systems system]
  (vec (clojure.core/remove #{system} systems)))

(defn execute [systems]
  (doseq [system systems]
    ((:execute-fn system))))

(defn activate [systems]
  (map (fn [system]
         ((:activate-fn system) system)
         (assoc system :active true)) systems))

(defn deactivate [systems]
  (map (fn [system]
         ((:deactivate-fn system) system)
         (assoc system :active false)) systems))

(defn remove-all [systems]
  [])

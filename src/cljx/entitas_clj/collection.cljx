(ns entitas-clj.collection
  (:require [entitas-clj.matcher :as m]))

(defn init-with-matcher [matcher]
  {:matcher matcher
   :entities {}
   :add-observers #{}
   :remove-observers {}})

(defn init-with-types [types]
  (init-with-matcher #(m/all-of-set types %)))

(defn entities [collection]
  (:entities collection))

(defn type-matcher [collection]
  (:matcher collection))

(defn notify [observers event-type collection entity]
  (doseq [observer observers]
    (observer entity collection)))

(defn add-entity [collection {:keys [creation-index] :as entity}]
  (let [path [:entities creation-index]
        new-collection (if (nil? (get-in collection path))
                         (assoc-in collection path entity)
                         collection)]
    (notify (:add-observers collection) :added collection entity)
    new-collection))

(defn exchange-entity [collection {:keys [creation-index] :as entity}]
  (let [path [:entities creation-index]]
    (if (nil? (get-in collection path))
      (add-entity collection entity)
      (let [new-collection (assoc-in collection path entity)]
        (notify (:remove-observers collection) :removed new-collection entity)
        (notify (:add-observers collection) :added new-collection entity)
        new-collection))))

(defn remove-entity [collection {:keys [creation-index] :as entity}]
  (let [path [:entities creation-index]]
    (if (nil? (get-in collection path))
      collection
      (let [new-collection (update-in collection path dissoc creation-index)]
        (notify (:remove-observers collection) :removed new-collection entity)
        new-collection))))

(defn add-observer [collection observer event-type]
  (let [path (case event-type
               :added [:add-observers]
               :removed [:remove-observers])]
    (update-in collection path conj observer)))

(defn remove-observer [collection observer event-type]
  (let [path (case event-type
               :added [:add-observers]
               :removed [:remove-observers])]
    (update-in collection path remove observer)))

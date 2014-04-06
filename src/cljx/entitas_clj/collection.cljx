(ns entitas-clj.collection
  (:require [entitas-clj.matcher :as m]))

(defn init-with-matcher [matcher mname mkey]
  (atom {:mkey mkey
         :name mname
         :matcher matcher
         :entities {}
         :add-observers #{}
         :remove-observers {}}))

(defn init-with-types [ctypes]
  (let [mname "entitas-clj.matcher/all-of-set"
        mkey (m/to-key mname ctypes)]
    (init-with-matcher #(entitas-clj.matcher/all-of-set ctypes %) mname mkey)))

(defn entities [collection]
  (vals (:entities @collection)))

(defn type-matcher [collection]
  (:matcher @collection))

(defn notify [observers event-type collection entity]
  (doseq [observer observers]
    (observer entity collection)))

(defn do-add [c path entity]
  (if (nil? (get-in c path))
    (assoc-in c path entity)
    c))

(defn add-entity [collection entity]
  (let [path [:entities (:creation-index @entity)]]
    (swap! collection do-add path entity)
    (notify (:add-observers @collection) :added collection entity)
    collection))

(defn exchange-entity [collection entity]
  (let [path [:entities (:creation-index @entity)]]
    (if (nil? (get-in @collection path))
      (add-entity collection entity)
      (do
        (swap! collection assoc-in path entity)
        (notify (:remove-observers @collection) :removed collection entity)
        (notify (:add-observers @collection) :added collection entity)
        collection))))

(defn remove-entity [collection entity]
  (let [creation-index (:creation-index @entity)
        path [:entities creation-index]]
    (if (nil? (get-in @collection path))
      collection
      (do
        (swap! collection update-in [:entities] dissoc creation-index)
        (notify (:remove-observers @collection) :removed collection entity)
        collection))))

(defn add-observer [collection observer event-type]
  (let [path (case event-type
               :added [:add-observers]
               :removed [:remove-observers])]
    (swap! collection update-in path conj observer)))

(defn remove-observer [collection observer event-type]
  (let [path (case event-type
               :added [:add-observers]
               :removed [:remove-observers])]
    (swap! collection update-in path remove observer)))

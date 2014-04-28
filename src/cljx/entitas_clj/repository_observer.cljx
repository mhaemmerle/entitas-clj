(ns entitas-clj.repository-observer
  (:require [entitas-clj.collection :as cl]
            [entitas-clj.repository :as r]))

(defn start-listening! [observer]
  (let [local-observer @observer
        watched-collection (:watched-collection local-observer)
        change-trigger (:change-trigger local-observer)]
    (cl/add-observer watched-collection observer change-trigger)
    observer))

(defn stop-listening! [observer]
  (let [local-observer @observer
        watched-collection (:watched-collection local-observer)
        change-trigger (:change-trigger local-observer)]
    (cl/remove-observer watched-collection observer change-trigger)
    observer))

(defn deactivate! [observer]
  (swap! observer assoc :collected-entities [] :active false)
  (stop-listening! observer))

(defn activate! [observer]
  (swap! observer assoc :active true)
  (start-listening! observer))

(defn drain! [observer]
  (swap! observer assoc :collected-entities [])
  observer)

(defn entity-changed-in-collection! [observer entity collection change-type]
  (let [f (fn [collected-entities entity]
            (if (some #{entity} collected-entities)
              collected-entities
              (conj collected-entities entity)))]
    (swap! observer update-in [:collected-entities] f entity)
    observer))

(defn init-with-repository
  ([repository matcher-config]
     (init-with-repository repository matcher :added))
  ([repository matcher-config change-trigger]
     (let [[r0 watched-collection] (r/collection-for-matcher repository matcher-config)
           observer (atom {:active :true
                           ;; not so nice
                           :f entity-changed-in-collection!
                           :watched-collection watched-collection
                           :collected-entities []
                           :change-trigger change-trigger})]
       (start-listening! observer)
       r0)))

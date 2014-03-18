(ns entitas-clj.repository
  (:require [entitas-clj.entity :as e]
            [entitas-clj.collection :as c]
            [entitas-clj.matcher :as m]))

(defn create []
  {:entities {}
   :collections {}
   :collections-for-type {}
   :current-index 0})

(defn all-entities [repository]
  (vals (:entities repository)))

(defn add-entity [{:keys [current-index] :as repository} entity]
  (swap! entity assoc :creation-index current-index)
  (let [new-repository (-> repository
                           (assoc-in ,, [:entities current-index] entity)
                           (update-in ,, [:current-index] inc))]
    [new-repository entity]))

;; FIXME this also updates the collections in the ObjC version
(defn remove-entity [repository entity]
  (update-in repository [:entities] dissoc (:creation-index @entity)))

(defn contains-entity [repository entity]
  (not (nil? (get-in repository [:entities (:creation-index @entity)]))))

(defn internal-collections-for-type [repository ctype]
  (or (get-in repository [:collections-for-type ctype]) #{}))

(defn memoize-matcher [repository {:keys [mtype ctypes] :as matcher-config}]
  (let [matcher #(mtype ctypes %)]
    (reduce (fn [acc entity]
              (if (matcher (:ctypes @entity))
                (c/add-entity acc entity)
                acc))
            (c/init-with-matcher matcher) (all-entities repository))))

(defn collection-for-matcher [repository {:keys [mtype ctypes] :as matcher-config}]
  (let [mkey (m/to-key mtype ctypes)]
    (let [collection (get-in repository [:collections mkey])]
      (if (not (nil? collection))
        [repository collection]
        (let [mcoll (memoize-matcher repository matcher-config)]
          [(update-in repository [:collections] assoc mkey mcoll) mcoll])))))

(defn entities-for-matcher [repository matcher-config]
  (let [[new-repository collection] (collection-for-matcher repository matcher-config)]
    [new-repository (c/entities collection)]))

(defn collection-for-types [repository ctypes]
  (collection-for-matcher repository {:mtype m/all-of-set :ctypes ctypes}))

(defn add-component [repository ctype entity]
  ;; FIXME this is questionable at best
  (let [r1 (assoc-in repository [:entities (:creation-index @entity)] entity)
        f (fn [collection]
            (if ((:matcher collection) (:ctypes @entity))
              (c/add-entity collection entity)
              collection))
        cft (internal-collections-for-type r1 ctype)
        f2 (if (= 0 (count cft))
             (let [m #(m/just ctype %)
                   c (c/init-with-matcher m)]
               [(f c)])
             (vec (map f cft)))]
    (assoc-in r1 [:collections-for-type ctype] f2)))

(defn exchange-component [repository ctype entity]
  (let [f (fn [collection]
            (if ((:matcher collection) (:ctypes @entity))
              (c/exchange-entity collection entity)
              collection))]
    (update-in repository [:collections-for-type ctype] #(vec (map f %)))))

(defn remove-component [repository ctype entity]
  (let [original-ctypes (conj (:ctypes @entity) ctype)
        f (fn [collection]
            (if (and ((:matcher collection) original-ctypes)
                     (not ((:matcher collection) (:ctypes @entity))))
              (c/remove-entity collection entity)
              collection))]
    (update-in repository [:collections-for-type ctype] #(vec (map f %)))))

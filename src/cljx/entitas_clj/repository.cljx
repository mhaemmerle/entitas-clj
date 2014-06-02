(ns entitas-clj.repository
  (:require [entitas-clj.entity :as e]
            [entitas-clj.component :as c :refer [get-type]]
            [entitas-clj.collection :as cl]
            [entitas-clj.matcher :as m]))

(defn create []
  {:entities {}
   :collections {}
   :collections-for-type {}
   :current-index 0})

(defn all-entities [repository]
  (vals (:entities repository)))

(defn contains-entity [repository entity]
  (not (nil? (get-in repository [:entities (:creation-index @entity)]))))

(defn internal-collections-for-type [repository ctype]
  (or (get-in repository [:collections-for-type ctype]) #{}))

(defn memoize-matcher [repository {:keys [mtype mname ctypes] :as matcher-config}]
  (let [matcher #(mtype ctypes %)
        mkey (m/to-key mname ctypes)
        mcoll (reduce (fn [acc entity]
                        (if (matcher (:ctypes @entity))
                          (cl/add-entity acc entity)
                          acc))
                      (cl/init-with-matcher matcher mname mkey) (all-entities repository))
        r0 (update-in repository [:collections] assoc mkey mcoll)
        r1 (reduce (fn [acc ctype]
                     (let [f (fnil (fn [colls]
                                     (conj colls mcoll)) #{})]
                       (update-in acc [:collections-for-type ctype] f))) r0 ctypes)]
    [r1 mcoll]))

(defn collection-for-matcher [repository {:keys [mtype mname ctypes] :as matcher-config}]
  (let [mkey (m/to-key mname ctypes)
        collection (get-in repository [:collections mkey])]
    (if (nil? collection)
      (memoize-matcher repository matcher-config)
      [repository collection])))

(defn entities-for-matcher [repository matcher-config]
  (let [[new-repository collection] (collection-for-matcher repository matcher-config)]
    [new-repository (cl/entities collection)]))

(defn collection-for-types [repository ctypes]
  (let [mtype entitas-clj.matcher/all-of-set
        mname "entitas-clj.matcher/all-of-set"]
    (collection-for-matcher repository {:mtype mtype :mname mname :ctypes ctypes})))

(defn add-component [repository ctype entity]
  (let [f (fn [collection]
            (if ((:matcher @collection) (:ctypes @entity))
              (cl/add-entity collection entity)
              collection))
        cft (set (map f (internal-collections-for-type repository ctype)))]
    (if (empty? cft)
      repository
      (assoc-in repository [:collections-for-type ctype] cft))))

(defn exchange-component [repository ctype entity]
  (let [f (fn [collection]
            (if ((:matcher @collection) (:ctypes @entity))
              (cl/exchange-entity collection entity)
              collection))
        cft (set (map f (internal-collections-for-type repository ctype)))]
    (if (empty? cft)
      repository
      (assoc-in repository [:collections-for-type ctype] cft))))

(defn remove-component [repository ctype entity]
  (let [ctypes (:ctypes @entity)
        f (fn [collection]
            (if ((:matcher @collection) ctypes)
              (cl/remove-entity collection entity)
              collection))
        cft (set (map f (internal-collections-for-type repository ctype)))]
    (if (empty? cft)
      repository
      (assoc-in repository [:collections-for-type ctype] cft))))

(defn remove-entity [repository entity]
  (let [ctypes (:ctypes @entity)
        creation-index (:creation-index @entity)
        r1 (reduce (fn [acc ctype]
                     (remove-component acc ctype entity)) repository ctypes)]
    (update-in r1 [:entities] dissoc creation-index)))

(defn add-entity [{:keys [current-index] :as repository} entity]
  (swap! entity assoc :creation-index current-index)
  (let [components (vals (:components @entity))
        r0 (-> repository
               (assoc-in ,, [:entities current-index] entity)
               (update-in ,, [:current-index] inc))]
    (reduce (fn [acc component]
              (add-component acc (get-type component) entity)) r0 components)))

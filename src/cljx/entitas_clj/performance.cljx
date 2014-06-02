(ns entitas-clj.performance
  #+clj (:gen-class)
  #+cljs (:use-macros [entitas-clj.macros :only [with-time]])
  (:require [entitas-clj.core :as cr]
            [entitas-clj.repository :as r]
            [entitas-clj.entity :as e]
            [entitas-clj.component :as c :refer [get-type]]
            [entitas-clj.collection :as cl]
            [entitas-clj.matcher :as m]
            [entitas-clj.macros :refer [defcomponent]]
            ))

(defcomponent SomeComponent)
(defcomponent SomeOtherComponent)

(defn entry-creation-bench [repository num-entities]
  (reduce (fn [acc i]
            (let [entity (e/create :foo)
                  new-acc (r/add-entity acc entity)]
              (condp = (mod i 25)
                0 (cr/add-component new-acc entity (SomeComponent.))
                1 (cr/add-component new-acc entity (SomeOtherComponent.))
                new-acc)))
          repository (range num-entities)))

(defn collection-creation-bench [repository ctype]
  (r/collection-for-types repository #{ctype}))

(defn getting-all-entities-initially [collection]
  (let [result (atom nil)]
    (doseq [n (range 100)]
      (reset! result (cl/entities collection)))
    @result))

(defn getting-all-entities-from-repository [repository ctype]
  (let [result (atom nil)
        mtype entitas-clj.matcher/all-of-set
        mname "entitas-clj.matcher/all-of-set"
        matcher-config {:mtype mtype :mname mname :ctypes #{ctype}}]
    (doseq [n (range 100)]
      (let [[new-repository entities] (r/entities-for-matcher repository matcher-config)]
        (reset! result entities)))
    @result))

(defn exchange-component-in-all-entities [repository entities]
  (let [[i result] (reduce (fn [[idx acc] entity]
                             (let [c (SomeComponent.)
                                   r (cr/exchange-component acc entity c)]
                               [(inc idx) r])) [0 repository] entities)]
    (println "exhanged" i "components")
    result))

(defn destroy-all-entities [repository]
  (reduce (fn [acc entity]
            (r/remove-entity acc entity)) repository (r/all-entities repository)))

(defmacro with-time [& body]
  (let [fname (first (flatten body))]
    `(let [start-time# (System/currentTimeMillis)
           result# ~@body]
       (println ~fname "---" (- (System/currentTimeMillis) start-time#) "ms")
       result#)))

(defn run-test [entity-count]
  (let [r1 (with-time (entry-creation-bench (r/create) entity-count))
        [r2 c1] (with-time (collection-creation-bench r1 "entitas-clj.performance.SomeComponent"))
        c2 (with-time (getting-all-entities-initially c1))
        c3 (with-time (getting-all-entities-from-repository r2 "entitas-clj.performance.SomeComponent"))
        r3 (with-time (exchange-component-in-all-entities r2 c3))
        r4 (with-time (destroy-all-entities r3))]
    nil))

#+clj (defn -main [& args]
        (dotimes [n 100] (run-test 1000000)))

#+cljs (defn ^:export main []
         (.log js/console "Running benchmarks...")
         (run-test 1000000))

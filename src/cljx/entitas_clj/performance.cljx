(ns entitas-clj.performance
  #+clj (:gen-class)
  #+cljs (:use-macros [entitas-clj.macros :only [with-time]])
  (:require [entitas-clj.core :as cr]
            [entitas-clj.repository :as r]
            [entitas-clj.entity :as e]
            [entitas-clj.component :as cm]
            [entitas-clj.collection :as c]
            [entitas-clj.matcher :as m]
            #+clj [criterium.core :refer [bench]]
            ))

(def simple-component {:type :foo :a 1 :b 2})
(def another-simple-component {:type :bar :x 1 :y 2})

(defn entry-creation-bench [repository num-entities ctype1 ctype2]
  (reduce (fn [acc i]
            (let [entity (e/create :foo)
                  new-acc (r/add-entity acc entity)]
              (condp = (mod i 25)
                0 (cr/add-component new-acc entity (cm/create ctype1 nil))
                1 (cr/add-component new-acc entity (cm/create ctype2 nil))
                new-acc)))
          repository (range num-entities)))

(defn collection-creation-bench [repository ctype]
  (r/collection-for-types repository #{ctype}))

(defn getting-all-entities-initially [collection]
  (let [result (atom nil)]
    (doseq [n (range 100)]
      (reset! result (c/entities collection)))
    @result))



(defn getting-all-entities-from-repository [repository ctype]
  (let [result (atom nil)
        mtype entitas-clj.matcher/all-of-set
        mname (m/to-name #'entitas-clj.matcher/all-of-set)
        matcher-config {:mtype mtype :mname mname :ctypes #{ctype}}]
    (doseq [n (range 100)]
      (let [[new-repository entities] (r/entities-for-matcher repository matcher-config)]
        (reset! result entities)))
    @result))

(defn exchange-component-in-all-entities [repository entities]
  (let [[i result] (reduce (fn [[idx acc] entity]
                             (let [r (cr/exchange-component acc entity (cm/create :foo nil))]
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
  (let [ctype1 :foo
        ctype2 :bar
        r1 (with-time (entry-creation-bench (r/create) entity-count ctype1 ctype2))
        [r2 c1] (with-time (collection-creation-bench r1 ctype1))
        c2 (with-time (getting-all-entities-initially c1))
        c3 (with-time (getting-all-entities-from-repository r2 ctype1))
        r3 (with-time (exchange-component-in-all-entities r2 (vals c3)))
        r4 (with-time (destroy-all-entities r3))]
    nil))

#+clj (defn -main [& args]
        (dotimes [n 100] (run-test 1000000)))

#+cljs (defn ^:export main []
         (.log js/console "Running benchmarks...")
         (run-test 1000000))

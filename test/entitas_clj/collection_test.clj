(ns entitas-clj.collection-test
  (:use clojure.test
        clojure.pprint)
  (:require [entitas-clj.component :as c]
            [entitas-clj.entity :as e]
            [entitas-clj.collection :as cl]
            [entitas-clj.repository :as r]))

(deftest create
  (let [coll @(cl/init-with-types #{:foo :bar})]
    (is (= "entitas-clj.matcher/all-of-set" (:name coll)))
    (is (= "entitas-clj.matcher/all-of-set:foo:bar" (:mkey coll)))))

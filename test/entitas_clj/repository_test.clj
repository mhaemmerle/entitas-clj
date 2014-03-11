(ns entitas-clj.repository-test
  (:use clojure.test)
  (:require [entitas-clj.component :as c]
            [entitas-clj.entity :as e]
            [entitas-clj.repository :as r]))

(deftest create
  (let [repository (r/create)]
    (is (= {} (:entities repository)))
    (is (= {} (:collections repository)))
    (is (= {} (:collections-for-type repository)))
    (is (= 0 (:current-index repository)))))

(deftest add-entity
  (let [repository (r/create)
        entity (e/add-component (e/create :foo) (c/create :bar))
        [new-repository new-entity] (r/add-entity repository entity)]
    (is (= 1 (:current-index new-repository)))
    (is (= nil (:creation-index entity)))
    (is (= 0 (:creation-index new-entity)))
    (is (= new-entity (get-in new-repository [:entities 0])))))

(deftest remove-entity
  (let [repository (r/create)
        entity (e/add-component (e/create :foo) (c/create :bar))
        [r1 e1] (r/add-entity repository entity)
        r2 (r/remove-entity r1 e1)]
    (is (= {} (:entities r2)))
    (is (= 1 (:current-index r2)))))

(deftest remove-entity
  (let [repository (r/create)
        entity (e/add-component (e/create :foo) (c/create :bar))
        [r1 e1] (r/add-entity repository entity)
        r2 (r/remove-entity r1 e1)]
    (is (= {} (:entities r2)))
    (is (= 1 (:current-index r2)))))

(deftest contains-existing-entity
  (let [repository (r/create)
        entity (e/create :foo)
        [r1 e1] (r/add-entity repository entity)]
    (is (r/contains-entity r1 e1))
    (is (not (r/contains-entity r1 (e/create :baz))))))

(deftest add-component
  (let [repository (r/create)
        ctype :bar
        entity (e/add-component (e/create :foo) (c/create ctype))
        [r1 e1] (r/add-entity repository entity)
        r2 (r/add-component r1 ctype e1)]
    (is (not (nil? (get-in r2 [:collections-for-type ctype]))))))

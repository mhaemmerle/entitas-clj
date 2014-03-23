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
        entity (e/add-component (e/create :foo) (c/create :bar nil))]
    (is (= nil (:creation-index entity)))
    (let [new-repository (r/add-entity repository entity)]
      (is (= 1 (:current-index new-repository)))
      (is (= 0 (:creation-index @entity)))
      (is (= entity (get-in new-repository [:entities 0]))))))

(deftest remove-entity
  (let [repository (r/create)
        entity (e/add-component (e/create :foo) (c/create :bar nil))
        r1 (r/add-entity repository entity)
        r2 (r/remove-entity r1 entity)]
    (is (= {} (:entities r2)))
    (is (= 1 (:current-index r2)))))

(deftest remove-entity
  (let [repository (r/create)
        entity (e/add-component (e/create :foo) (c/create :bar nil))
        r1 (r/add-entity repository entity)
        r2 (r/remove-entity r1 entity)]
    (is (= {} (:entities r2)))
    (is (= 1 (:current-index r2)))))

(deftest contains-existing-entity
  (let [repository (r/create)
        entity (e/create :foo)
        r1 (r/add-entity repository entity)]
    (is (r/contains-entity r1 entity))
    (is (not (r/contains-entity r1 (e/create :baz))))))

(deftest add-component
  (let [repository (r/create)
        ctype :bar
        entity (e/add-component (e/create :foo) (c/create ctype nil))
        r1 (r/add-entity repository entity)
        r2 (r/add-component r1 ctype entity)]
    (is (not (nil? (get-in r2 [:collections-for-type ctype]))))))

(deftest exchange-component
  (let [repository (r/create)
        ctype :bar
        c1 (c/create ctype nil)
        entity (e/add-component (e/create :foo) c1)
        r1 (r/add-entity repository entity)
        r2 (r/add-component r1 ctype entity)
        c2 (assoc c1 :x 10 :y 10)
        _ (e/exchange-component entity c2)
        r3 (r/exchange-component r2 ctype entity)
        c-entity (get-in (first (get-in r3 [:collections-for-type ctype])) [:entities 0])]
    (is (= entity c-entity))))

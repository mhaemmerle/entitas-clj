(ns entitas-clj.repository-test
  (:use clojure.test)
  (:require [entitas-clj.component :as c]
            [entitas-clj.entity :as e]
            [entitas-clj.collection :as cl]
            [entitas-clj.repository :as r]))

(deftest create
  (let [repository (r/create)]
    (is (empty? (r/all-entities repository)))
    (is (= {} (:collections repository)))
    (is (= {} (:collections-for-type repository)))
    (is (= 0 (:current-index repository)))))

(deftest contains-existing-entity
  (let [repository (r/create)
        entity (e/create :foo)
        repo1 (r/add-entity repository entity)]
    (is (r/contains-entity repo1 entity))
    (is (not (r/contains-entity repo1 (e/create :baz))))))

(deftest add-component
  (let [repository (r/create)
        ctype :bar
        entity (e/add-component (e/create :foo) (c/create ctype nil))
        repo1 (r/add-entity repository entity)
        repo2 (r/add-component repo1 ctype entity)]
    (is (= nil (get-in repo2 [:collections-for-type ctype])))
    (is (= (r/all-entities repo2) (list entity)))))

(deftest exchange-component
  (let [repository (r/create)
        ctype :bar
        comp1 (c/create ctype nil)
        entity (e/add-component (e/create :foo) comp1)
        repo1 (r/add-entity repository entity)
        repo2 (r/add-component repo1 ctype entity)
        comp2 (assoc comp1 :x 10 :y 10)]
    (e/exchange-component entity comp2)
    (let [repo3 (r/exchange-component repo2 ctype entity)]
      (is (= nil (get-in repo3 [:collections-for-type ctype])))
      (is (= (r/all-entities repo3) (list entity))))))

(deftest remove-component
  (let [repository (r/create)
        ctype :bar
        entity (e/add-component (e/create :foo) (c/create ctype nil))
        repo1 (r/add-entity repository entity)
        repo2 (r/add-component repo1 ctype entity)
        [repo3 coll1] (r/collection-for-types repo2 #{ctype})
        cft1 (get-in repo3 [:collections-for-type ctype])]
    (is (= entity (first (cl/entities coll1))))
    (is (= #{coll1} cft1))
    (let [repo4 (r/remove-component repo3 ctype entity)
          [repo5 coll2] (r/collection-for-types repo4 #{ctype})
          cft2 (get-in repo4 [:collections-for-type ctype])]
      (is (= nil (cl/entities coll2)))
      (is (= #{coll2} cft2)))))

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
        repo1 (r/add-entity repository entity)
        repo2 (r/remove-entity repo1 entity)]
    (is (empty? (r/all-entities repo2)))
    (is (= 1 (:current-index repo2)))))

(deftest remove-entity-collection
  (let [repository (r/create)
        entity (e/add-component (e/create :foo) (c/create :bar nil))
        repo1 (r/add-entity repository entity)
        [repo2 coll1] (r/collection-for-types repo1 #{:bar})]
    (is (= 1 (count (cl/entities coll1))))
    (let [repo3 (r/remove-entity repo2 entity)
          [repo4 coll2] (r/collection-for-types repo3 #{:bar})]
      (is (empty? (r/all-entities repo3)))
      (is (= 1 (:current-index repo3)))
      (is (= 0 (count (cl/entities coll2)))))))

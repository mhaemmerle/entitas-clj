(ns entitas-clj.entity-test
  (:use clojure.test)
  (:require [entitas-clj.entity :as e]
            [entitas-clj.component :as c]))

(deftest create
  (let [id :foo
        entity (e/create id)]
    (is (= id (:id @entity)))
    (is (= nil (:creation-index @entity)))
    (is (= {} (:components @entity)))
    (is (= #{} (:ctypes @entity)))))

(deftest has-component-of-type
  (let [entity (e/create :foo)
        ctype :bar
        component (c/create ctype nil)
        new-entity (e/add-component entity component)]
    (is (= {ctype component} (:components @new-entity)))
    (is (= #{ctype} (:ctypes @new-entity)))
    (is (e/has-component-of-type new-entity ctype))
    (is (not (e/has-component-of-type new-entity :qux)))))

(deftest has-components-of-types
  (let [ctype1 :bar
        ctype2 :baz
        component1 (c/create ctype1 nil)
        component2 (c/create ctype2 nil)
        new-entity (-> (e/create :foo)
                       (e/add-component ,, component1)
                       (e/add-component ,, component2))]
    (is (e/has-components-of-types new-entity #{ctype1 ctype2}))
    (is (e/has-components-of-types new-entity #{ctype2 ctype1}))
    (is (not (e/has-components-of-types new-entity #{ctype2 ctype1 :qux})))))

(deftest components-of-type
  (let [ctype :bar
        component (c/create ctype nil)
        entity (e/add-component (e/create :foo) component)]
    (is (= component (e/component-of-type entity ctype)))))

(deftest contains-component
  (let [ctype :bar
        component (c/create ctype nil)
        entity (e/add-component (e/create :foo) component)]
    (is (e/contains-component entity component))
    (is (not (e/contains-component entity {:type :qux})))))

(deftest exchange-component
  (let [ctype :bar
        component (c/create ctype nil)
        new-component {:type ctype :prop :a}
        entity (e/add-component (e/create :foo) component)
        new-entity (e/exchange-component entity new-component)]
    (is (= {ctype new-component} (:components @new-entity)))))

(deftest remove-component-of-type
  (let [ctype1 :bar
        ctype2 :baz
        component1 (c/create ctype1 nil)
        component2 (c/create ctype2 nil)
        entity (e/create :foo)]
    (e/add-component entity component1)
    (is (= #{ctype1} (:ctypes @entity)))
    (e/add-component entity component2)
    (is (= #{ctype1 ctype2} (:ctypes @entity)))
    (is (= {ctype1 component1 ctype2 component2} (:components @entity)))
    (e/remove-component-of-type entity ctype1)
    (is (= {ctype2 component2} (:components @entity)))))

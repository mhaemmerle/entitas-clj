(ns entitas-clj.entity-test
  (:use clojure.test)
  (:require [entitas-clj.entity :as e]))

(deftest new
  (let [id :foo
        entity (e/new id)]
    (is (= id (:id entity)))
    (is (= nil (:creation-index entity)))
    (is (= {} (:components entity)))
    (is (= #{} (:ctypes entity)))))

(deftest has-component-of-type
  (let [entity (e/new :foo)
        ctype :bar
        component {:type ctype}
        new-entity (e/add-component entity component)]
    (is (= {ctype component} (:components new-entity)))
    (is (= #{ctype} (:ctypes new-entity)))
    (is (e/has-component-of-type new-entity ctype))
    (is (not (e/has-component-of-type new-entity :qux)))))

(deftest has-components-of-types
  (let [ctype1 :bar
        ctype2 :baz
        component1 {:type ctype1}
        component2 {:type ctype2}
        new-entity (-> (e/new :foo)
                       (e/add-component ,, component1)
                       (e/add-component ,, component2))]
    (is (e/has-components-of-types new-entity #{ctype1 ctype2}))
    (is (e/has-components-of-types new-entity #{ctype2 ctype1}))
    (is (not (e/has-components-of-types new-entity #{ctype2 ctype1 :qux})))))

(deftest components-of-type
  (let [ctype :bar
        component {:type ctype}
        entity (e/add-component (e/new :foo) component)]
    (is (= component (e/component-of-type entity ctype)))))

(deftest contains-component
  (let [ctype :bar
        component {:type ctype}
        entity (e/add-component (e/new :foo) component)]
    (is (e/contains-component entity component))
    (is (not (e/contains-component entity {:type :qux})))))

(deftest exchange-component
  (let [ctype :bar
        component {:type ctype}
        new-component {:type ctype :prop :a}
        entity (e/add-component (e/new :foo) component)
        new-entity (e/exchange-component entity new-component)]
    (is (= {ctype new-component} (:components new-entity)))))

(deftest remove-component-of-type
  (let [ctype1 :bar
        ctype2 :baz
        component1 {:type ctype1}
        component2 {:type ctype2}
        entity (-> (e/new :foo)
                   (e/add-component ,, component1)
                   (e/add-component ,, component2))
        new-entity (e/remove-component-of-type entity ctype1)]
    (is (= #{ctype1 ctype2} (:ctypes entity)))
    (is (= {ctype1 component1 ctype2 component2} (:components entity)))
    (is (= #{ctype2} (:ctypes new-entity)))
    (is (= {ctype2 component2} (:components new-entity)))))

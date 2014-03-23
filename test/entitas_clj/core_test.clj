(ns entitas-clj.core-test
  (:use clojure.test
        entitas-clj.core)
  (:require [entitas-clj.component :as c]
            [entitas-clj.entity :as e]
            [entitas-clj.repository :as r]))

(deftest contains-component
  (let [etype :foo
        entity (e/create etype)
        r0 (r/add-entity (r/create) entity)
        ctype :bar
        component (c/create ctype nil)
        r1 (add-component r0 entity component)
        expected-components {ctype {:type ctype :data nil}}]
    (is (= expected-components (:components @(get-in r1 [:entities 0]))))))

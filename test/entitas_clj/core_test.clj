(ns entitas-clj.core-test
  (:use clojure.test
        entitas-clj.core)
  (:require [entitas-clj.component :as c]
            [entitas-clj.entity :as e]
            [entitas-clj.repository :as r]))

(deftest contains-component
  (let [etype :foo
        e0 (e/create etype)
        [r0 e1] (r/add-entity (r/create) e0)
        ctype :bar
        component (c/create ctype nil)
        [r1 e2] (add-component r0 e1 component)
        expected-components {ctype {:type ctype :data nil}}]
    (is (= expected-components (:components @(get-in r1 [:entities 0]))))))

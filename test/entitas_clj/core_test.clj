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
        component (c/create ctype)
        [r1 e1] (add-component r0 e1 component)]
    (is (= (:components (get-in r1 [:entities 0])) {ctype component}))))

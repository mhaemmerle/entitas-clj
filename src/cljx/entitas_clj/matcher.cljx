(ns entitas-clj.matcher
  (:require [clojure.set :refer [subset? intersection]]))

(defn all-matching? [a b]
  (subset? a b))

(defn any-matching? [a b]
  (not (= #{} (intersection a b))))

(defn equal? [a b]
  (= a b))

(defn all-of [ctypes-a & ctypes-b]
  (all-matching? ctypes-a (set ctypes-b)))

(defn all-of-set [ctypes-a ctypes-b]
  (all-matching? ctypes-a ctypes-b))

(defn any-of [ctypes-a & ctypes-b]
  (any-matching? ctypes-a (set ctypes-b)))

(defn any-of-set [ctypes-a ctypes-b]
  (any-matching? ctypes-a ctypes-b))

(defn just [type-a ctypes-b]
  (all-matching? #{type-a} ctypes-b))

;; expects a VAR
(defn to-name [mtype]
  (let [m (meta mtype)
        mns (ns-name (:ns m))
        n (:name m)]
    (format "%s/%s" mns n)))

(defn to-key [mtype ctypes]
  (let [safe-ctypes (if (coll? ctypes) ctypes [ctypes])]
    ;; (hash (apply str mtype safe-ctypes))
    (apply str mtype safe-ctypes)
    ))

(ns day2
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [set-test is]]))

(def sample
  "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defn safe-delta [l1 l2]
  (let [delta (abs (- l1 l2))]
    (and (<= 1 delta)
         (<= delta 3))))

(defn all-increasing? [levels]
  (every? (fn [[n m]] (and (< n m) (safe-delta n m))) (partition 2 1 levels)))

(defn all-decreasing? [levels]
  (every? (fn [[n m]] (and (> n m) (safe-delta n m))) (partition 2 1 levels)))

(defn safe? [levels]
  (or (all-increasing? levels)
      (all-decreasing? levels)))

;; 7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
;; 1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
;; 9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
;; 1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
;; 8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
;; 1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.

(set-test
 safe?
 (is (safe? [7 6 4 2 1]))
 (is (not (safe? [1 2 7 8 9])))
 (is (not (safe? [9 7 6 2 1])))
 (is (not (safe? [1 3 2 4 5])))
 (is (not (safe? [8 6 4 4 1])))
 (is (safe? [1 3 6 7 9])))


(def input
  (->> "input/2"
       io/resource
       slurp
       str/split-lines
       (mapv #(mapv parse-long (str/split % #"\s+")))))

(count (filter safe? input))
;; => 334


(defn butnth
  [xs n]
  (concat (subvec xs 0 n) (subvec xs (inc n) (count xs))))

(defn droppings
  [xs]
  (for [n (range (count xs))]
    (butnth xs n)))

(droppings '[a b c d])
;; => ((b c d) (a c d) (a b d) (a b c))

(defn safeish?
  [levels]
  (some safe? (droppings levels)))

(set-test
 safeish?
 (is (safeish? [7 6 4 2 1]))
 (is (not (safeish? [1 2 7 8 9])))
 (is (not (safeish? [9 7 6 2 1])))
 (is (safeish? [1 3 2 4 5]))
 (is (safeish? [8 6 4 4 1]))
 (is (safeish? [1 3 6 7 9])))

(count (filter safeish? input))
;; => 400

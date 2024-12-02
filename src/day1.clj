(ns day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(def input
  (->> "input/1"
       io/resource
       slurp
       str/split-lines
       (mapv #(mapv parse-long (str/split % #"\s+")))))


(def l1 (mapv first input))
(def l2 (mapv second input))
(reduce +
        0
        (map (fn [x y] (abs (- x y)))
             (sort l1)
             (sort l2)))
;; => 1110981

(def h (frequencies l2))
(reduce + 0 (map (fn [n] (* n (get h n 0))) l1))
;; => 24869388

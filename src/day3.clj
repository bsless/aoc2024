(ns day3
  (:require
   [clojure.java.io :as io]))

(def sample
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def pat #"mul\((\d{1,3}),(\d{1,3})\)")

(defn calculate1
  [matches]
  (reduce +
          (map (fn [[_ n m]]
                 (* (parse-long n) (parse-long m)))
               matches)))

(calculate1 (re-seq pat sample))
;; => 161

(def input
  (->> "input/3"
       io/resource
       slurp))

(calculate1 (re-seq pat input))
;; => 157621318

(def sample2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(def pat2 #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)")

(defn sum-active
  [tokens]
  (loop [[[head n m] & tokens] tokens
         active true
         acc 0]
    (if head
      (condp = head
        "don't()" (recur tokens false acc)
        "do()" (recur tokens true acc)
        (recur tokens active (if active (+ acc (* (parse-long n) (parse-long m))) acc)))
      acc)))

(sum-active (re-seq pat2 sample2))

(sum-active (re-seq pat2 input))
;; => 79845780

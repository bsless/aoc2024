(ns day3
  (:require
   [clojure.java.io :as io]))

(def sample
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def pat #"mul\((\d{1,3}),(\d{1,3})\)")

(defn find-matches
  ([sample]
   (find-matches sample pat))
  ([sample pat]
   (let [m (re-matcher pat sample)]
     (loop [v []]
       (if (.find m)
         (recur (conj v (.group m)))
         v)))))

(def matches
  (find-matches sample))

(defn calculate1
  [matches]
  (reduce +
          (map (fn [s]
                 (let [[_ n m] (re-find pat s)]
                   (* (parse-long n) (parse-long m))))
               matches)))
(calculate1 (find-matches sample))
;; => 161

(def input
  (->> "input/3"
       io/resource
       slurp))

(calculate1 (find-matches input))
;; => 157621318

(def sample2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")
(def pat2 #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)")
(find-matches sample2 pat2)
(def tokens (find-matches input pat2))

(defn find-active-tokens
  [tokens]
  (loop [[head & tokens] tokens
         active true
         acc []]
    (if head
      (condp = head
        "don't()" (recur tokens false acc)
        "do()" (recur tokens true acc)
        (recur tokens active (if active (conj acc head) acc)))
      acc)))

(calculate1 (find-active-tokens (find-matches sample2 pat2)))
;; => 48

(calculate1 (find-active-tokens (find-matches input pat2)))
;; => 79845780

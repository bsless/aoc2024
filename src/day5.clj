(ns day5
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.set :as set]))

(def sample
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")


(defn parse
  [sample]
  (let [[-rules -out] (str/split sample #"\n\n")]
    {:rules (->> -rules
                 str/split-lines
                 (mapv (fn [s] (mapv parse-long (str/split s #"\|")))))
     :out (->> -out
               str/split-lines
               (mapv (fn [s] (mapv parse-long (str/split s #"\,")))))}))

(def parsed (parse sample))

(defn build-rules
  [parsed]
  (->> parsed :rules (reduce (fn [m [from to]] (update m from (fnil conj #{}) to)) {})))

(def rules (build-rules parsed))
;; => {47 #{13 61 29 53},
;;     97 #{75 13 61 29 47 53},
;;     75 #{13 61 29 47 53},
;;     61 #{13 29 53},
;;     29 #{13},
;;     53 #{13 29}}

(defn build-report
  [report]
  (with-meta (reduce-kv (fn [m i n] (assoc m n i)) {} report) {:report report}))

(def report (build-report [75,97,47,61,53]))

(defn rules-apply?
  [rules report]
  (every?
   true?
   (for [[k vs] rules
         :let [before (get report k)]
         :when before
         v vs
         :let [after (get report v)]
         :when after]
     (< (long before) (long after)))))

(defn solve1
  [sample]
  (reduce + 0
          (let [parsed (parse sample)
                rules (build-rules parsed)]
            (for [report (:out parsed)
                  :when (rules-apply? rules (build-report report))]
              (nth report (/ (dec (count report)) 2))))))

(def input
  (->> "input/5"
       io/resource
       slurp))

(solve1 input)
(def report [75,97,47,61,53])

(defn build-graph
  [rules report]
  (let [nodes (into #{} report)]
    (reduce (fn [m k] (assoc m k (set/intersection nodes (get rules k)))) {} report)))

(defn toposort
  [rules report]
  (let [graph (build-graph rules report)
        nodes (into #{} (map first) graph)
        visit (fn visit [node marked sorted]
                (if (marked node)
                  [marked sorted]
                  (let [[marked sorted] (reduce
                                         (fn [[marked sorted] node]
                                           (visit node marked sorted))
                                         [marked sorted]
                                         (get graph node))]
                    [(conj marked node) (conj sorted node)])))]
    (loop [unmarked nodes
           marked #{}
           sorted ()]
      (if (seq unmarked)
        (let [[marked sorted] (visit (first unmarked) marked sorted)]
          (recur (set/difference unmarked marked)
                 marked
                 sorted))
        (vec sorted)))))

(defn solve2
  [sample]
  (reduce + 0
          (let [parsed (parse sample)
                rules (build-rules parsed)]
            (for [report (:out parsed)
                  :when (not (rules-apply? rules (build-report report)))
                  :let [report (toposort rules report)]]
              (nth report (/ (dec (count report)) 2))))))

(solve2 input)

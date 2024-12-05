(ns day4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def small-sample
  "..X...
.SAMX.
.A..A.
XMAS.S
.X....")

(def sample
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def sample'
  "....XXMAS.
.SAMXMS...
...S..A...
..A.A.MS.X
XMASAMX.MM
X.....XA.A
S.S.S.S.SS
.A.A.A.A.A
..M.M.M.MM
.X.X.XMASX")

(defn build-patterns
  [sample]
  (let [width (dec (String/.indexOf sample "\n"))
        pad (format ".{%s}" width)
        lpad (format ".{%s}" (inc width))
        rpad (format ".{%s}" (dec width))
        samx (apply str (reverse "XMAS"))]
    (into
     {}
     (map (fn [xs]
            (let [p (apply str xs)]
              [p (re-pattern p)])))
     ["XMAS"
      samx
      (interpose pad "XMAS")
      (interpose pad samx)

      (interpose lpad "XMAS")
      (interpose lpad samx)
      (interpose rpad "XMAS")
      (interpose rpad samx)])))


(defn apply-patterns
  [sample patterns]
  (reduce-kv (fn [m k v]
               (assoc m k (re-seq v sample))) {} patterns))

(defn go
  [sample]
  (apply-patterns (str/replace sample "\n" "") (build-patterns sample)))


(def tiny
  "X..S
M..A
A..M
S..X")

(go small-sample)

(reduce + (map count (vals (go sample))))

(go sample')

(defn build-pattern
  [sample]
  (re-pattern
   (str/join
    "|"
    (keys (build-patterns sample)))))

(build-pattern small-sample)

(def pat (build-pattern sample))

(defn all-left-diagonals
  [sample]
  (let [width (dec (String/.indexOf sample "\n"))
        s (str/replace sample "\n" "")]
    (for [i (range (dec width))]
      (take-nth (dec width) (drop i s)))))

(defn all-right-diagonals
  [sample]
  (let [width (dec (String/.indexOf sample "\n"))
        s (str/replace sample "\n" "")]
    (for [i (range (dec width))]
      (take-nth (inc width) (drop i s)))))

(defn all-verticals
  [sample]
  (let [width (dec (String/.indexOf sample "\n"))
        s (str/replace sample "\n" "")]
    (for [i (range (dec width))]
      (take-nth width (drop i s)))))

(reduce + (map count (keep #(re-seq #"XMAS|SAMX" %)
                  (map (partial apply str)
                       (concat (str/split-lines sample')
                               (all-verticals sample')
                               (all-left-diagonals sample')
                               (all-right-diagonals sample'))))))

(map (partial apply str )(all-left-diagonals sample'))

(take-nth 9 (drop 1 (str/replace sample' "\n" "")))

(def lines (str/split-lines sample'))

(def dirs (for [x [-1 0 1]
                y [-1 0 1]
                :when (not (= x y 0))]
            [x y]))

(def r [0 1 2 3])

(defn make-paths [x y]
  (for [[dx dy] dirs
        :let [p
              (vec (for [r r
                         :let [x' (+ x (* dx r))
                               y' (+ y (* dy r))]
                         :when (and (nat-int? x') (nat-int? y'))]
                     [x' y']))]
        :when (= 4 (count p))]
    p))

(defn solve1
  [sample]
  (let [lines (str/split-lines sample)]
    (count
     (for [x (range (count lines))
           :let [line (get lines x)]
           y (range (count line))
           :let [c (get line y)]
           :when (= c \X)
           paths (make-paths x y)
           :let [view (apply str
                             (for [[x y] paths]
                               (get-in lines [x y])))]
           :when (or (= view "XMAS")
                     (= view "SAMX"))]
       view))))

(solve1 sample')

(def input
  (->> "input/4"
       io/resource
       slurp))

(solve1 input)
;; => 2370

(defn make-paths2
  [x y]
  [[[(dec x) (dec y)]
    [x y]
    [(inc x) (inc y)]]
   [[(dec x) (inc y)]
    [x y]
    [(inc x) (dec y)]]])


(defn solve2
  [sample]
  (let [lines (str/split-lines sample)]
    (count
     (for [x (range 1 (dec (count lines)))
           :let [line (get lines x)]
           y (range 1 (dec (count line)))
           :let [c (get line y)]
           :when (= c \A)
           :let [paths (make-paths2 x y)
                 views (for [path paths
                             :let [view
                                   (apply str
                                          (for [[x y] path]
                                            (get-in lines [x y])))]
                             :when (or (= view "MAS") (= view "SAM"))]
                         view)]
           :when (= 2 (count views))]
       views))))

(solve2 "M.S
.A.
M.S")

(solve2 ".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........")

(solve2 input)
;; => 1908

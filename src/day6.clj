(ns day6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(def lines (str/split-lines sample))

(defn find-start
  [lines]
  (->> lines
       (map-indexed (fn [i ^String s]
                      (let [j (.indexOf s "^")]
                        (when (nat-int? j)
                          [i j]))))
       (some identity)))

(def start (find-start lines))

(defn visit
  [visited x y]
  (let [v (get visited x #{})]
    (assoc visited x (conj v y))))

(defn out-of-bounds?
  [^long x ^long y ^long xmax ^long ymax]
  (or (>= x xmax)
      (>= y ymax)
      (neg-int? x)
      (neg-int? y)))

(defn blocked?
  [x y dx dy lines]
  (let [xmax (count lines)
        ymax (String/.length (nth lines 0))
        x (+ x dx)
        y (+ y dy)]
    (and (not (out-of-bounds? x y xmax ymax))
         (-> lines
             (nth x)
             (String/.charAt y)
             (= \#)))))

(defn count-visits
  [visited]
  (reduce-kv (fn [acc _ v] (+ acc (count v))) 0 visited))

(defn traverse
  [lines]
  (let [xmax (count lines)
        ymax (String/.length (nth lines 0))
        start (find-start lines)]
    (loop [dx -1
           dy 0
           x (nth start 0)
           y (nth start 1)
           visited {}]
      (if (out-of-bounds? x y xmax ymax)
        (count-visits visited)
        (let [visited (visit visited x y)]
          (if (blocked? x y dx dy lines)
            (cond
              (== -1 dx) (recur 0  1 x y visited)
              (==  1 dy) (recur 1  0 x y visited)
              (==  1 dx) (recur 0 -1 x y visited)
              (== -1 dy) (recur -1 0 x y visited))
            (recur dx dy (+ x dx) (+ y dy) visited)))))))

(def input
  (->> "input/6"
       io/resource
       slurp
       str/split-lines))

(dotimes [_ 10]
  (time
   (dotimes [_ 1e3]
     (traverse input))))

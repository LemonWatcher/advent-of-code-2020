(ns advent-of-code-2020.day-3
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]
            [clojure.test :refer [deftest testing is]]))

;; # Day 3: Toboggan Trajectory
;;
;; Navigation a toboggan from the top left of a world given as input
;; like:
;;
;;     ..##.......
;;     #...#...#..
;;     .#....#..#.
;;     ..#.#...#.#
;;     .#...##..#.
;;     ..#.##.....
;;     .#.#.#....#
;;     .#........#
;;     #.##...#...
;;     #...##....#
;;     .#..#...#.#
;;
;; Where . is open snow and # is a tree.

(defn load-input
  "Fetch the input from a text file."
  []
  (slurp (io/resource "input/day_3.txt")))

;; ## World

(defn world
  "The world is given as a multi-line string representing a grid. `.`
  represents open snow and `#` represents a tree. The world holds the
  current coordinates of the toboggan in its `:x` and `:y` keys."
  [terrain]
  (let [m (split-lines terrain) 
        width (.length (first m))
        height (count m)]
    {:terrain m
     :width width
     :height height
     :x 0
     :y 0}))

(defn loc
  "Return the contents of the world at the current toboggan location."
  [{:keys [terrain width x y]}]
  (let [x (mod x width)
        y (+ y (quot x width))]
    (when-let [row (get terrain y nil)]
      (.charAt row x))))

(defn tree?
  "Is there a tree at the current toboggan location?"
  [w]
  (= \# (loc w)))

(defn move
  "Move the toboggan in world `w` by the deltas `dx` and `dy.`"
  [w dx dy]
  (-> w
      (update :x + dx)
      (update :y + dy)))

(defn on-piste?
  "Is the current toboggan location on the slopes, or has it gone beyond
  the bottom?"
  [w]
  (< (:y w) (:height w)))

(defn follow-slope
  "Return a lazy sequence of updates to world `w` as the toboggan
  follows a slope with deltas `dx` and `dy`."
  [w dx dy]
  (->> w
       (iterate #(move % dx dy))
       (take-while on-piste?)))

(defn count-trees
  "Count the number of trees passed by the toboggan as it travels world
  `w` on the slope with deltas `dx` and `dy`."
  [w dx dy]
  (->> (follow-slope w dx dy)
       (filter tree?)
       count))

;; ## Testing

(def test-terrain
  "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(deftest slope-test
  (testing "test terrain"
    (let [w (world test-terrain)]
      (is (= 7 (count-trees w 3 1)))
      (is (= 336 (* (count-trees w 1 1)
                    (count-trees w 3 1)
                    (count-trees w 5 1)
                    (count-trees w 7 1)
                    (count-trees w 1 2))))))
  (testing "input"
    (let [w (world (load-input))]
      (is (= 195 (count-trees w 3 1)))
      (is (= 3772314000 (* (count-trees w 1 1)
                           (count-trees w 3 1)
                           (count-trees w 5 1)
                           (count-trees w 7 1)
                           (count-trees w 1 2)))))))

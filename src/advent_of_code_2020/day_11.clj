(ns advent-of-code-2020.day-11
  (:require [clojure.test :refer [with-test is deftest]]
            [clojure.string :refer [split]]
            [com.climate.claypoole :as cp]
            [clojure.java.io :as io]))

;; # Day 11: Seating System

(def example-layout
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")


(with-test
  (defn initial-layout
    "Read `layout` as a string into a two-dimensional grid of chars with
  the first dimension addressing the row, the second the column."
    [layout]
    (mapv vec (split layout #"\n")))

  (is (= \L (-> example-layout initial-layout (get-in [0 0]))))
  (is (= \. (-> example-layout initial-layout (get-in [0 1]))))
  (is (= \L (-> "LLL" initial-layout (get-in [0 2])))))

(with-test
  (defn seat
    "Retrieve the contents of the grid at `row,col`."
    [layout row col]
    (get-in layout [row col] nil))

  (is (= \. (-> example-layout initial-layout (seat 6 0))))
  (is (= \. (-> example-layout initial-layout (seat 6 1)))))

(with-test
  (def adjacent-seats
    "Returns a sequence containing the contents of cells adjacent to
  `row,col`."
    (memoize
     (fn [layout row col]
       (for [drow [-1 0 1]
             dcol [-1 0 1]
             :when (not= drow dcol 0)
             :let [seat (seat layout (+ row drow) (+ col dcol))]
             :when seat]
         seat))))

  (let [layout (initial-layout "#L#\nL.L\n#L#")]
    (is (= (seq "LL.") (-> layout (adjacent-seats 0 0))))
    (is (= (seq "#L#LL#L#") (-> layout (adjacent-seats 1 1))))))

(defn occupied?
  "Is this seat occupied?"
  [seat]
  (= \# seat))

(defn crowded?
  "Does this seat have four or more adjacent occupied seats?"
  [neighbours]
  (->> neighbours
       (filter occupied?)
       (take 4)
       (count)
       (= 4)))

(defn quiet?
  "Does this seat only have unoccupied adjacent seats?"
  [neighbours]
  (every? (complement occupied?) neighbours))

(defn free?
  "Is this seat empty?"
  [seat]
  (= \L seat))

(with-test
  (defn change-seat
    "Returns a vector `[row col new-value]` indicating that the seat at
  the specified `row` and `col` should be updated to `new-value`."
    [layout row col]
    (let [seat (seat layout row col)
          neighbours (adjacent-seats layout row col)]
      (cond
        (and (free? seat)
             (quiet? neighbours)) [row col \#]

        (and (occupied? seat)
             (crowded? neighbours)) [row col \L])))

  (let [quiet (initial-layout "LLL\nLLL\nLLL")
        busy (initial-layout "###\n###\n###")]
    (is (= [1 1 \#] (change-seat quiet 1 1)))
    (is (= [1 1 \L] (change-seat busy 1 1)))
    (is (nil? (change-seat busy 0 0)))))

(def example-rounds
  [example-layout
   "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"
   "#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##"
   "#.##.L#.##
#L###LL.L#
L.#.#..#..
#L##.##.L#
#.##.LL.LL
#.###L#.##
..#.#.....
#L######L#
#.LL###L.L
#.#L###.##"
   "#.#L.L#.##
#LLL#LL.L#
L.L.L..#..
#LLL.##.L#
#.LL.LL.LL
#.LL#L#.##
..L.L.....
#L#LLLL#L#
#.LLLLLL.L
#.#L#L#.##"
   "#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##"])

(def cpu-pool (cp/threadpool (cp/ncpus)))

(with-test
  (defn all-change
    "Update all seats in layout for one round."
    [layout]
    (->> (cp/upfor cpu-pool
                   [row (range 0 (count layout))
                    col (range 0 (count (first layout)))]
                   (change-seat layout row col))
         (filter some?)
         (reduce (fn [new-layout [row col change]]
                   (assoc-in new-layout [row col] change))
                 layout)))

  (dotimes [i (dec (count example-rounds))]
    (is (= (initial-layout (example-rounds (inc i)))
           (all-change (initial-layout (example-rounds i)))))))

(with-test
  (defn converge
    "Iteratively call `all-change` on layout until the result converges."
    [layout]
    (reduce (fn [prev-layout layout]
              (if (= prev-layout layout)
                (reduced layout)
                layout))
            (iterate all-change layout)))

  (is (= (initial-layout (last example-rounds))
         (converge (initial-layout example-layout)))))

(defn load-input
  "Get the input layout."
  []
  (slurp (io/resource "input/day_11.txt")))

(with-test
  (defn eventual-occupants
    [layout]
    (->> layout
         (initial-layout)
         (converge)
         (flatten)
         (filter occupied?)
         count))

  (is (= 37 (eventual-occupants example-layout)))
  (is (= 0 (eventual-occupants (load-input)))))

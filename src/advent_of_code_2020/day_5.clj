(ns advent-of-code-2020.day-5
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [clojure.string :refer [split-lines]]))

;; # Day 5: Binary Boarding
;;
;; Seats on a plane are identified with a binary partitioning scheme
;; like the following:
;;
;;     FBFBBFFRLR
;;
;; Where F means the front of the partition, B means back, L means
;; left, R means right, in a plane with 128 rows and 8 columns.

;; ## Parsing

(def bits
  "Trivially, `F` and `L` are zeroes, `B` and `R` are ones."
  {\F 0
   \L 0
   \B 1
   \R 1})

(defn parse-seat
  "Seats are just binary numbers, so translate into a long."
  [seat]
  (loop [id 0
         [bit & seat] seat]
    (if-not bit
      id
      (recur (bit-or
              (bit-shift-left id 1)
              (bits bit))
             seat))))

(defn seats
  "Load and parse all the seats."
  []
  (->> "input/day_5.txt"
       io/resource
       slurp
       split-lines
       (map parse-seat)))

;; ## Puzzle

(defn highest-seat-id
  "Find the highest seat ID."
  [seats]
  (reduce max seats))

(defn missing-seat
  "Find the missing seat in an otherwise empty plane."
  [seats]
  (reduce (fn [prev seat]
            (let [expected-seat (inc prev)]
              (if (and prev (not= seat expected-seat))
                (reduced expected-seat)
                seat)))
          (sort seats)))

;; ## Testing

(deftest parse-seat-test
  (is (= 357 (parse-seat "FBFBBFFRLR")))
  (is (= 567 (parse-seat "BFFFBBFRRR")))
  (is (= 119 (parse-seat "FFFBBBFRRR")))
  (is (= 820 (parse-seat "BBFFBBFRLL"))))

(deftest highest-seat-id-test
  (is (= 826 (highest-seat-id (seats)))))

(deftest missing-seat-test
  (is (= 678 (missing-seat (seats)))))

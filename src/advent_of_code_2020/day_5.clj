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
              (if (not= seat expected-seat)
                (reduced expected-seat)
                seat)))
          (sort seats)))

(defn missing-seat-linear
  "Find the missing seat without the initial sort."
  [seats]
  (let [reservations (boolean-array (* 8 128) false)]
    (doseq [seat seats]
      (aset reservations seat true))
    (loop [i 0
           prev false]
      (let [reserved? (aget reservations i)]
        (if (and prev (not reserved?))
          i
          (recur (unchecked-inc-int i) reserved?))))))

;; ## Testing

(deftest parse-seat-test
  (is (= 357 (parse-seat "FBFBBFFRLR")))
  (is (= 567 (parse-seat "BFFFBBFRRR")))
  (is (= 119 (parse-seat "FFFBBBFRRR")))
  (is (= 820 (parse-seat "BBFFBBFRLL"))))

(deftest highest-seat-id-test
  (is (= 826 (highest-seat-id (seats)))))

(deftest missing-seat-test
  (is (= 678 (missing-seat (seats))))
  (is (= 678 (missing-seat-linear (seats)))))

;; ## Benchmarks
;;
;; Showing that the linear approach to the missing seat is
;; substantially faster.

(comment
  (let [seats (seats)]
    ;; Evaluation count : 498 in 6 samples of 83 calls.
    ;; Execution time mean : 1.194823 ms
    ;; Execution time std-deviation : 32.001224 µs
    ;; Execution time lower quantile : 1.175012 ms ( 2.5%)
    ;; Execution time upper quantile : 1.250110 ms (97.5%)
    ;; Overhead used : 31.740990 ns

    ;; Found 1 outliers in 6 samples (16.6667 %)
    ;; low-severe	 1 (16.6667 %)
    ;; Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
    (quick-bench (missing-seat seats))

    ;; Evaluation count : 10302 in 6 samples of 1717 calls.
    ;; Execution time mean : 61.160273 µs
    ;; Execution time std-deviation : 3.010229 µs
    ;; Execution time lower quantile : 58.867294 µs ( 2.5%)
    ;; Execution time upper quantile : 66.168131 µs (97.5%)
    ;; Overhead used : 31.740990 ns

    ;; Found 1 outliers in 6 samples (16.6667 %)
    ;; low-severe	 1 (16.6667 %)
    ;; Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
    (quick-bench (missing-seat-linear seats))))

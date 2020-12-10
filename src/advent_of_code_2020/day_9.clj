(ns advent-of-code-2020.day-9
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [clojure.string :refer [split split-lines]]
            [clojure.math.combinatorics :refer [combinations]]))

;; # Day 9: Encoding Error
;;
;; With a preamble of N numbers, validate that each subsequent number
;; is a sum of two of the previous N numbers.

(defn load-input
  "Get the list of numbers."
  []
  (->> "input/day_9.txt"
       io/resource
       slurp
       split-lines
       (map read-string)))

(defn example-input
  "Generate some sample data containing the numbers up to
  `preamble-length` shuffled, with `data` appended at the end to
  validate various scenarios."
  [preamble-length & data]
  (concat (shuffle (range 1 (inc preamble-length))) data))

(defn valid?
  "Slow implementation to check whether `x` is the sum of any pair of
  numbers in `preamble`."
  [preamble x]
  (some #{x} (map (partial apply +) (combinations preamble 2))))

(defn check-data
  "Find a number in `data` that violates the rule that it must be the
  sum of two previous numbers within a window of
  `preamble-length`. Returns `nil` if data is valid."
  [preamble-length data]
  (let [[preamble data] (split-at preamble-length data)]
    (loop [preamble preamble
           [x & data] data]
      (when x
        (if-not (valid? preamble x)
          x
          (recur (concat (rest preamble) [x]) data))))))

(def test-input [35 20 15 25 47 40 62 55 65 95 102 117 150 182 127 219 299 277 309 576])

(deftest check-data-tests
  (is (nil? (check-data 25 (example-input 25 26))))
  (is (nil? (check-data 25 (example-input 25 49))))
  (is (= 100 (check-data 25 (example-input 25 100))))
  (is (= 50 (check-data 25 (example-input 25 50))))
  (is (nil? (check-data 25 (example-input 25 45 26))))
  (let [input (concat [20] (range 1 20) (range 21 26) [45])]
    (is (= 65 (check-data 25 (concat input [65]))))
    (is (nil? (check-data 25 (concat input [64]))))
    (is (nil? (check-data 25 (concat input [66])))))
  (is (= 127 (check-data 5 test-input)))
  (is (= 466456641 (check-data 25 (load-input)))))

(defn contiguous-sum
  "Find a range of contiguous numbers in `input` that sum to `target`,
  and return the sum of the minimum and maximum in that range."
  [target input]
  (first (for [n (range 2 (count input))
               partition (partition n 1 input)
               :when (= target (apply + partition))]
           (+ (apply min partition) (apply max partition)))))

(deftest contiguous-sum-tests
  (is (= 62 (contiguous-sum 127 test-input)))
  (let [input (load-input)]
    (is (= 55732936 (contiguous-sum (check-data 25 input) input)))))

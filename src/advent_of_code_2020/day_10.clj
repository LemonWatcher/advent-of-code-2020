(ns advent-of-code-2020.day-10
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :refer [split-lines]]
            [clojure.java.io :as io]))

;; # Day 10: Adapter Array
;;
;; Adapters have different voltanges and can be chained
;; together. Adapters can only cope with differences between 1 and 3
;; volts. Initial output voltage is 0, final output voltage is max +
;; 3.

(def example-adapters
  [16 10 15 5 1 11 7 19 6 12 4])

(def example-adapters-2
  [28 33 18 42 31 14 46 20 48 47 24 23 49 45 19 38 39 11 1 32 25 35 8 17 7 9 4 2 34 10 3])

(def output-voltage 0)

(defn device-voltage
  "Final device output is 3 plus the max of your adapters."
  [adapters]
  (+ 3 (reduce max adapters)))

(defn voltage-differences
  "You have to use all `adapters`, in which case the only solution is
  just a sorted list."
  [adapters]
  (->> (conj adapters output-voltage (device-voltage adapters))
       sort
       (partition 2 1)
       (map #(- (second %) (first %)))))

(deftest voltage-differences-tests
  (is (= {1 7 3 5} (frequencies (voltage-differences example-adapters))))
  (is (= {1 22 3 10} (frequencies (voltage-differences example-adapters-2)))))

(defn product
  "Part one answer relies on the product of two counts from `frequencies`."
  [diffs]
  (apply * (vals (select-keys (frequencies diffs) [1 3]))))

(defn load-input
  "Get the list of numbers."
  []
  (->> "input/day_10.txt"
       io/resource
       slurp
       split-lines
       (map read-string)))

(deftest product-differences-tests
  (is (= 2070 (-> (load-input) voltage-differences product))))

;; ## Part 2
;;
;; Count the number of possible chains of adapters between the input
;; and your device, bearing in mind adapters can only support 1 to 3
;; volt differences.
;;
;; So this is either extremely dumb or showing off. The intuition here
;; is very simple: you can't avoid traversing differences of 3 volts,
;; so you're not actually interested in them because there's no
;; branching. All you're interested in are runs of 1 volt differences
;; (input data only seems to have 1 and 3, fortunately).
;;
;; Turns out there's a very simple trick here, which is to use [this
;; sequence](http://oeis.org/A050231).
;;
;; That is, the number of n-tosses having a run of 3 or more heads -
;; heads in this case are gaps in the adapter chain of n
;; adapters. These runs are invalid so if we subtract them from 2^n
;; (which would be the world in which all adapters could plug into all
;; others), we get the right answer.

(defonce tribonacci
  (memoize (fn [n]
             (case n
               0 0
               1 0
               2 1
               3 3
               (- (* 3 (tribonacci (- n 1)))
                  (tribonacci (- n 2))
                  (tribonacci (- n 3))
                  (* 2 (tribonacci (- n 4))))))))

(defn count-chains
  "Counts the number of valid chains in `adapters`."
  [adapters]
  (->> adapters
       voltage-differences
       (partition-by identity) ;; get runs of the same difference
       (filter (comp #{1} first)) ;; we only care about 1s
       (map count) ;; count the length of the runs
       (map #(- (Math/pow 2 (dec %)) ;; possible combinations
                (tribonacci (max 0 (- % 2))))) ;; invalid combinations
       (map biginteger) ;; result overflows int
       (reduce *)))

(deftest count-chains-test
  (is (= 1 (count-chains [1])))
  (is (= 2 (count-chains [1 2])))
  (is (= 4 (count-chains [1 2 3])))
  (is (= 7 (count-chains [1 2 3 4])))
  (is (= 13 (count-chains [1 2 3 4 5])))
  (is (= 24 (count-chains [1 2 3 4 5 6])))
  (is (= 44 (count-chains [1 2 3 4 5 6 7])))
  (is (= 81 (count-chains [1 2 3 4 5 6 7 8])))
  (is (= 1 (count-chains [1 4 5])))
  (is (= 2 (count-chains [1 2 5])))
  (is (= 1 (count-chains [3 4 7])))
  (is (= 4 (count-chains [1 2 5 6 7])))
  (is (= 16 (count-chains [1 2 3 5 6 7 8])))
  (is (= 8 (count-chains example-adapters)))
  (is (= 19208 (count-chains example-adapters-2)))
  (is (= 24179327893504 (count-chains (load-input)))))

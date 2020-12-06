(ns advent-of-code-2020.day-6
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [clojure.string :refer [split split-lines]]
            [clojure.set :refer [union intersection]]))

;; # Day 6: Custom Customs
;;
;; Fine yes/no answers given by all users in a group, from input like:
;;
;;     abc
;;     
;;     a
;;     b
;;     c
;;     
;;     ab
;;     ac
;;     
;;     a
;;     a
;;     a
;;     a
;;     
;;     b
;;
;; Where each character represents one of 26 questions that were
;; answered yes by a user. Each line is one user, each group is
;; separated by a blank line.

;; ## Input

(defn load-input
  "Load today's data."
  []
  (-> "input/day_6.txt"
      io/resource
      slurp))

(defn responses
  "Split into groups, and convert each individual response within a
  group into a set."
  [input]
  (->> (split input #"\n{2,}")
       (map split-lines)
       (map (partial map set))))

;; ## Puzzle

(def all-yesses
  "Counting all answers to which anybody in a group has answered yes is
  just the union of their answers."
  union)

(defn count-answers
  "Sum the totals of each group depending on the provided `reducer` to
  combine group answers."
  [reducer input]
  (->> input
       responses
       (map (partial reduce reducer))
       (map count)
       (reduce +)))

(def example-group
  "abcx
abcy
abcz")

(def example-groups
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(deftest all-yesses-test
  (is (= 6 (count-answers all-yesses example-group)))
  (is (= 11 (count-answers all-yesses example-groups)))
  (is (= 6778 (count-answers all-yesses (load-input)))))

;; ## Part 2

(def shared-yesses
  "Counting answers to which all members of a group answered yes is just
  the intersection of their answers."
  intersection)

(def example-aggreements
  "abc

a
b
c

ab
ac

a
a
a
a

b")

(deftest shared-yesses-test
  (is (= 6 (count-answers shared-yesses example-aggreements)))
  (is (= 3406 (count-answers shared-yesses (load-input)))))

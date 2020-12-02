(ns advent-of-code-2020.day-2
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :refer [split-lines]]
            [clojure.test :refer [deftest testing is]]))

;; # Day 2: Password Philosophy
;;
;; Read lines of password policies and passwords, and check i f the
;; password matches the policy. Each line is of the form:
;;
;;     x-y c: password
;;
;; Where x is the minimum required, and y the maximum allowed, number
;; of instances of character c in password.

(defn load-input
  "Fetch the input from a text file."
  []
  (slurp (io/resource "input/day_2.txt")))

;; ## Parsing
;;
;; We'll use Spec to parse the various parts of each line of
;; input. The rules here are a little loose and wouldn't work if we
;; wanted to gen up random input but we're not interested in that
;; right now.

(s/def ::bound (s/+ (set "0123456789")))

(s/def ::separator (s/+ (set "-: ")))

(s/def ::password (s/+ (set "abcdefghijklmnopqrstuvwxyz")))

(s/def ::password-line
  (s/cat
   :min ::bound
   :sep ::separator
   :max ::bound
   :sep ::separator
   :char char?
   :sep ::separator
   :password ::password))

(defn coerce-int
  "Coerce a sequence of digit characters in `chars` into an `Integer`."
  [chars]
  (Integer/parseInt (apply str chars)))

(defn parse-line
  "Parse a single line of input in `line` into a map holding the `:min`,
  `:max`, `:char` and `:password`."
  [line]
  (let [{:keys [min max char password]} (s/conform ::password-line (seq line))]
    {:min (coerce-int min)
     :max (coerce-int max)
     :char char
     :password password ;; don't bother coercing to a string
     }))

(defn matches-rule?
  "Check if the password matches the associated rule."
  [{:keys [min max char password]}]
  (let [freqs (frequencies password)]
    (<= min (freqs char 0) max)))

(defn valid-line?
  "Does the line contain a valid password for its rule?"
  [line]
  (matches-rule? (parse-line line)))

(defn count-valid-passwords
  "Counts the number of valid passwords in the input."
  []
  (->> (load-input)
       (split-lines)
       (filter valid-line?)
       (count)))

(deftest valid-line-tests
  (is (valid-line? "1-3 a: abcde"))
  (is (not (valid-line? "1-3 b: cdefg")))
  (is (valid-line? "2-9 c: ccccccccc")))

(deftest count-valid-passwords-test
  (is (= 628 (count-valid-passwords))))

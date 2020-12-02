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
;;     a-b c: password
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
   :a ::bound
   :sep ::separator
   :b ::bound
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
  (let [{:keys [a b char password]} (s/conform ::password-line (seq line))]
    {:a (coerce-int a)
     :b (coerce-int b)
     :char char
     :password password ;; don't bother coercing to a string
     }))

(defn matches-sled-rental-policy?
  "Check if the password matches the sled rental policy."
  [{:keys [a b char password]}]
  (let [freqs (frequencies password)]
    (<= a (freqs char 0) b)))

(defn matches-official-toboggan-corporate-policy?
  "Check if the password matches Official Toboggan Corporate policy."
  [{:keys [a b char password]}]
  (let [a? (= char (nth password (dec a)))
        b? (= char (nth password (dec b)))]
    (if a? (not b?) b?)))

(defn valid-line?
  "Does the line contain a valid password for its rule?"
  [policy line]
  (policy (parse-line line)))

(defn count-valid-passwords
  "Counts the number of valid passwords in the input."
  [policy]
  (->> (load-input)
       (split-lines)
       (filter (partial valid-line? policy))
       (count)))

(deftest valid-line-tests
  (testing "sled rental policy"
    (is (valid-line? matches-sled-rental-policy? "1-3 a: abcde"))
    (is (not (valid-line? matches-sled-rental-policy? "1-3 b: cdefg")))
    (is (valid-line? matches-sled-rental-policy? "2-9 c: ccccccccc")))
  (testing "official toboggan corporate policy"
    (is (valid-line? matches-official-toboggan-corporate-policy? "1-3 a: abcde"))
    (is (not (valid-line? matches-official-toboggan-corporate-policy? "1-3 b: cdefg")))
    (is (not (valid-line? matches-official-toboggan-corporate-policy? "2-9 c: ccccccccc")))))

(deftest count-valid-passwords-test
  (is (= 628 (count-valid-passwords matches-sled-rental-policy?)))
  (is (= 705 (count-valid-passwords matches-official-toboggan-corporate-policy?))))

(ns advent-of-code-2020.day-4
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split]]
            [instaparse.core :as insta]
            [clojure.spec.alpha :as s]
            [clojure.test :refer [deftest testing is]]))

;; # Day 4: Passport Processing
;;
;; Validate passports with the following fields:
;;
;; - byr (Birth Year)
;; - iyr (Issue Year)
;; - eyr (Expiration Year)
;; - hgt (Height)
;; - hcl (Hair Color)
;; - ecl (Eye Color)
;; - pid (Passport ID)
;; - cid (Country ID)
;;
;; In part one of the puzzle, we write rules to validate the presence
;; of all keys except `cid`. In part two we more strictly validate the
;; contents of these keys.

(def example-input
  "Example of scanned passport data. Each passport is a series of key
  value pairs separated by spaces or newlines. Records are separated
  by blank lines."
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(def invalid-passports
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(def valid-passports
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

;; ## Parsing
;;
;; Time to break out the big guns. We'll define a grammar in
;; Instaparse instead of trying to do it all in spec this time.

(def parse
  "Do the lowest common denominator for parsing both parts of the task."
  (insta/parser
   "passport = field (<field-separator> field)* <field-separator>?
    <field-separator> = #'\\s'
    <field> = byr | iyr | eyr | hgt | hcl | ecl | pid | cid
    byr = <'byr:'> year
    iyr = <'iyr:'> year
    eyr = <'eyr:'> year
    hgt = <'hgt:'> height
    hcl = <'hcl:'> color
    ecl = <'ecl:'> color
    pid = <'pid:'> string
    cid = <'cid:'> id
    <year> = integer
    height = integer unit?
    <unit> = 'cm' | 'in'
    <color> = hex-color | color-name
    <id> = #'[0-9]+'
    <hex-color> = <'#'>? hexadecimal
    <color-name> = string
    <string> = #'\\S+'
    integer = #'[1-9][0-9]*'
    <hexadecimal> = #'[0-9a-f]+'"
   :partial true))

(defn transform
  "Do a little cleanup on the parse tree to make it nicer to validate
  later."
  [parse-tree]
  (insta/transform
   {:integer read-string
    :height vector
    :passport (comp (partial into {}) vector)}
   parse-tree))

;; ## Validation
;;
;; We use two sets of specs, one loose for part on of the task, which
;; just validates the presence of the right keys, another strict for
;; the second part.

(s/def :loose/passport (s/keys :req-un [:loose/byr :loose/iyr :loose/eyr :loose/hgt :loose/hcl :loose/ecl :loose/pid]
                               :opt-un [:loose/cid]))

(s/def :strict/byr (s/int-in 1920 2003))
(s/def :strict/iyr (s/int-in 2010 2021))
(s/def :strict/eyr (s/int-in 2020 2031))
(s/def ::cm (s/int-in 150 194))
(s/def ::in (s/int-in 59 77))
(s/def :strict/hgt (s/or :cm (s/tuple ::cm #{"cm"})
                         :in (s/tuple ::in #{"in"})))
(s/def :strict/pid (fn [s] (re-matches #"[0-9]{9}" s)))
(s/def :strict/hcl (fn [s] (re-matches #"#[0-9a-f]{6}" s)))
(s/def :strict/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(s/def :strict/passport (s/keys :req-un [:strict/byr :strict/iyr :strict/eyr :strict/hgt :strict/hcl :strict/ecl :strict/pid]
                                :opt-un [:strict/cid]))

(defn valid-passport?
  "Is the parsed `passport` valid given the rules defined by `spec`?"
  [spec passport]
  (s/valid? spec passport))

(defn count-valid-passports
  "How many valid passports are there in `input` as defined by `spec`?"
  [spec input]
  (->> (split input #"\n{2,}")
       (map parse)
       (map transform)
       (filter (partial valid-passport? spec))
       count))

;; ## Input

(defn load-input
  "Fetch the input from a text file."
  []
  (slurp (io/resource "input/day_4.txt")))

;; ## Testing

(deftest count-valid-passports-test
  (testing "loose parsing"
    (is (= 2 (count-valid-passports :loose/passport example-input)))
    (is (= 219 (count-valid-passports :loose/passport (load-input)))))
  (testing "strict parsing"
    (is (= 0 (count-valid-passports :strict/passport invalid-passports)))
    (is (= 4 (count-valid-passports :strict/passport valid-passports)))
    (is (= 127 (count-valid-passports :strict/passport (load-input))))))

(deftest spec-tests
  (is (s/valid? :strict/byr 2002))
  (is (not (s/valid? :strict/byr 2003)))
  (is (s/valid? :strict/hgt [60 "in"]))
  (is (s/valid? :strict/hgt [190 "cm"]))
  (is (not (s/valid? :strict/hgt [190 "in"])))
  (is (not (s/valid? :strict/hgt [190])))
  (is (s/valid? :strict/hcl "#123abc"))
  (is (not (s/valid? :strict/hcl "#123abz")))
  (is (not (s/valid? :strict/hcl "123abc")))
  (is (s/valid? :strict/ecl "brn"))
  (is (not (s/valid? :strict/ecl "wat")))
  (is (s/valid? :strict/pid "000000001"))
  (is (not (s/valid? :strict/pid "0123456789"))))

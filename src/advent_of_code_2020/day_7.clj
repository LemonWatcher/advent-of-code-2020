(ns advent-of-code-2020.day-7
  (:require [clojure.test :refer [deftest testing is]]
            [instaparse.core :as insta]
            [clojure.string :refer [join]]
            [clojure.java.io :as io]
            [clojure.set :refer [union]]))

;; # Day 7: Handy Haversacks
;;
;; Due to recent aviation regulations, many rules (your puzzle input)
;; are being enforced about bags and their contents; bags must be
;; color-coded and must contain specific quantities of other
;; color-coded bags. Apparently, nobody responsible for these
;; regulations considered how long they would take to enforce!

(def example-rules
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

;; ## Parsing
;;
;; Use Instaparse again to make things explicit.

(def parse
  "Parse rules into an intermediate format."
  (insta/parser
   "rules = bag+
    bag = design <'bags contain'> (contains | empty)+ <'.'>
    design = #'[a-z]+' #'[a-z]+'
    contains = count design <'bag' 's'?>
    empty = <'no other bags'>
    count = #'[0-9]+'"
   :auto-whitespace :comma))

(defn transform
  "Transform a parse tree into a map of the form:

      :vibrant-plum {:containers #{:shiny-gold}, :contains {:faded-blue 5, :dotted-black 6}}"
  [parse-tree]
  (insta/transform
   {:count read-string
    :design (comp keyword (partial join "-") vector)
    :empty (constantly nil)
    :contains vector
    ;; Denormalise rules so we can process them in one sequence.
    :bag (fn [bag & contents] (map (partial cons bag) contents))
    ;; Store each bags `:contains` and `:containers` relationships.
    :rules (fn [& bags]
             (reduce (fn [ret [container count contained]]
                       (if-not contained
                         ret
                         (-> ret
                             (update-in [container :contains] merge {contained count})
                             (update-in [contained :containers] (fnil conj #{}) container))))
                     {}
                     (apply concat bags)))}
   parse-tree))

(defn rules
  "Load the rules in `input`."
  [input]
  (-> input parse transform))

;; ## Containment

(defn containers
  "Returns the list of possible containers (both direct and their
  ancestors) for a certain `design` of bag under the specified
  `rules`."
  [rules design]
  (let [bags (-> rules design :containers)]
    (->> bags
         (map (partial containers rules))
         (reduce union bags))))

(defn load-input
  "Load today's input."
  []
  (-> "input/day_7.txt"
      io/resource
      slurp))

(deftest containers-tests
  (is (= #{:bright-white
           :muted-yellow
           :dark-orange
           :light-red}
         (containers (rules example-rules) :shiny-gold)))
  (is (= 222 (count (containers (rules (load-input)) :shiny-gold)))))

;; ## Contents

(defn count-contents
  "Recursively count the bags contained by `design` given the specified
  `rules`, and the bags contained by those bags etc."
  [rules design]
  (->> rules
       design
       :contains
       (map (fn [[contents count]] (+ count (* count (count-contents rules contents)))))
       (apply +)))

(deftest contents-test
  (let [rules (rules example-rules)]
    (is (= 0 (count-contents rules :faded-blue)))
    (is (= 0 (count-contents rules :dotted-black)))
    (is (= 11 (count-contents rules :vibrant-plum)))
    (is (= 7 (count-contents rules :dark-olive))))
  (is (= 13264 (count-contents (rules (load-input)) :shiny-gold))))

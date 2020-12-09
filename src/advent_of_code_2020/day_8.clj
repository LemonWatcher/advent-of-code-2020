(ns advent-of-code-2020.day-8
  (:refer-clojure :exclude [compile])
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [loom.graph :as g]
            [loom.alg :as ga]))

;; # Day 8: Handheld Halting
;;
;; Implement a simple computer with opcodes with a single argument:
;; `nop` (no op), `acc` to add the argument to the accumlator, and
;; `jmp` which jumps to an offset defined in arg.
;;
;; We could implement code to interpret the opcodes and update the
;; computer's state at every instructions, but it's actually much
;; simpler just to model the computer's program counter as a traversal
;; through a graph, with the accumulator being the weights on edges
;; originating at `acc` nodes.

(def example-code
  "Simple example program with an infinite loop."
  "nop +0
  acc +1
  jmp +4
  acc +3
  jmp -3
  acc -99
  acc +1
  jmp -4
  acc +6")

(defn instructions
  "Parse the code into a format like:

      [[:nop 0]
       [:acc 1]]"
  [code]
  (vec (for [[_ op arg] (re-seq #"(nop|acc|jmp) ([+-]\d+)" code)]
         [(keyword op) (read-string arg)])))

(def debug-weight
  "Use this value as an artificially high weight to ensure that only one
  edge added during debugging is ever followed."
  Integer/MAX_VALUE)

(defn compile
  "Compile the vector of instructions in `code` into a graph, connecting
  each line with its logical next line, and adding `:acc` arguments as
  edge weights."
  [code & {:keys [debug] :or {debug false}}]
  (apply g/weighted-digraph
         (map-indexed
          (fn [i [op arg]]
            (case op
              ;; When `debug` is `true` we add potential new edges to
              ;; turn `:nop` operators into `:jmp`s and vice-versa.
              :nop (if debug
                     {i (hash-map (+ i arg) debug-weight (inc i) 0 )}
                     [i (inc i) 0])
              :acc [i (inc i) arg]
              :jmp (if debug
                     {i (hash-map (inc i) debug-weight (+ i arg) 0)}
                     [i (+ i arg) 0])))
          (instructions code))))

(defn run
  "Run the `program` and return its accumulator, either after program
  termination, or after meeting an infinite loop."
  [program]
  ;; Enumerate nodes differently depending on whether the program
  ;; is complete or not.
  (->> (if (ga/connected? program)
         (ga/shortest-path program 0 (dec (count (g/nodes program))))
         (ga/bf-traverse program 0))
       (partition 2 1)
       (keep (partial apply g/weight program))
       ;; We want to follow debug edges but not count their weights.
       (remove #{debug-weight})
       (reduce +)))

(defn load-input
  "Load today's data."
  []
  (-> "input/day_8.txt"
      io/resource
      slurp))

(deftest run-test
  (is (= 5 (-> example-code compile run)))
  (is (= 1930 (-> (load-input) compile run)))
  (is (= 8 (-> example-code (compile :debug true) run)))
  (is (= 1688 (-> (load-input) (compile :debug true) run))))

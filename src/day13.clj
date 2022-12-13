(ns day13
  (:require [instaparse.core :as insta]
            [clojure.edn :as edn]))

(def test-input
  "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")


(def parser
  (insta/parser
   "pairs = pair*
    pair = list list
    list = <'['> (list | num)* <']'>
    num = #'\\d+'
    "
   :auto-whitespace :comma))

(defn parse [s]
  (->> s
       parser
       (insta/transform
        {:num edn/read-string
         :list vector
         :pair vector
         :pairs vector})))

(defn ordered? [a b]
  (cond
    (and (int? a) (int? b)) (if (= a b) :dunno (< a b))
    (int? a) (ordered? [a] b)
    (int? b) (ordered? a [b])
    (and (empty? a) (empty? b)) :dunno
    (empty? a) true
    (empty? b) false
    :else
    (let [o? (ordered? (first a) (first b))]
      (case o?
        :dunno (ordered? (rest a) (rest b))
        o?))))

(defn pair-comparator [a b]
  ({:dunno 0
    true -1
    false 1} (ordered? a b)))

(def sum (partial reduce + 0))

(defn part1 [s]
  (->> s
       parse
       (map #(apply ordered? %))
       (keep-indexed #(when %2 (inc %1)))
       sum))

(def dividers #{[[2]] [[6]]})

(defn part2 [s]
  (->> s
       parse
       (apply concat dividers)
       (sort pair-comparator)
       (keep-indexed #(when (dividers %2) (inc %1)))
       (apply *)))

(defn input []
  (slurp "src/day13_input.txt"))

(defn solve1 []
  (part1 (input)))

(defn solve2 []
  (part2 (input)))

(comment

  (parse test-input)
  (part1 test-input)
  (solve1)
  (part2 test-input)
  (solve2)

  nil)

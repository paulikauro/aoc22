(ns day1
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def test-input
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")


(defn parse [input]
  (as-> input x
    (str/split x #"\n\n")
    (map #(as-> % y (str/split y #"\s+") (map edn/read-string y)) x)))

(def sum (partial reduce + 0))

(defn solve []
  (->> "src/day1_input.txt"
       slurp parse
       (map sum)
       (apply max)))

(defn solve2 []
  (->> "src/day1_input.txt"
       slurp parse
       (map sum)
       sort
       (take-last 3)
       sum))

(comment
  (parse test-input)
  (solve)
  (solve2)
  nil)

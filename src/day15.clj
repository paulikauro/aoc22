(ns day15
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :refer [union intersection]]))

(def test-input
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")

(def allowed-chars (set "0123456789- "))

(defn parse [s]
  (as-> s x
    (filter allowed-chars x)
    (apply str x)
    (str/split x #"\s+")
    (rest x)
    (map edn/read-string x)
    (partition 2 x)
    (map vec x)
    (partition 2 x)
    (map vec x)))

(defn distance [sx sy bx by]
  (+ (abs (- sx bx)) (abs (- sy by))))

(defn process-pair [y [[sx sy] [bx by]]]
  (let [dist (distance sx sy bx by)
        c (- dist (abs (- sy y)))]
    (when (>= c 0)
      (set (range (- sx c) (+ sx c 1))))))

(defn solve1 [s y]
  (let [ss (parse s)
        beacons (map second ss)
        k (count (reduce #(union %1 (process-pair y %2)) nil ss))
        bs (count (set (filter (fn [[_ by]] (= by y)) beacons)))]
    (- k bs)))

(defn range' [a b]
  (let [step (cond
               (< a b) 1
               (= a b) 1
               (> a b) -1)]
    (range a (+ b step) step)))

(defn point-range [[x1 y1] [x2 y2]]
  (map vector (range' x1 x2) (range' y1 y2)))

(defn borders [[[sx sy] dist]]
  (let [d (inc dist)
        p1 [sx (- sy d)]
        p2 [(+ sx d) sy]
        p3 [sx (+ sy d)]
        p4 [(- sx d) sy]]
    (concat
     (point-range p1 p2)
     (point-range p2 p3)
     (point-range p3 p4)
     (point-range p4 p1))))

(defn not-in? [[[sx sy] d] [x y]]
  (> (distance sx sy x y) d))

(defn outside-all? [ss p]
  (every? #(not-in? % p) ss))

(defn solve2 [s x-max y-max]
  (let [ss (map (fn [[[sx sy] [bx by]]] [[sx sy] (distance sx sy bx by)]) (parse s))
        valid? (fn [[x y]] (and (>= x 0) (>= y 0)
                                (<= x x-max) (<= y y-max)))
        points (filter valid? (mapcat borders ss))
        ans (filter (partial outside-all? ss) points)
        ;;_ (prn ans)
        [x y] (first ans)]
    (+ (* x 4000000) y)))

(defn input []
  (slurp "src/day15_input.txt"))

(comment
  (parse test-input)
  (solve1 test-input 10)
  (time (solve1 (input) 2000000))
  (borders [[8 7] [2 10]])
  (solve2 test-input 20 20)
  (solve2 (input) 4000000 4000000)
  nil)

(ns day12
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str]))

(def test-input
  "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn create-graph [lines width height]
  (let [fix-value #(case % \S \a \E \z %)
        height-at (fn [[x y]] (-> lines (nth y) (nth x) fix-value int))
        neighbors
        (fn [[x y]]
          [[(dec x) y]
           [(inc x) y]
           [x (dec y)]
           [x (inc y)]])
        exists? (fn [[x y]] (and (>= x 0) (>= y 0) (< x width) (< y height)))
        can-go? (fn [from to] (<= (height-at to) (inc (height-at from))))]
    (into {}
          (for [x (range width)
                y (range height)]
            [[x y] (vec (for [neighbor (neighbors [x y])
                              :when (and (exists? neighbor) (can-go? [x y] neighbor))]
                          neighbor))]))))

(def indexed (partial map-indexed vector))

(def inf Double/POSITIVE_INFINITY)

(defn parse [s]
  (let [lines (str/split s #"\n")
        height (count lines)
        width (count (nth lines 0))
        cleaned (str/replace s "\n" "")
        index-to-coord #(vector (mod % width) (quot % width))
        find-one #(-> cleaned (str/index-of %) index-to-coord)]
    {:width width
     :height height
     :start (find-one \S)
     :end (find-one \E)
     :all-as (for [[i c] (indexed cleaned)
                   :when (#{\S \a} c)]
               (index-to-coord i))
     :graph (create-graph lines width height)}))

(defn find-paths [graph start]
  (loop [q (priority-map start 0)
         dist {start 0}
         seen #{}]
    (if (empty? q)
      dist
      (let [node (first (peek q))
            newq (pop q)]
        (if (seen node)
          (recur newq dist seen)
          (let [new-cost (inc (dist node))
                updates (for [neighbor (graph node)
                              :let [old-cost (get dist neighbor inf)]
                              :when (< new-cost old-cost)]
                          [neighbor new-cost])]
            (recur (into newq updates) (into dist updates) (conj seen node))))))))

(defn part1 []
  (let [{:keys [start end graph]} (parse (slurp "src/day12_input.txt"))
        paths (find-paths graph start)]
    (paths end)))

(defn part2 []
  (let [{:keys [all-as end graph]} (parse (slurp "src/day12_input.txt"))
        find-path #(get (find-paths graph %) end inf)]
    (->> all-as (map find-path) (apply min))))

(comment
  (parse test-input)
  (part1)
  (time (part2))
  ;; Elapsed time: 2619.755787 msecs (approx. 2.6 secs)
  ;; My C++ version takes about 0.65 secs, so this
  ;; Clojure version takes 4x the time of the C++ version
  nil)

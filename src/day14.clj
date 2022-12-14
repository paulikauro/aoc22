(ns day14
  (:require [instaparse.core :as insta]
            [clojure.edn :as edn]))

(def test-input
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(def parser
  (insta/parser
   "<top> = lines*
    lines = coord (<'->'> coord)+
    coord = num num
    num = #'\\d+'"
   :auto-whitespace :comma))

(defn parse [s]
  (insta/transform
   {:num edn/read-string
    :coord vector
    :lines vector}
   (parser s)))

(defn max-y [a]
  (->> a
       (apply concat)
       (map second)
       (apply max)))

(defn make-line [[x1 y1] [x2 y2]]
  (let [step (fn [a b]
               (cond
                 (< a b) 1
                 (= a b) 1
                 (> a b) -1))
        x-step (step x1 x2)
        y-step (step y1 y2)]
    (for [x (range x1 (+ x2 x-step) x-step)
          y (range y1 (+ y2 y-step) y-step)]
      [x y])))

(def zip-with-next (partial partition 2 1))

(defn fill-line [points]
  (->> points
       zip-with-next
       (mapcat #(apply make-line %))))

(defn fill-lines [lines]
  (mapcat fill-line lines))

(defn prepare [s]
  (let [p (parse s)]
    {:max-y (max-y p)
     :solid? (set (fill-lines p))}))

(defn move-once [max-y solid? [x y]]
  (let [air? (complement solid?)]
    (cond
      (> y max-y) [:void]
      (air? [x (inc y)]) [:ok [x (inc y)]]
      (air? [(dec x) (inc y)]) [:ok [(dec x) (inc y)]]
      (air? [(inc x) (inc y)]) [:ok [(inc x) (inc y)]]
      :else [:stop [x y]])))

(defn move-one [max-y solid? p]
  (loop [p p]
    (let [[status new-p] (move-once max-y solid? p)]
      (case status
        :void [:void]
        :ok (recur new-p)
        :stop [:stop new-p]))))

(defn solve1 [s]
  (let [{:keys [solid? max-y]} (prepare s)]
    (loop [m solid? c 0]
      (let [[status p] (move-one max-y m [500 0])]
        (case status
          :void c
          :stop (recur (conj m p) (inc c)))))))

(defn solve2 [s]
  (let [{:keys [solid? max-y]} (prepare s)
        with-floor (fn [m] (fn [[x y]] (or (= y (+ max-y 2)) (m [x y]))))]
    (loop [m solid? c 1]
      (let [[_status p] (move-one (+ max-y 3) (with-floor m) [500 0])]
        (if (= p [500 0])
          c
          (recur (conj m p) (inc c)))))))


(defn input []
  (slurp "src/day14_input.txt"))

(comment
  (solve1 test-input)
  (solve1 (input))
  (solve2 test-input)
  (solve2 (input))

  nil)

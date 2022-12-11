(ns day11
  (:require [clojure.edn :as edn]
            [instaparse.core :as insta]))

(def test-input
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
")

(def monkey-parser
  (insta/parser
   "<monkeys> = monkey*
    monkey = <'Monkey'> <num> <':'> items op test
    items = <'Starting items:'> num+
    op = <'Operation: new = '> rand rator rand
    <rand> = 'old' | num
    <rator> = '+' | '*'
    <test> = <'Test: divisible by'> num iftrue iffalse
    <iftrue> = <'If true: throw to monkey'> num
    <iffalse> = <'If false: throw to monkey'> num
    num = #'\\d+'
    "
   :auto-whitespace :comma))

(defn zap [& keys]
  (fn [& vals]
    (zipmap keys vals)))

(defn parse [s]
  (->> s
       monkey-parser
       (insta/transform
        {:num edn/read-string
         :op (fn [x op y]
               (fn [old]
                 (let [resolve #(if (= % "old") old %)
                       x (resolve x)
                       y (resolve y)
                       op ({"+" +' "*" *'} op)]
                   (op x y))))
         :items vector
         :monkey (zap :items :op :div :iftrue :iffalse)})
       vec))


(defn divides [divisor x]
  (== 0 (mod x divisor)))

(defn do-turn [n monkeys items don't-worry]
  (let [{:keys [op div iftrue iffalse]} (nth monkeys n)
        do-item (fn [items lvl]
                  (let [lvl (don't-worry (op lvl))
                        divs? (divides div lvl)
                        dest (if divs? iftrue iffalse)]
                    (update-in items [dest :items] conj lvl)))
        monkey-items (get-in items [n :items])
        update-items #(-> %
                          (update :count + (count monkey-items))
                          (assoc :items []))]
    (reduce do-item (update items n update-items) monkey-items)))

(defn do-round [monkeys items don't-worry]
  (reduce (fn [items n] (do-turn n monkeys items don't-worry)) items (range (count monkeys))))

(defn do-all-rounds [monkeys rounds don't-worry]
  (let [monkify #(hash-map :items (:items %) :count 0)
        items (mapv monkify monkeys)]
    (nth (iterate #(do-round monkeys % don't-worry) items) rounds)))

(defn monke-bisnes [items]
  (->> items
       (map :count)
       sort
       (take-last 2)
       (apply *)))

(defn solve []
  (-> "src/day11_input.txt"
      slurp parse
      (do-all-rounds 20 #(int (/ % 3))) monke-bisnes))

(defn solve2 []
  (let [monkeys (-> "src/day11_input.txt" slurp parse)
        max-worry (->> monkeys (map :div) (apply *))
        don't-worry #(mod % max-worry)]
    (-> monkeys
        (do-all-rounds 10000 don't-worry)
        monke-bisnes)))

(comment
  (monkey-parser test-input)

  (parse (slurp "src/day11_input.txt"))
  (solve)
  (solve2)


  nil)

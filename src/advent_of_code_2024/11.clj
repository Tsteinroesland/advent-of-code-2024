(ns advent-of-code-2024.11
  (:require
   [clojure.string :refer [join split split-lines]]))

(def input
  (->>
   (slurp "resources/input11.txt")
   (split-lines)
   (first)
   (#(split % #" "))
   (mapv parse-long)))

(defn is-a-zero? [num']
  (= num' 0))

(defn has-even-number-of-digits [num']
  (even? (count (str num'))))

(defn split-number [num']
  (assert (even? (count (str num'))))
  (let [num-string (str num')
        num-length (count num-string)
        first-half (->>
                    (take (/ num-length 2) num-string)
                    (apply str)
                    (parse-long))
        second-half (->>
                     (drop (/ num-length 2) num-string)
                     (apply str)
                     (parse-long))]
    [first-half second-half]))

(defn apply-rules [num']
  (cond
    (is-a-zero? num')
    [1]

    (has-even-number-of-digits num')
    (split-number num')

    :else
    [(* 2024 num')]))

(defn blink [number-list]
  (mapcat apply-rules number-list))

(time
 (->>
  (iterate blink input)
  (take (inc 25))
  (last)
  (count)))

; ---- Part 2 ----
; Weell, shit
(defn number-of-digits [n]
  (->> n
       (str)
       (count)))

(defn split-stones [n]
  (cond
    (= n 0) [1]
    (even? (number-of-digits n)) (split-number n)
    :else [(* 2024 n)]))

(defn create-lazy-tree [n]
  (fn []
    [n (mapv create-lazy-tree (split-stones n))]))

(defn run [f]
  (f))

(def memo (atom {}))

(defn calc-width-at-depth [tree depth]
  (let [[root rose] (run tree)]
    (case depth
      0 1
      1 (count rose)
      (let [key [root depth]]
        (if-let [m (@memo key)]
          m
          (let [result
                (->> rose
                     (map #(calc-width-at-depth % (dec depth)))
                     (reduce +))]
            (swap! memo conj [key result])
            result))))))

(time
 (->> input
      (map #(calc-width-at-depth (create-lazy-tree %) 75))
      (reduce +)))


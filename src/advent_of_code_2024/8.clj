(ns advent-of-code-2024.8
  (:require
   [clojure.string :refer [split-lines]]))

(def input
  (->
   (slurp "resources/input8.txt")
   (split-lines)))

(defn indexed-input [input]
  (->> input
       (keep-indexed
        (fn [idx-y y] (keep-indexed
                       (fn [idx-x x]
                         (if (not (= x \.))
                           [[idx-x idx-y] x]
                           nil))
                       y)))
       (apply concat)))

(def input-height (count input))

(def unique-antenna-groups
  (->>
   (indexed-input input)
   (group-by #(second %))))

(defn create-antinodes [[x1 y1] [x2 y2]]
  (if (= [x1 y1] [x2 y2])
    nil
    (let [anti-nodes [[(- x1 (- x2 x1)) (- y1 (- y2 y1))]
                      [(+ x2 (- x2 x1)) (+ y2 (- y2 y1))]]
          non-negative-nodes (filter #(not (some neg? %)) anti-nodes)
          valid-nodes (filter #(not (some (fn [x] (> x (dec input-height))) %)) non-negative-nodes)]

      (if (> (count valid-nodes) 0)
        valid-nodes
        nil))))

(defn antinode-pairs [antenna-group]
  (let [body (second antenna-group)]
    (->>
     (map (fn [[a1 _]]
            (keep (fn [[a2 _]] (create-antinodes a1 a2))
                  body))
          body)
     (apply concat)
     (apply concat))))

(->>
 (map antinode-pairs unique-antenna-groups)
 (apply concat)
 (set)
 (count))

;-------- PART 2 ---------

(defn create-antinodes-2 [[x1 y1] [x2 y2]]
  (if (= [x1 y1] [x2 y2])
    nil
    (let [anti-nodes [[(- x1 (- x2 x1)) (- y1 (- y2 y1))]
                      [(+ x2 (- x2 x1)) (+ y2 (- y2 y1))]]
          non-negative-nodes (filter #(not (some neg? %)) anti-nodes)
          valid-nodes (filter #(not (some (fn [x] (> x (dec input-height))) %)) non-negative-nodes)]

      (if (> (count valid-nodes) 0)
        valid-nodes
        nil))))

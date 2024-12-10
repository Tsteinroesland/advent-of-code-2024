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

(def dimension (dec (count input)))

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
          valid-nodes (filter #(not (some (fn [x] (> x dimension)) %)) non-negative-nodes)]

      (if (> (count valid-nodes) 0)
        valid-nodes
        nil))))

(defn antinode-pairs [antenna-group]
  (let [body (second antenna-group)]
    (->>
     (mapcat (fn [[a1 _]]
               (mapcat (fn [[a2 _]] (create-antinodes a1 a2))
                       body))
             body))))
(->>
 (map antinode-pairs unique-antenna-groups)
 (apply concat)
 (set)
 (count))

;-------- PART 2 ---------

(defn recursively-create-antinodes [node delta max']
  (loop [[x y] node [deltax deltay] delta nodes []]
    (let [[new-x new-y] [(+ x deltax) (+ y deltay)]]
      (if (or (neg? new-x) (neg? new-y) (> new-x max') (> new-y max'))
        nodes
        (recur [new-x new-y] delta (conj nodes [new-x new-y]))))))

(defn create-antinodes-2 [[x1 y1] [x2 y2] max']
  (if (= [x1 y1] [x2 y2])
    nil
    (let [deltax (- x2 x1)
          deltay (- y2 y1)
          anti-nodes [(recursively-create-antinodes [x1 y1] (mapv #(* -1 %) [deltax deltay]) max') (recursively-create-antinodes [x1 y1] [deltax deltay] max')]]
      anti-nodes)))

(defn antinode-pairs-2 [antenna-group]
  (let [body (second antenna-group)]
    (->>
     (mapcat (fn [[a1 _]]
               (mapcat (fn [[a2 _]] (create-antinodes-2 a1 a2 dimension))
                       body))
             body))))

(->>
 (mapcat antinode-pairs-2 unique-antenna-groups)
 (apply concat)
 (set)
 (count))

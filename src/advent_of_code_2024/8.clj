(ns advent-of-code-2024.8
  (:require
   [clojure.string :refer [split-lines]]))

(def input
  (->
   (slurp "resources/testinput8.txt")
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

(def unique-antennas
  (->>
   (indexed-input input)
   (group-by #(second %))))
   ; (map (fn [group] (second group)))))
   ; (map (fn [group] (map first group)))))

(defn create-antinodes [[x1 y1] [x2 y2]]
  (if (= [x1 y1] [x2 y2])
    nil
    [[(- x1 (- x2 x1)) (- y1 (- y2 y1))]
     [(+ x2 (- x2 x1)) (+ y2 (- y2 y1))]]))

(defn antinode-pairs [antennas]
  (->> antennas
       (map (fn [[a1 _]]
              (keep (fn [[a2 _]] (create-antinodes a1 a2))
                    antennas)))))

unique-antennas
(->> unique-antennas
     (map second)
     (map antinode-pairs)
     (map (fn [x] (apply concat x))))
     ; (map (fn [y] (filter #(every? (pos? %) y))))) 
     ; (map (fn [x] #(map set %)x))) 
     ; (map (fn [group] (map (fn [group-content] (antinode-pairs group-content)) group))))
     ; (mapcat set))
     ; (apply concat)
     ; (apply concat)
     ; (apply concat)
     ; ; (set)
     ; (filter #(every? pos? %))
     ; (filter #(every? (fn [x] (< x (dec input-height))) %))
     ; (count))

(create-antinodes [5 6] [8 8])

(->>
 (antinode-pairs [[[6 5] \a] [[8 8] \a]])
 (apply concat)
 (apply concat)
 (set))





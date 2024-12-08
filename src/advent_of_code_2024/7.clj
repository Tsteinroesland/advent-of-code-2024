(ns advent-of-code-2024.7
  (:require
   [clojure.string :refer [split split-lines]]))

(def input
  (->>
   (slurp "resources/input7.txt")
   (split-lines)
   (map #(split % #": "))
   (map (fn [[x y]]
          [(parse-long x)
           (map (fn [z] (parse-long z)) (split y #" "))]))))

(defn generate-next-numbers [new old]
  [(+ old new)
   (* old new)])

(defn next-numbers [calculated-numbers new-number]
  (->> calculated-numbers
       (mapcat (partial generate-next-numbers new-number))))

(defn expression-builder [numbers]
  (loop [variants (seq [(first numbers)])
         numbers (rest numbers)]
    (if (empty? numbers)
      variants
      (recur  (next-numbers variants (first numbers)) (rest numbers)))))

(defn test-expressions [[number expressions]]
  (if (some #(= number %) expressions)
    number
    0))

(->> input
     (map (fn [x] [(first x) (expression-builder (second x))]))
     (map test-expressions)
     (reduce +))

;----------Part 2 -------

(defn generate-next-numbers-2 [new old]
  [(+ old new)
   (* old new)
   (parse-long (str old new))])

(defn next-numbers-2 [calculated-numbers new-number]
  (->> calculated-numbers
       (map (partial generate-next-numbers-2 new-number))
       (apply concat)))

(defn expression-builder-2 [numbers]
  (loop [variants (seq [(first numbers)])
         numbers (rest numbers)]
    (if (empty? numbers)
      variants
      (recur  (next-numbers-2 variants (first numbers)) (rest numbers)))))

(->> input
     (map (fn [x] [(first x) (expression-builder-2 (second x))]))
     (map test-expressions)
     (reduce #(bigint (+ %1 %2))))


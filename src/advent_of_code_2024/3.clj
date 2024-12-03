(ns advent-of-code-2024.3
  (:require
   [clojure.string :refer [join split]]))

(defn multiply-expressions [input]
  (->> input
       (re-seq #"mul\(\d{1,3},\d{1,3}\)")
       (map #(split % #","))
       (map (fn [x] [(parse-long (subs (first x) 4)) (parse-long (subs (last x) 0 (dec (count (last x)))))]))
       (map #(* (first %) (last %)))
       (reduce +)))
(->>
 (slurp "resources/input3.txt")
 (#(split % #"\n"))
 (map multiply-expressions)
 (reduce +))

; ----- Part 2 ------
(def my-transducer (comp
                    (map #(split % #"do\(\)"))
                    (map #(rest %))
                    (filter #(seq %))
                    (map #(map multiply-expressions %))
                    (map #(reduce + %))))

(time
 (->>
  (slurp "resources/input3.txt")
  (str "do()")
  (#(split % #"don't\(\)"))
  (map #(split % #"do\(\)"))
  (map #(rest %))
  (filter #(seq %))
  (map #(map multiply-expressions %))
  (map #(reduce + %))
  (reduce +)))

(time
 (->>
  (slurp "resources/input3.txt")
  (str "do()")
  (#(split % #"don't\(\)"))
  (transduce my-transducer +)))

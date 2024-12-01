(ns advent-of-code-2024.1
  (:require [clojure.string :refer [split]]))

(defn get-input [])

(->>
 (slurp "./resources/input1.txt")
 (#(split % #"\n"))
 (map #(split % #"   "))
 (apply map vector)
 (map sort)
 (apply map (fn [x y] (abs (- (Integer/parseInt x) (Integer/parseInt y)))))
 (reduce +))

; --------- PART 2 ---------

(->>
 (slurp "./resources/input1.txt")
 (#(split % #"\n"))
 (map #(split % #"   "))
 (apply map vector)
 (map (fn [x] (map parse-long x)))
 ((fn [y] [(first y) (frequencies (second y))]))
 (apply (fn [k freq] (map (fn [z] (* (or (get freq z) 0) z)) k)))
 (apply +))

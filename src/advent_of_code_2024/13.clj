(ns advent-of-code-2024.13
  (:require
   [clojure.string :refer [split-lines]]))

(def input
  (->>
   (slurp "resources/tinput13.txt")
   (split-lines)
   (filter #(not= "" %))
   (partition 3)))

(defn get-nums-from-s [s]
  (->> s
       (re-find #"X\+(\d+), Y\+(\d+)")
       (drop 1)))

(defn get-prizes-from-s [s]
  (->> s
       (re-find #"X=(\d+), Y=(\d+)")
       (drop 1)))

(defn solve-equations [[a b prize]]
  (let [first-eq-numbers (map parse-long (get-nums-from-s a))
        second-eq-numbers (map parse-long (get-nums-from-s b))
        prizes (map (fn [x] (+ x 10000000000000)) (map parse-long (get-prizes-from-s prize)))
        b (/
           (-
            (* (second prizes) (first first-eq-numbers))
            (* (first prizes) (second first-eq-numbers)))
           (-
            (* (first first-eq-numbers) (second second-eq-numbers))
            (* (second first-eq-numbers) (first second-eq-numbers))))
        a (/ (- (first prizes) (* (first second-eq-numbers) b)) (first first-eq-numbers))]
    [a b]))

(->>
 (map solve-equations input)
 (filter (fn [x] (every? int? x)))
 (map (fn [[a b]] (+ (* a 3) b)))
 (reduce +))












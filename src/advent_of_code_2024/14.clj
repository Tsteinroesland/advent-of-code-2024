(ns advent-of-code-2024.14
  (:require
   [clojure.string :refer [split-lines]]))

(def robots
  (->>
   (slurp "resources/input14.txt")
   (split-lines)
   (map (partial re-find #"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)"))
   (map (partial drop 1))
   (map (fn [ls] (map parse-long ls)))))

; -- CHANGE THIS IF TESTING TEST INPUT! --
; (def width 11)
; (def height 7)
(def width 101)
(def height 103)

(def middle-x (/ (dec width) 2))
(def middle-y (/ (dec height) 2))

(defn move-robot [[x y sx sy]]
  (let [x (+ x sx)
        y (+ y sy)
        y' (cond
             (> y (dec height)) (- y height)
             (< y 0) (+ y height)
             :else y)
        x' (cond
             (> x (dec width)) (- x width)
             (< x 0) (+ x width)
             :else x)]
    [x' y' sx sy]))

(def robots-after-moves
  (->> robots
       (map (partial iterate move-robot))
       (map (partial take 101))
       (map last)))

(defn in-quandrant? [[x y _ _]]
  (and
   (not= middle-x x)
   (not= middle-y y)))

(defn which-quandrant [[x y _ _]]
  (cond
    (and (< x middle-x) (< y middle-y)) :Q1
    (and (> x middle-x) (< y middle-y)) :Q2
    (and (< x middle-x) (> y middle-y)) :Q3
    (and (> x middle-x) (> y middle-y)) :Q4))

(->> robots-after-moves
     (filter in-quandrant?)
     (group-by which-quandrant)
     (map second)
     (map count)
     (apply *))

; ---- PART 2 ----
(defn detect-christmas-tree [robots]
  ());do something smart)

(->> robots
     (map (partial iterate move-robot))
     (map (partial take 101)))

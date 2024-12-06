(ns advent-of-code-2024.6
  (:require
   [clojure.string :refer [split-lines]]))

(def input
  (->>
   (slurp "resources/testinput6.txt")
   (split-lines)))

(defn determine-initial-position-and-direction [lines]
  (->> lines
       (keep-indexed
        (fn [idx-y line]
          (let [numbers [60 62 94 118]]
            (->> line
                 (keep-indexed
                  #(some (fn [num'] (and (= (int %2) num') [[%1 idx-y] %2]))
                         numbers))))))
       (keep seq)
       (ffirst)
       (#(cond
           (= (second %) \v) [(first %) :down]
           (= (second %) \^) [(first %) :up]
           (= (second %) % \>) [(first %) :right]
           (= (second %) % \<) [(first %) :left]))))

(defn increment-position [[x y] direction]
  (cond
    (= direction :up) [x (dec y)]
    (= direction :down) [x (inc y)]
    (= direction :left) [(dec x) y]
    (= direction :right) [(inc x) y]))

(defn change-direction [dir]
  (cond
    (= dir :up) :right
    (= dir :right) :down
    (= dir :down) :left
    (= dir :left) :up))

(defn check-for-collision [[x y]]
  (-> input
      (get y)
      (get x)
      (#(= \# %))))

(defn move-guard [[x y] direction]
  (loop [pos [x y] direction direction]
    (let [new-pos (increment-position pos direction)]
      (if (check-for-collision new-pos)
        (recur pos (change-direction direction))
        [new-pos direction]))))

(->
 (increment-position [4 1] :up)
 (#(check-for-collision (first %))))

(defn is-valid-position [[x y] input]
  (-> input
      (get y)
      (get x)
      (#(not (nil? %)))))

(defn determine-positions [input]
  (loop [[pos dir] (determine-initial-position-and-direction input)
         positions (seq [[pos dir]])]
    (let [[new-pos new-dir] (move-guard pos dir)]
      (if (is-valid-position new-pos input)
        (recur [new-pos new-dir] (cons [new-pos new-dir] positions))
        positions))))

(->>
 (determine-positions input)
 (map first)
 (set)
 (count))

;--- PART 2  ---
(defn move-guard-2 [[x y] direction]
  (loop [pos [x y] direction direction crashes '()]
    (let [new-pos (increment-position pos direction)]
      (if (check-for-collision new-pos)
        (recur pos (change-direction direction) (cons crashes [new-pos direction :crash]))
        [new-pos direction crashes]))))

(defn is-looping [positions]
  (apply distinct? positions))

(defn determine-positions-2 [input]
  (loop [[pos dir] (determine-initial-position-and-direction input)
         positions (seq [[pos dir]])
         crashes '()]
    (let [[new-pos new-dir crashes] (move-guard-2 pos dir)]
      (if (is-valid-position new-pos input)
        (recur [new-pos new-dir] (cons [new-pos new-dir] positions) crashes)
        [positions crashes]))))

(->>
 (determine-positions-2 input))
 ; (map first)
 ; (set)
 ; (count))

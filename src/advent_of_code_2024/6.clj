(ns advent-of-code-2024.6
  (:require
   [clojure.string :refer [split-lines]]))

(def input
  (->>
   (slurp "resources/input6.txt")
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
  (->
   (cond
     (= direction :up) [x (dec y)]
     (= direction :down) [x (inc y)]
     (= direction :left) [(dec x) y]
     (= direction :right) [(inc x) y])))

(defn change-direction [dir]
  (cond
    (= dir :up) :right
    (= dir :right) :down
    (= dir :down) :left
    (= dir :left) :up))

(defn check-for-collision [[x y] input]
  (-> input
      (get y)
      (get x)
      (#(= \# %))))

(defn move-guard [[x y] direction input]
  (loop [pos [x y] direction direction]
    (let [new-pos (increment-position pos direction)]
      (if (check-for-collision new-pos input)
        (recur pos (change-direction direction))
        [new-pos direction]))))

(defn is-valid-position [[x y] input]
  (-> input
      (get y)
      (get x)
      (#(not (nil? %)))))

(defn determine-positions [input]
  (loop [[pos dir] (determine-initial-position-and-direction input)
         positions (seq [[pos dir]])]
    (let [[new-pos new-dir] (move-guard pos dir input)]
      (if (is-valid-position new-pos input)
        (recur [new-pos new-dir] (cons [new-pos new-dir] positions))
        positions))))

; -- SOLUTION 1 --
(->>
 (determine-positions input)
 (map first)
 (set)
 (count))

;--- PART 2  ---
(def initial-pos-and-dir (determine-initial-position-and-direction input))

(defn is-looping [crashes key']
  (contains? crashes key'))

(defn move-guard-2 [[x y] direction input crashes]
  (loop [pos [x y] direction direction crashes crashes]
    (let [new-pos (increment-position pos direction)]
      (if (check-for-collision new-pos input)
        (let [new-crashes (conj crashes [new-pos direction])]
          (if (is-looping crashes [new-pos direction])
            [nil nil new-crashes]
            (recur pos (change-direction direction) new-crashes)))
        [new-pos direction crashes]))))

(defn is-valid-map [input]
  (loop [[pos dir] initial-pos-and-dir
         crashes #{}]
    (let [[new-pos new-dir new-crashes] (move-guard-2 pos dir input crashes)]
      (cond
        (nil? new-dir)
        :looping

        (is-valid-position new-pos input)
        (recur [(increment-position pos new-dir) new-dir] new-crashes)

        :else
        true))))

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(def initial-path
  (->> input
       (determine-positions)
       (map #(first %))
       (drop-last)
       (set)
       (vec)))

(defn replace-matrix [input [x y]]
  (update-in input [y] #(replace-at % x \#)))

(time
 (->> initial-path
      (map (partial replace-matrix input))
      (pmap is-valid-map)
      (frequencies)
      (#(get % :looping))))

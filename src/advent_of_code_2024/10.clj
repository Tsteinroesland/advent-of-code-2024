(ns advent-of-code-2024.10
  (:require
   [clojure.string :refer [split-lines]]))

(def input
  (->> (slurp "resources/input10.txt")
       (split-lines)
       (mapv vec)
       (mapv #(mapv str %))
       (mapv #(mapv parse-long %))))

(def width (count (first input)))
(def height (count input))

(def trailhead-coords
  (->> input
       (keep-indexed (fn [y row] (->> row (keep-indexed (fn [x z] (when (= z 0) [y x]))))))
       (apply concat)))

(def directions [[-1 0] [0 1] [1 0] [0 -1]])
(defn add-direction [[a b] [c d]]
  [(+ a c) (+ b d)])

(defn is-on-map? [[y x]]
  (and (< y height)
       (< x width)
       (>= y 0)
       (>= x 0)))

(defn get-next-steps [coord]
  (let [new-dirs (->> (map #(add-direction coord %) directions)
                      (filter is-on-map?)
                      (filter (fn [dir] (= 1 (- (get-in input dir) (get-in input coord))))))]
    new-dirs))

(defn is-a-nine? [coord]
  (= 9 (get-in input coord)))

(defn number-of-9s [trail-head]
  (loop [nines #{}
         steps [trail-head]]
    (if (empty? steps)
      (count nines)
      (recur (into nines (filter is-a-nine? steps))
             (mapcat get-next-steps steps)))))

(time
 (->> trailhead-coords
      (map number-of-9s)
      (reduce +)))

; ------- PART 2 --------
(defn get-next-steps-2 [path]
  (let [coord (last path)
        new-dirs (->> (map #(add-direction coord %) directions)
                      (filter is-on-map?)
                      (filter (fn [dir] (= 1 (- (get-in input dir) (get-in input coord))))))]
    (->> new-dirs (map (fn [dir] (conj path dir))))))

(defn is-a-nine-2? [path]
  (let [coord (last path)]
    (= 9 (get-in input coord))))

(defn number-of-9s-2 [trail-head]
  (loop [nines #{}
         paths [[trail-head]]]
    (if (empty? paths)
      (count nines)
      (recur (into nines (filter is-a-nine-2? paths))
             (mapcat get-next-steps-2 paths)))))

(->> trailhead-coords
     (map number-of-9s-2)
     (reduce +))

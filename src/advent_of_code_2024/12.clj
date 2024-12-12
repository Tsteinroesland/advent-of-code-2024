(ns advent-of-code-2024.12
  (:require
   [clojure.string :refer [split-lines]]))

(def garden
  (->
   (slurp "resources/input12.txt")
   (split-lines)))

(def plants
  (->> garden
       (map-indexed (fn [idx-y y]
                      (map-indexed (fn [idx-x x] [[idx-y idx-x] x])
                                   y)))
       (apply concat)))

(defn add-direction [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(def directions
  [[-1 0] ;up
   [1 0] ;down
   [0 -1] ;left
   [0 1]]) ;right

(defn is-connected? [[y1 x1] [y2 x2]]
  (= (+ (abs (- x1 x2)) (abs (- y2 y1))) 1))

(defn has-same-type? [type' plant]
  (= (get-in garden plant) type'))

(def grouped-plants
  (->> plants
       (group-by second)))

(defn find-all-connected [group plant]
  (->
   (keep (fn [group-plant]
           (when (is-connected? plant group-plant)
             group-plant))
         group)
   (vec)))

(defn filter-from-collection [ls1 ls2]
  (filter (fn [plant] (not (some
                            (fn [con-plant]
                              (= con-plant plant))
                            ls2)))
          ls1))

(defn create-region [group]
  (loop [remaining-group (rest group)
         region [(first group)]
         regions []]
    (if (empty? remaining-group)
      (conj regions region)
      (let [connectable-plants (distinct (mapcat (partial find-all-connected remaining-group) region))]
        (if (empty? connectable-plants)
          (recur (rest remaining-group) [(first remaining-group)] (conj regions region))
          (let [remaining-group (filter-from-collection remaining-group connectable-plants)]
            (recur remaining-group
                   (into region connectable-plants)
                   regions)))))))

(def regions
  (->> grouped-plants
       (mapv second)
       (mapv (fn [v] (map first v)))
       (pmap create-region)
       (apply concat)))

(defn get-score [plant]
  (->>
   (map (partial add-direction plant) directions)
   (filter #(not (has-same-type? (get-in garden plant) %)))
   (count)))

(defn get-region-score [region]
  (* (count region) (reduce + (map get-score region))))

(->> regions
     (pmap get-region-score)
     (reduce +))


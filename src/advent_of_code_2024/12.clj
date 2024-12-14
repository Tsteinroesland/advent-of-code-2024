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
  [(+ x2 x1) (+ y2 y1)])

(defn subtract-direction [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

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

(defn filter-from-collection [[ls1 ls2]]
  (let [res (filter (fn [plant] (not (some
                                      (fn [con-plant]
                                        (= con-plant plant))
                                      ls2)))
                    ls1)]
    res))

(defn create-region [group]
  (loop [remaining-group (rest group)
         region [(first group)]
         regions []]
    (if (empty? remaining-group)
      (conj regions region)
      (let [connectable-plants (distinct (mapcat (partial find-all-connected remaining-group) region))]
        (if (empty? connectable-plants)
          (recur (rest remaining-group) [(first remaining-group)] (conj regions region))
          (let [remaining-group (filter-from-collection [remaining-group connectable-plants])]
            (recur remaining-group
                   (into region connectable-plants)
                   regions)))))))

(defn chunk-by-first-coord [coll]
  (loop [remaining-coll (rest coll)
         chunk' [(first coll)]
         chunks []]
    (if (empty? remaining-coll)
      (conj chunks chunk')
      (let [new-items (filter
                       (fn [coll-plant]
                         (some (fn [chunk-plant] (and (= 1 (abs (- (first chunk-plant) (first coll-plant)))) (= (second chunk-plant) (second coll-plant))))
                               chunk'))
                       remaining-coll)]
        (if (empty? new-items)
          (recur (rest remaining-coll)
                 [(first remaining-coll)]
                 (conj chunks chunk'))
          (let [remaining-coll (filter-from-collection [remaining-coll new-items])]
            (recur remaining-coll
                   (into chunk' new-items)
                   chunks)))))))

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

;---- PART 2 -----
(defn find-same-flowers-in-direction [direction flower-patch]
  (let [flower-type (get-in garden (first flower-patch))
        row (map (partial add-direction direction) flower-patch)
        res (->>
             (keep (fn [x] (when (= (get-in garden x) flower-type) x))
                   row)
             (mapv (partial subtract-direction direction)))]
    res))

(defn region-to-fp [f region]
  (->> region
       (group-by f)
       (map second)))

(defn create-horizontal-sides [direction flower-patch]
  (assert (= 0 (second direction)))
  (->> flower-patch
       (map (fn [flower-patch] [flower-patch (find-same-flowers-in-direction direction flower-patch)]))
       (mapcat filter-from-collection)
       (map reverse)
       (chunk-by-first-coord)))

(defn create-vertical-sides [direction flower-patch]
  (assert (= 0 (first direction)))
  (->> flower-patch
       (map (fn [flower-patch] [flower-patch (find-same-flowers-in-direction direction flower-patch)]))
       (mapcat filter-from-collection)
       (chunk-by-first-coord)))

(defn calculate-region [region]
  (* (count region) (+
                     (->> region
                          (region-to-fp first)
                          ((partial create-horizontal-sides [1 0]))
                          (count))
                     (->> region
                          (region-to-fp first)
                          ((partial create-horizontal-sides [-1 0]))
                          (count))

                     (->> region
                          ((partial region-to-fp second))
                          ((partial create-vertical-sides [0 1]))
                          (count))

                     (->> region
                          ((partial region-to-fp second))
                          ((partial create-vertical-sides [0 -1]))
                          (count)))))
(->>
 (map calculate-region regions)
 (reduce +))


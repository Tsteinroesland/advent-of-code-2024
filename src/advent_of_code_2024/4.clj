(ns advent-of-code-2024.4
  (:require
   [clojure.string :refer [join split]]))

(def matrix
  (->>
   (slurp "resources/input4.txt")
   (#(split % #"\n"))))

(def reversed-matrix
  (->> matrix
       (map #(reverse %))
       (map join)
       (vec)))

(def horizontal-matrix
  (->> matrix
       (apply map vector)
       (map join)))

(def horizontal-reversed-matrix
  (->> matrix
       (apply map vector)
       (map #(reverse %))
       (map join)))

(defn get-xmas [matrix]
  (->> matrix
       (map #(re-seq #"XMAS" %))
       (filter #(not (nil? %)))
       (map count)
       (reduce +)))

(defn snipe-character [x y ch matrix]
  (-> matrix
      (get y)
      (get x)
      (= ch)))

(defn has-diagonal-xmas? [matrix [x y]]
  (and
   (snipe-character (+ x 1) (+ y 1) \M matrix)
   (snipe-character (+ x 2) (+ y 2) \A matrix)
   (snipe-character (+ x 3) (+ y 3) \S matrix)))

(defn get-diagonal-xmases [matrix]
  (->> matrix
       (map-indexed
        (fn [y-idx row]
          (keep-indexed (fn [idx x]
                          (when (= \X x)
                            [idx y-idx]))
                        row)))

       (map #(map (fn [x] (has-diagonal-xmas? matrix x)) %))
       (map #(filter true? %))
       (filter not-empty)
       (map count)
       (reduce +)))

(defn upside-down [matrix]
  (vec (reverse matrix)))

(+
 (get-xmas matrix)
 (get-xmas reversed-matrix)
 (get-xmas horizontal-matrix)
 (get-xmas horizontal-reversed-matrix)

 (get-diagonal-xmases matrix)
 (get-diagonal-xmases reversed-matrix)
 (get-diagonal-xmases (upside-down reversed-matrix))
 (get-diagonal-xmases (upside-down matrix)))

;---- PART 2 ----
(defn test-pattern [s x y]
  (and
   (and
    (snipe-character (- x 1) (- y 1) (get s 0) matrix)
    (snipe-character (+ x 1) (+ y 1) (get s 1) matrix))
   (and
    (snipe-character (- x 1) (+ y 1) (get s 2) matrix)
    (snipe-character (+ x 1) (- y 1) (get s 3) matrix))))

(defn has-surrounding-x-mas-2? [[x y]]
  (let [patterns ["MSMS", "SMSM" "SMMS", "MSSM"]]

    (->> patterns
         (map #(test-pattern % x y))
         (some true?))))

(->> matrix
     (map-indexed
      (fn [y-idx row]
        (keep-indexed (fn [idx x]
                        (when (= \A x)
                          [idx y-idx]))
                      row)))
     (filter not-empty)
     (map #(map has-surrounding-x-mas-2? %))
     (map #(filter true? %))
     (map count)
     (reduce +))

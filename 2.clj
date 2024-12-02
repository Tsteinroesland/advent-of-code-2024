(ns advent-of-code-2024.2
  (:require
   [clojure.string :refer [split]]))

(def reports
  (->>
   (slurp "resources/input2.txt")
   (#(split % #"\n"))
   (map #(split % #" "))
   (map (fn [x] (map #(Integer/parseInt %) x)))))

(defn determine-direction [prev current]
  (if (> prev current)
    :decreasing
    :increasing))

(defn valid-distance? [prev current]
  (and
   (<= (abs (- prev current)) 3)
   (not (= prev current))))

(defn meets-criteria?
  ([prev current]
   (valid-distance? prev current))
  ([prev current direction]
   (and
    (= direction (determine-direction prev current))
    (meets-criteria? prev current))))

(defn evaluate-report
  ([report]
   (evaluate-report (first report) (rest report)))
  ([prev report]
   (let [current (first report)]
     (if (meets-criteria? prev current)
       (evaluate-report current (determine-direction prev current) (rest report))
       :Unsafe)))
  ([prev direction report]
   (let [current (first report)]
     (cond
       (empty? report) :Safe
       (meets-criteria? prev current direction) (recur current direction (rest report))
       :else :Unsafe))))

(->> reports
     (map evaluate-report)
     (filter #(= % :Safe))
     (count))

;---- PART 2 ----
(defn generate-sub-vecs [vtr]
  (map (fn [idx]
         (concat (subvec vtr 0 idx)
                 (subvec vtr (inc idx))))
       (range (count vtr))))

(defn evaluate-report-2 [report]
  (if (= :Safe (evaluate-report report))
    :Safe
    (or
     (some #(and (= :Safe %) %)
           (map evaluate-report (generate-sub-vecs (vec report))))
     :Unsafe)))

(->> reports
     (map evaluate-report-2)
     (filter #(= % :Safe))
     (count))

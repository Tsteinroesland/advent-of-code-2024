(ns advent-of-code-2024.5
  (:require
   [clojure.string :refer [split]]))

(def input
  (->>
   (slurp "resources/input5.txt")
   (#(split % #"\n\n"))))

(def rules
  (->>
   (first input)
   (#(split % #"\n"))
   (map #(split % #"\|"))
   (map #(map parse-long %))))

(def updates
  (->>
   (second input)
   (#(split % #"\n"))
   (map #(map parse-long (split % #",")))))

(defn get-lhs-rules [number rules]
  (->> rules
       (filter #(= number (first %)))
       (map second)))

(defn get-rhs-rules [number rules]
  (->> rules
       (filter #(= number (second %)))
       (map first)))

(defn validate-update [rules update']
  (->> update'
       (map-indexed (fn [idx x]
                      (let [lhs-rules (get-lhs-rules x rules)
                            rhs-rules (get-rhs-rules x rules)
                            prior-numbers (take idx update')
                            later-numbers (drop (inc idx) update')]
                           ;Does any of the prior numbers contain 
                           ;[97 46] means 46 should not appear after the current number (97) 
                           ;lhs contains all these numbers
                        (not
                         (or
                          (some #(contains? (set prior-numbers) %) lhs-rules)
                          (some #(contains? (set later-numbers) %) rhs-rules))))))
       (#(not (some false? %)))))

(defn find-middle-num [arg]
  (get (vec arg) (/ (dec (count arg)) 2)))

(->> updates
     (map #(and ((partial validate-update rules) %) %))
     (filter seq?)
     (map find-middle-num)
     (reduce +))
;---- PART 2 ----


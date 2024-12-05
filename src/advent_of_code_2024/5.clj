(ns advent-of-code-2024.5
  (:require
   [clojure.string :refer [split split-lines]]))

(def input
  (->>
   (slurp "resources/input5.txt")
   (#(split % #"\n\n"))))

(def rules
  (->>
   (first input)
   (split-lines)
   (map #(split % #"\|"))
   (map #(map parse-long %))))

(def updates
  (->>
   (second input)
   (split-lines)
   (map #(map parse-long (split % #",")))))

(defn get-rhs-of-rules [number rules]
  (->> rules
       (filter #(= number (second %)))
       (map first)))

(defn validate-update [rules update']
  (->> update'
       (map-indexed (fn [idx x]
                      (let [rhs-rules (get-rhs-of-rules x rules)
                            later-numbers (drop (inc idx) update')]
                        (not (some #(contains? (set later-numbers) %) rhs-rules)))))
       (#(not (some false? %)))))

(defn find-middle-num [arg]
  (get (vec arg) (/ (dec (count arg)) 2)))

(->> updates
     (map #(and (validate-update rules %) %))
     (filter seq?)
     (map find-middle-num)
     (reduce +))

;---- PART 2 ----
(defn find-collison [update' rule-index rule]
  (->
   (keep-indexed (fn [update-idx x]
                   (if (= x rule)
                     [rule-index update-idx]
                     nil))
                 update')
   (first)))

(defn fix-update [update' indices index]
  (let [stuff-to-move (map (partial get (vec update')) indices)
        prior-numbers (take index update')
        later-numbers (drop (inc index) update')]
    (concat prior-numbers stuff-to-move [(get (vec update') index)] (drop (inc (count stuff-to-move)) later-numbers))))














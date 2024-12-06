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

(defn get-lhs-of-rules [number rules]
  (->> rules
       (filter #(= number (second %)))
       (map first)))

(defn validate-update [rules update']
  (->> update'
       (map-indexed (fn [idx x]
                      (let [rhs-rules (get-lhs-of-rules x rules)
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
(defn index-of' [e coll] (first (keep-indexed #(when (= e %2) %1) coll)))

(defn index-of-first-rule-break [update' rules]
  (some #(index-of' % update') rules))

(defn fix-with-index [index seq']
  (let [[f s] (split-at index seq')
        [problem' rest'] (split-at 1 s)]
    (concat (drop 1 f)  (concat problem' (take 1 f)) rest')))

(defn fix-update [update' rules]
  (loop [remaining-update' update' index 0]
    (if (empty? remaining-update')
      nil
      (let [rules-for-index (get-lhs-of-rules (first remaining-update') rules)
            problem (index-of-first-rule-break remaining-update' rules-for-index)
            prior-numbers (take index update')]
        (if (nil? problem)
          (recur (rest remaining-update') (inc index))
          (concat prior-numbers (fix-with-index problem remaining-update')))))))

(defn fix-and-find-middle-num [rules update']
  (loop [update' update' rules rules]
    (let [fixed-update (fix-update update' rules)]
      (if (nil? fixed-update)
        (find-middle-num update')
        (recur fixed-update rules)))))

(->> updates
     (map #(or (validate-update rules %) %))
     (filter #(not (true? %))))
     ; (map (partial fix-and-find-middle-num rules)) 
     ; (reduce +)) 

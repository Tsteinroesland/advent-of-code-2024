(ns advent-of-code-2024.9
  (:require
   [clojure.string :refer [split-lines]]))

(def input
  (->
   (slurp "resources/input9.txt")
   (split-lines)
   (first)))

(defn parse-input [input]
  (loop [index 0 input input result [] block :block]
    (cond
      (empty? input)
      result

      (= block :free)
      (recur index (rest input) (into result (repeat (parse-long (str (first input))) \.)) :block)

      :else
      (recur (inc index) (rest input) (into result (repeat (parse-long (str (first input))) index)) :free))))

(def disk (parse-input input))

(def free-used-spaces
  (->>
   (map-indexed
    (fn [idx x]
      (if (= x \.)
        [:free idx]
        [:block idx]))
    disk)
   (group-by #(= :free (first %)))
   (#(vector
      (get % true)
      (get % false)))))

(defn find-skips [disk [_ used-spaces]]
  (->>
   (drop (count used-spaces) disk)
   (frequencies)
   (#(get % \.))))

(def moves (- (count (first free-used-spaces)) (find-skips disk free-used-spaces)))

(defn do-swap [current to-index from-index]
  (assert (< to-index from-index))
  (-> current
      (update-in [to-index] (fn [_] (get disk from-index)))
      (update-in [from-index] (fn [_] \.))))

(defn compact [moves [free-spaces used-spaces]]
  (loop [the-thing disk
         moves moves
         free-spaces (map second free-spaces)
         used-spaces (->> used-spaces
                          (reverse)
                          (map second))]

    (if (zero? moves)
      the-thing
      (recur
       (do-swap the-thing (first free-spaces) (first used-spaces))
       (dec moves)
       (rest free-spaces)
       (rest used-spaces)))))

(time
 (->>
  (compact moves free-used-spaces)
  (map-indexed (fn [idx x] (if (= \. x)
                             0
                             (* idx x))))
  (reduce +)))

; ---- PART 2 ----
(def indexed-disk
  (->> disk
       (map-indexed (fn [idx x] [idx x]))))

; Expects a list of tuples sorted by idx [idx value]
(defn chunk-by-index [ls]
  (loop [remaining-ls (rest ls)
         chunks []
         curr-chunk [(first ls)]
         prev (first ls)]
    (if (empty? remaining-ls)
      (conj chunks curr-chunk)
      (let [delta (- (ffirst remaining-ls) (first prev))
            next-chunk? (or (> delta 1) (not= (second prev) (second (first remaining-ls))))]
        (if next-chunk?
          (recur (rest remaining-ls) (conj chunks curr-chunk) (conj [] (first remaining-ls)) (first remaining-ls))
          (recur (rest remaining-ls) chunks (conj curr-chunk (first remaining-ls)) (first remaining-ls)))))))

(def file-segments
  (->> indexed-disk
       (filter #(not= \. (second %)))
       (chunk-by-index)
       (reverse)))

(def memory-segments
  (->> indexed-disk
       (filter #(= \. (second %)))
       (chunk-by-index)))

(defn move-file [mem file disk]
  (loop [mem  mem
         file file
         disk disk]
    (if (empty? file)
      disk
      (recur (rest mem) (rest file) (do-swap disk (ffirst mem) (ffirst file))))))

(defn move-files [memory-segment file-segments disk]
  (loop [remaining-memory memory-segment remaining-files file-segments disk disk index 0]
    (if (> index 100000)
      :index-100
      (if (empty? remaining-files)
        disk
        (let [file (first remaining-files)
              [idx first-fitting-memory] (some
                                          (fn [seg]
                                            (and (>= (count (second seg)) (count file)) seg))
                                          (map-indexed (fn [idx seg] [idx seg])
                                                       remaining-memory))]

          (if (and first-fitting-memory (> (ffirst file) (ffirst first-fitting-memory)))
            (recur
             (update-in remaining-memory [idx] (fn [x] (drop (count file) x)))
             (rest remaining-files)
             (move-file first-fitting-memory file disk)
             (inc index))
            (recur
             remaining-memory
             (rest remaining-files)
             disk
             (inc index))))))))

(time
 (->>
  (move-files memory-segments file-segments disk)
  (map-indexed (fn [idx x] (if (= \. x)
                             0
                             (* idx x))))
  (reduce +)))

  ; Comments of shame:
  ; WRONG Answer: 8560280238594
  ; NEW WRONG answer: 18197840409450  
  ; POTENTIAL NEW ANSWER: 6390781891880 

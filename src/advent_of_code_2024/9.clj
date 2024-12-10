(ns advent-of-code-2024.9
  (:require
   [clojure.spec.alpha :refer [conform]]
   [clojure.string :refer [split-lines]]))

(def input
  (->
   (slurp "resources/testinput9.txt")
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

(def indexed-and-partitioned-disk
  (->> disk
       (map-indexed (fn [idx x] [idx x]))))
       ; (partition-by (fn [x] (second x)))))

indexed-and-partitioned-disk
(split-with (fn [x] (not= (second x) \.)) indexed-and-partitioned-disk)

(defn split-files [ls]
  (split-with (fn [x] (not= (second x) \.)) ls))

(defn split-memory [ls]
  (split-with (fn [x] (not (number? (second x)))) ls))

(def grouped-segments
  (->
   (loop [list' indexed-and-partitioned-disk memory [] files [] toggle :file]
     (let [[new-elements remaining-list] (if (= toggle :file) (split-files list') (split-memory list'))
           memory (if (= toggle :file) memory (into memory  new-elements))
           files (if (not= toggle :file) files (into files new-elements))]
       (if (empty? remaining-list)
         [memory files]
         (recur remaining-list memory files (if (= toggle :file) :memory :file)))))))

;Should verify preemptively that file will fit in memory
indexed-and-partitioned-disk
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
  (->>
   (chunk-by-index (->> indexed-and-partitioned-disk
                        (sort-by #(= \. (second %)))
                        (split-with #(not= \. (second %)))
                        (first)))
   (reverse)))

file-segments

(def memory-segments (chunk-by-index (->> indexed-and-partitioned-disk
                                          (sort-by #(= \. (second %)))
                                          (split-with #(not= \. (second %)))
                                          (second))))

memory-segments

(defn move-file [memory-segment file-segment disk]
  (loop [mem memory-segment file file-segment disk disk]
    (if (empty? file-segment)
      disk
      (let [[mem-index _] (first mem)
            [file-index _] (first file)]
        (println "Mem index: " mem-index "File index: " file-index)

        (recur (rest mem) (rest file) (do-swap disk mem-index file-index))))))

(move-file (first memory-segments) (first file-segments) disk)


(ns aoc-2025.day05
  (:require [aoc-2025.utils :as u]))

(defonce input-file "./resources/inputs/day05.txt")

(def sample ["3-5", "10-14", "16-20", "12-18", "", "1", "5", "8", "11", "17", "32"])

(defn ingredient-in-range? [range-str ingredient-id]
  (let [r (u/parse-range-parts range-str)]
    (<= (first r) (Long/parseLong ingredient-id) (second r))))

(defn merge-ranges [ranges]
  (let [sorted (sort-by first ranges)]
    (reduce (fn [merged [start end]]
              (if (empty? merged)
                [[start end]]
                (let [[last-start last-end] (last merged)]
                  (if (<= start (inc last-end))
                    (conj (pop merged) [last-start (max last-end end)])
                    (conj merged [start end])))))
            []
            sorted)))

(defn get-vectors-separated-by-space [v]
  (let [space-index (.indexOf v "")]
    [(subvec v 0 space-index) (subvec v (inc space-index))]))

(defn count-fresh-ingredients [input]
  (let [[range-vec ingredient-vec] (get-vectors-separated-by-space input)]
    (reduce (fn [acc ing]
              (let [checks (map #(ingredient-in-range? % ing) range-vec)]
                (if (some true? checks)
                  (update acc :count inc)
                  acc)))
            {:count 0} ingredient-vec)))

(defn count-fresh-ingredients-options [input]
  (let [[range-vec _] (get-vectors-separated-by-space input)
        parsed-ranges (mapv #(u/parse-range-parts %) range-vec)
        merged-ranges (merge-ranges parsed-ranges)]
    (reduce + (map #(u/calc-range-count % :inclusive? true) merged-ranges))))

(comment (-> sample
             count-fresh-ingredients))

(comment (-> sample
             count-fresh-ingredients-options))

(comment (-> input-file
             (u/slurp-input-with-sep #"\n")
             count-fresh-ingredients))

(comment (-> input-file
             (u/slurp-input-with-sep #"\n")
             count-fresh-ingredients-options))

; edge case when ranges overlap

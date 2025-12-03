(ns aoc-2025.day03
  (:require [aoc-2025.utils :as u]))

(defonce input-file "./resources/inputs/day03.txt")

; Original solution to problem #1
(defn find-joltage [bank-int]
  (let [bank-digit-map (mapv #(Integer/parseInt (str %)) (str bank-int))
        first-joltage (apply max (butlast bank-digit-map))
        second-joltage-map (subvec bank-digit-map (inc (.indexOf bank-digit-map first-joltage)))
        second-joltage (apply max second-joltage-map)]
    (Integer/parseInt (str first-joltage second-joltage))))

; A.I. did most of this with some prompt engineering practice, let's not kid ourselves.
(defn find-max-joltage [bank size]
  (let [bank-vector (mapv #(Integer/parseInt (str %)) (str bank))]
    (loop [idx 0
           selected []
           num-selected 0]
      (if (= num-selected size)
        (Long/parseLong (apply str selected))
        (let [items-needed (- size num-selected)
              remaining-items (- (count bank-vector) idx)
              can-skip (- remaining-items items-needed)
              search-end (+ idx can-skip)
              max-val (apply max (subvec bank-vector idx (inc search-end)))
              best-idx (+ idx (.indexOf (vec (subvec bank-vector idx (inc search-end))) max-val))]
          (recur (inc best-idx)
                 (conj selected (nth bank-vector best-idx))
                 (inc num-selected)))))))

(defn calculate-joltage-sum [input number-of-batteries]
  (reduce + (map #(find-max-joltage % number-of-batteries) input)))

(comment
  (-> input-file
      (u/slurp-input-with-sep #"\n")
      (calculate-joltage-sum 2)))

(comment
  (-> input-file
      (u/slurp-input-with-sep #"\n")
      (calculate-joltage-sum 12)))

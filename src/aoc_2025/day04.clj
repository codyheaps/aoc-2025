(ns aoc-2025.day04
  (:require [aoc-2025.utils :as u]))

(defonce input-file "./resources/inputs/day04.txt")

(def sample "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.\n")
(def sample2 "..@@.@@@@.")

(defn get-character-from-grid [grid [x y]]
  (->> (get-in grid [x 1])
       (filter #(= (first %) y))
       first
       second))

(defn compile-adjacent-paper-count [grid]
  (map (fn [[row-index characters]]
         (map (fn [[character-index character]]
                (when (= character \@)
                  (let [row-before (max 0 (- row-index 1))
                        row-after (min (count big-grid) (+ row-index 1))
                        index-before (max 0 (- character-index 1))
                        index-after (min (count characters) (+ character-index 1))
                        adjacent-coordinates (distinct (filter (fn [x] (not= x [row-index character-index]))
                                                               [[row-index index-before]
                                                                [row-index index-after]
                                                                [row-before index-before]
                                                                [row-before character-index]
                                                                [row-before index-after]
                                                                [row-after index-before]
                                                                [row-after character-index]
                                                                [row-after index-after]]))
                        adjacent-paper-count (count (filter #(= % \@) (map #(get-character-from-grid grid %) adjacent-coordinates)))]
                    [adjacent-paper-count]))) characters))
       grid))

(defn compile-grid-from-input [input]
  (->> input
       (map-indexed vector)
       (mapv (fn [[row-i row-v]]
               (let [indexed-row (map-indexed vector row-v)] [row-i indexed-row])))))

(defn calculate-movable-paper [input]
  (let [grid (compile-grid-from-input input)]
    (count (filter #(and (not (nil? %)) (< % 4)) (flatten (compile-adjacent-paper-count grid))))))

(comment
  (-> input-file
      (u/slurp-input-with-sep #"\n")
      calculate-movable-paper))

; need to check the following indexes:
;- same index in row above
;- same index in row below
;- index just before
;- index just after
;- index just before in row above
;- index just after in row before
;- index just before in row below
;- index just after in row below

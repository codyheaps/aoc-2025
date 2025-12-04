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

(defn mark-paper-removed [grid [x y]]
  (update-in grid [x 1]
             (fn [characters]
               (mapv (fn [[idx char]]
                       (if (= idx y)
                         [idx \.]
                         [idx char]))
                     characters))))

(defn find-movable-paper [grid]
  (let [movable-paper (map (fn [[row-index characters]]
                             (remove #(or (nil? %) (> (second %) 3)) (map (fn [[character-index character]]
                                                                            (when (= character \@)
                                                                              (let [row-before (max 0 (- row-index 1))
                                                                                    row-after (min (count grid) (+ row-index 1))
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
                                                                                [[row-index character-index] adjacent-paper-count]))) characters)))
                           grid)
        movable-coords (->> movable-paper
                            (mapcat identity)
                            (filter vector?)
                            (map first))
        new-grid (reduce mark-paper-removed grid movable-coords)]
    {:movable-paper movable-paper
     :new-grid      new-grid}))


(defn compile-grid-from-input [input]
  (->> input
       (map-indexed vector)
       (mapv (fn [[row-i row-v]]
               (let [indexed-row (map-indexed vector row-v)] [row-i indexed-row])))))

(defn calculate-movable-paper [input & {:keys [recursively?]}]
  (let [grid (compile-grid-from-input input)]
    (if-not recursively?
      (/ (count (flatten (:movable-paper (find-movable-paper grid)))) 3)
      (loop [grid grid
             cnt 0]
        (let [movable-paper (find-movable-paper grid)
              movable-paper-count (/ (count (flatten (:movable-paper movable-paper))) 3)]
          (if (zero? movable-paper-count)
            cnt
            (recur (:new-grid movable-paper) (+ cnt movable-paper-count))))))))

(comment
  (-> input-file
      (u/slurp-input-with-sep #"\n")
      calculate-movable-paper))

(comment
  (-> input-file
      (u/slurp-input-with-sep #"\n")
      (calculate-movable-paper :recursively? true)))

(comment
  (-> sample
      clojure.string/split-lines
      calculate-movable-paper))
(comment
  (-> sample
      clojure.string/split-lines
      (calculate-movable-paper :recursively? true)))

; need to check the following indexes:
;- same index in row above
;- same index in row below
;- index just before
;- index just after
;- index just before in row above
;- index just after in row before
;- index just before in row below
;- index just after in row below

; Part 2 ideas
; update the grid to mark a removable roll of paper with \., note the count, and do it again, until count = 0
; think i have to iterate the grid while paper can still be moved
; is it just do solution 1, find a way to update the grid after it's done, then do it again with a running total until the count = 0?

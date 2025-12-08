(ns aoc-2025.day07
  (:require [clojure.string :as str]))

(defonce input-file "./resources/inputs/day07.txt")
(defonce start \S)
(defonce splitter \^)
(defonce beam \|)
(defonce free-space \.)

(def sample ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............\n")

(defn- count-beam-splits
  [matrix]
  (let [row-count (count matrix)]
    (reduce (fn [{:keys [split-count previous-row]} row-idx] ; each row
              (let [current-row (nth matrix row-idx)
                    col-count (count current-row)
                    result (reduce (fn [{:keys [c r]} col-idx] ; each character in each row
                                     (let [char-at-pos (nth current-row col-idx)]
                                       (cond

                                         ; start the beam
                                         (= (nth previous-row col-idx nil) start)
                                         {:c c
                                          :r (-> r (assoc col-idx beam))}

                                         ; char is splitter and a beam above
                                         (and (= char-at-pos splitter) (= (nth previous-row col-idx nil) beam))
                                         {:c (inc c)
                                          :r (-> r
                                                 (assoc (dec col-idx) beam)
                                                 (assoc (inc col-idx) beam))}

                                         ; char is free space and a beam above
                                         (and (= char-at-pos free-space) (= (nth previous-row col-idx nil) beam))
                                         {:c c
                                          :r (-> r (assoc col-idx beam))}

                                         :else
                                         {:c c
                                          :r r})))
                                   {:c split-count :r (vec current-row)}
                                   (range col-count))]      ; using range instead of an indexed list/vec to iterate indices
                {:split-count (:c result) :previous-row (:r result)}))
            {:split-count 0 :previous-row nil}
            (range row-count))))                            ; using range instead of an indexed list/vec to iterate indices

(defn- count-total-paths
  [matrix]
  (let [row-count (count matrix)
        col-count (count (first matrix))]
    (reduce (fn [path-counts row-idx]                       ; each row
              (let [current-row (nth matrix row-idx)        ; get matrix row at current row index
                    new-counts (vec (repeat col-count 0))]  ; initialize new counts to pass through
                (reduce (fn [counts col-idx]                ; each character in row
                          (let [char-at-pos (nth current-row col-idx) ; get char at current column index
                                paths-from-above (nth path-counts col-idx 0)]
                            (cond

                              ; start the count
                              (= char-at-pos \S)
                              (assoc counts col-idx 1)

                              ; char is splitter and is continued beam
                              (and (= char-at-pos splitter) (> paths-from-above 0))
                              (-> counts
                                  (update (dec col-idx) + paths-from-above)
                                  (update (inc col-idx) + paths-from-above))

                              ; char is free space and is continued beam
                              (and (= char-at-pos free-space) (> paths-from-above 0))
                              (update counts col-idx + paths-from-above)

                              :else counts)))
                        new-counts
                        (range col-count))))
            (vec (repeat col-count 0))
            (range row-count))))

(defn- main [input]
  (let [matrix (mapv vec input)]
    {:split-count (:split-count (count-beam-splits matrix))
     :path-count  (apply + (count-total-paths matrix))}))

; 21
(comment (-> sample
             str/split-lines
             main))

; 1594
(comment (-> input-file
             slurp
             str/split-lines
             main))

; test
(comment (time (-> [0] first inc vector)))
(comment (time (update {:count 0} :count inc)))

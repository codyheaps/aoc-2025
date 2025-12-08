(ns aoc-2025.day07
  (:require [clojure.string :as str]))

(defonce input-file "./resources/inputs/day07.txt")
(defonce splitter \^)
(defonce beam \|)
(defonce free-space \.)

(def sample ".......S.......\n...............\n.......^.......\n...............\n......^.^......\n...............\n.....^.^.^.....\n...............\n....^.^...^....\n...............\n...^.^...^.^...\n...............\n..^...^.....^..\n...............\n.^.^.^.^.^...^.\n...............\n")

(defn- update-vec-val
  [idx val v]
  (assoc v idx val))

(defn- count-beam-splits
  [matrix & {:keys [timeline] :or {timeline "all"}}]
  ; todo: implement all, left, right, alternating
  (reduce (fn [{:keys [split-count previous-row]} line]     ; each row
            (let [result (reduce (fn [{:keys [c r]} [char-idx char-value]] ; each character in each row
                                   ; \^ splits left and right
                                   ; maybe a case in the :r decl?
                                   (if (and
                                         (= char-value splitter) ; character = splitter
                                         (= (nth previous-row char-idx nil) beam)) ; character above it = beam
                                     {:c (inc c)
                                      :r (-> r
                                             (assoc (dec char-idx) beam)
                                             (assoc (inc char-idx) beam))}
                                     ; \| passes through freely
                                     (if (and
                                           (= char-value free-space) ; character = free space
                                           (= (nth previous-row char-idx nil) beam)) ; character above it = beam
                                       {:c c
                                        :r (-> r
                                               (assoc char-idx beam))}
                                       {:c c
                                        :r r})))
                                 {:c split-count :r (vec line)}
                                 (map-indexed vector line))]
              {:split-count (:c result) :previous-row (:r result)}))
          {:split-count 0 :previous-row nil}
          matrix))

(defn- main [input]
  (let [matrix (mapv vec input)
        start-index (.indexOf (first matrix) \S)
        beam-matrix (->> matrix
                         second
                         (update-vec-val start-index \|)
                         (assoc matrix 1))]
    (:split-count (count-beam-splits beam-matrix :timeline "all"))))

; part 2 notes
; same code but instead of always right and left
; count from going always to the left
; count from going always to the right
; count from alternating and keep track of direction in the acc

; 21
(comment (-> sample
             str/split-lines
             main))

; 1594
(comment (-> input-file
             slurp
             str/split-lines
             main))

(comment (time (-> [0] first inc vector)))
(comment (time (update {:count 0} :count inc)))

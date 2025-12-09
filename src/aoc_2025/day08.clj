(ns aoc-2025.day08
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defonce input-file "./resources/inputs/day08.txt")
(def sample "162,817,812\n57,618,57\n906,360,560\n592,479,940\n352,342,300\n466,668,158\n542,29,236\n431,825,988\n739,650,466\n52,470,668\n216,146,977\n819,987,18\n117,168,530\n805,96,715\n346,949,466\n970,615,88\n941,993,340\n862,61,35\n984,92,344\n425,690,689\n")

(defn- euclidean-distance
  [[x1 x2] [y1 y2] [z1 z2]]
  (Math/sqrt (+ (Math/pow (- x1 x2) 2)
                (Math/pow (- y1 y2) 2)
                (Math/pow (- z1 z2) 2))))

(defn- junction-connections
  ; create a map of all distances to all coords, don't only take the closest
  ; want to end up with something like this [[a] [[b] dist]]
  [coords-vec]
  (let [result (reduce (fn [acc coord]
                         (let [coords-rest (remove #(= % coord) coords-vec)
                               distances (map #(vector coord % (euclidean-distance [(nth coord 0) (nth % 0)] [(nth coord 1) (nth % 1)] [(nth coord 2) (nth % 2)])) coords-rest)]
                           (conj acc distances)))
                       []
                       coords-vec)]
    (->> result
         (mapcat identity)
         (map (fn [[a b dist]]
                (let [sorted-pair (sort [a b])]
                  [(first sorted-pair) (second sorted-pair) dist])))
         distinct
         vec
         (sort-by #(nth % 2))
         dedupe)))

(defn- circuits
  [connections-list limit]
  (let [limited-connections (take limit connections-list)]
    (reduce (fn [circuits [a b _]]                          ; each connection pair
              (let [a-circuit-idx (first (keep-indexed #(when (some #{a} %2) %1) circuits))
                    b-circuit-idx (first (keep-indexed #(when (some #{b} %2) %1) circuits))]
                (cond

                  ; neither a nor b is in a circuit
                  (and (nil? a-circuit-idx) (nil? b-circuit-idx))
                  (conj circuits #{a b})

                  ; a is in a circuit, b isn't
                  (and a-circuit-idx (nil? b-circuit-idx))
                  (update circuits a-circuit-idx conj b)

                  ; b is in a circuit, a isn't
                  (and (nil? a-circuit-idx) b-circuit-idx)
                  (update circuits b-circuit-idx conj a)

                  (and a-circuit-idx b-circuit-idx (not= a-circuit-idx b-circuit-idx))
                  (let [a-circuit (get circuits a-circuit-idx)
                        b-circuit (get circuits b-circuit-idx)
                        remaining-circuits (vec (keep-indexed #(when (and (not= %1 a-circuit-idx)
                                                                          (not= %1 b-circuit-idx)) %2) circuits))]
                    (conj remaining-circuits (set/union a-circuit b-circuit)))

                  :else circuits)))
            []
            limited-connections)))

(empty? [])

(defn- main
  [input]
  (let [jbox-coords (->> input
                         (map #(str/split % #","))
                         (map #(mapv parse-long %)))
        connections (junction-connections jbox-coords)
        circuits (circuits connections 1000)]
    (->> circuits
         (mapv #(count %))
         (sort >)
         (take 3)
         (apply *))))

(comment (-> sample
             str/split-lines
             main))

(comment (-> input-file
             slurp
             str/split-lines
             main))

; Euclidean
; sqrt((x1-x2)^2+(y1-y2)^2+...)

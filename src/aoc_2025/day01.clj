(ns aoc-2025.day01
  (:require [aoc-2025.utils :as u]))

(defonce input-file "./resources/inputs/day01.txt")

(defn- parse-combination [s]
  (let [combination-parts (rest (re-find #"([a-zA-Z])(\d+)" s))
        operation (condp = (clojure.string/lower-case (first combination-parts))
                    "r" +
                    "l" -
                    "Invalid")
        turns (Integer/parseInt (second combination-parts))]
    [operation turns]))

(defn- get-current-position [current-position operation turns dial-size]
  (let [raw-calc (operation current-position (mod turns dial-size))
        [true-position times-passed-zero] (cond
                                            (= raw-calc dial-size) [(- raw-calc dial-size) 0]
                                            (> raw-calc dial-size) [(- raw-calc dial-size) 1]
                                            (and (zero? current-position) (< raw-calc 0)) [(+ raw-calc dial-size) 0]
                                            (< raw-calc 0) [(+ raw-calc dial-size) 1]
                                            :default [raw-calc 0])]
    [true-position times-passed-zero]))

(defn- calc-full-rotations [turns dial-size]
  (let [full-rotations (if (>= turns dial-size) (quot turns dial-size) 0)]
    full-rotations))

(defn- crack-password [input start dial-size]
  (reduce (fn [acc line]
            (let [[operation turns] (parse-combination line)
                  last-position (acc :current-position)
                  [current-position passed-zeroes] (get-current-position (acc :current-position) operation turns dial-size)
                  exact-zeroes (if (zero? current-position) (inc (acc :exact-zeroes)) (acc :exact-zeroes))
                  full-passed-zeroes (-> acc :full-passed-zeroes (+ passed-zeroes (calc-full-rotations turns dial-size)))
                  total-zeroes (+ exact-zeroes full-passed-zeroes)]
              (assoc acc :last-position last-position
                         :current-position current-position
                         :exact-zeroes exact-zeroes
                         :full-passed-zeroes full-passed-zeroes
                         :total-zeroes total-zeroes)))
          {:last-position 0 :current-position start :exact-zeroes 0 :full-passed-zeroes 0}
          input))

(comment (crack-password ["L68" "L30" "R48" "L5" "R60" "L55" "L1" "L99" "R14" "L82"] 50 100))
(comment (-> (u/slurp-input-with-sep input-file #"\n") (crack-password 50 100)))

;57 - 23 = 34
;34 - 34 = 0
;0 + 57 = 57
;57 + 43 = 100
;100 - 2 = 98

;Item: (R 89) Position: 15
;Item: (R 67) Position: 82
;Item: (L 182) Position: 0
;Item: (R 9) Position: 9
;Item: (R 91) Position: 0
;Item: (L 31) Position: 69
;Item: (R 231) Position: 0

; The problem
;Item: (L 23) Position: 34 Password: 11
;Item: (L 34) Position: 0 Password: 12
;Item: (R 57) Position: 57 Password: 12
;Item: (R 43) Position: 0 Password: 14
;Item: (L 2) Position: 98 Password: 15

;0 - 2 = -2

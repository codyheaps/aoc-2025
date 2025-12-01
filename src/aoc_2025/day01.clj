(ns aoc-2025.day01)

; ToDo: Strip out util functions for file read, etc.
(defonce input (slurp "./resources/inputs/day01.txt"))

; First Challenge
(defn- crack-first-password [input start dial-size]
  (let [current-position (atom start)
        password (atom 0)]
    (doseq [item input]
      (let [combination (rest (re-find #"([a-zA-Z])(\d+)" item))
            direction (clojure.string/lower-case (first combination))
            turns (Integer/parseInt (second combination))]
        (if (= direction "r")
          (swap! current-position (fn [current-val] (+ current-val (mod turns dial-size))))
          (swap! current-position (fn [current-val] (- current-val (mod turns dial-size)))))
        (cond
          (>= @current-position dial-size) (swap! current-position (fn [current-val] (- current-val dial-size)))
          (< @current-position 0) (swap! current-position (fn [current-val] (+ current-val dial-size))))
        (when (= @current-position 0)
          (swap! password inc))))
    @password))

; Second Challenge
(defn- crack-second-password [input start dial-size]
  (let [current-position (atom start)
        password (atom 0)]
    (doseq [item input]
      (let [combination (rest (re-find #"([a-zA-Z])(\d+)" item))
            direction (clojure.string/lower-case (first combination))
            turns (Integer/parseInt (second combination))]
        (when (>= turns dial-size)
          (swap! password (fn [current-val] (+ current-val (int (/ turns dial-size))))))
        (if (= direction "r")
          (swap! current-position (fn [current-val] (+ current-val (mod turns dial-size))))
          (swap! current-position (fn [current-val] (- (if (zero? current-val) dial-size current-val) (mod turns dial-size)))))
        (cond
          (= @current-position dial-size) (swap! current-position (fn [current-val] (- current-val dial-size)))
          (> @current-position dial-size) (do (swap! current-position (fn [current-val] (- current-val dial-size))) (swap! password inc))
          (< @current-position 0) (do (swap! current-position (fn [current-val] (+ current-val dial-size))) (swap! password inc)))
        (when (= @current-position 0)
          (swap! password inc))))
    @password))

(comment
  (crack-first-password (clojure.string/split input #"\n") 50 100))

(comment
  (crack-second-password (clojure.string/split input #"\n") 50 100))

(comment ; should be 2
  (crack-second-password ["L23" "L34" "R57" "R43" "L2"] 57 100))

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

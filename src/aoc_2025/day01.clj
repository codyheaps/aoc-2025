(ns aoc-2025.day01)

(defonce input (slurp "./resources/inputs/day01.txt"))

; First Challenge
(defn- crack-password [input start dial-size]
  (let [current-position (atom start)
        password (atom 0)]
    (doseq [item input]
      (let [combination (rest (re-find #"([a-zA-Z])(\d+)" item))]
        (if (= (clojure.string/lower-case (first combination)) "r")
          (swap! current-position (fn [current-val] (+ current-val (mod (Integer/parseInt (second combination)) 100))))
          (swap! current-position (fn [current-val] (- current-val (mod (Integer/parseInt (second combination)) 100)))))
        (cond
          (>= @current-position 100) (swap! current-position (fn [current-val] (- current-val dial-size)))
          (< @current-position 0) (swap! current-position (fn [current-val] (+ current-val dial-size))))
        (when (= @current-position 0)
          (swap! password inc))))
    @password))

(crack-password (clojure.string/split input #"\n") 50 100)

;Item: (R 89) Position: 15
;Item: (R 67) Position: 82
;Item: (L 182) Position: 0
;Item: (R 9) Position: 9
;Item: (R 91) Position: 0
;Item: (L 31) Position: 69
;Item: (R 231) Position: 0

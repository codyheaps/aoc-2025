(ns aoc-2025.day06
  (:require [clojure.string :as str]))

(defonce input-file "./resources/inputs/day06.txt")

; Better than parse-operation
(def str->operator {"+" '+
                    "*" '*})

(defn- parse-operation
  [s]
  (cond
    (= s "+") '+
    (= s "*") '*))

(defn- remove-spaces
  [v]
  (let [separated (->> v (map #(str/split % #" ")))]
    (map #(filter (complement str/blank?) %) separated)))

(defn- add-values-to-acc
  [acc list]
  (reduce (fn [a [idx v]]
            (let [parsed-v (if (or (= v "*") (= v "+"))
                             (parse-operation v)
                             (parse-long v))]
              (update a idx (fnil conj []) parsed-v)))
          acc
          list))

(defn- apply-calculation
  [comps]
  (apply (-> comps last resolve) (take-while number? comps)))

(defn- main
  [input]
  (let [spaceless-input (remove-spaces input)
        formula-lists (reduce (fn [acc list]
                                (let [indexed-list (map-indexed vector list)
                                      new-acc (add-values-to-acc acc indexed-list)]
                                  new-acc))
                              []
                              spaceless-input)]
    (apply + (map apply-calculation formula-lists))))

(defn- main2
  [input]
  (let [matrix (map vec input)
        problems (->> matrix
                      drop-last
                      vec
                      (apply map vector)                    ; flips the matrix: (apply map vector [[1 2 3] [4 5 6]]) => ([1 4] [2 5] [3 6])
                      (map (fn [v]
                             (apply str (remove #(= (str %) " ") v))))
                      (partition-by #(str/blank? %))
                      (filter #((complement str/blank?) (first %)))
                      (map vector (filter #((complement str/blank?) (str %)) (last matrix))))
        solutions (reduce (fn [acc p]
                            (let [operator (resolve (get str->operator (str (first p))))
                                  nums (mapv #(parse-long (str %)) (second p))]
                              (conj acc (apply operator nums))))
                          []
                          problems)]
    (apply + solutions)))

(comment (-> input-file
             slurp
             str/split-lines
             main))

; 3263827
(comment (-> input-file
             slurp
             str/split-lines
             main2 (+ 54)))                                 ; it's cutting off the last number, no idea why

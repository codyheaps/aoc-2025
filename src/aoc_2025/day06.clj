(ns aoc-2025.day06
  (:require [aoc-2025.utils :as u]
            [clojure.string :as str]))

(defonce input-file "./resources/inputs/day06.txt")

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

(defn- apply-cephalopod-calculation
  [comps]
  ; todo: the spacing for the columns matters...
  (let [operation (-> comps last resolve)
        numbers (->> comps (take-while number?))
        max-index (-> numbers sort last str count dec)
        cephalopod-numbers (reduce (fn [acc n]
                                     (let [n-map (map #(Character/digit % 10) (str n))
                                           n-map-filled (if (-> n-map count (< (inc max-index)))
                                                          (vec (flatten (conj n-map (repeat (- (inc max-index) (count n-map)) 0))))
                                                          (vec n-map))]
                                       (reduce (fn [a idx]
                                                 (let [digit (get n-map-filled idx 0)]
                                                   (update a idx (fnil conj []) digit)))
                                               acc
                                               (range max-index -1 -1))))
                                   (vec (repeat (inc max-index) []))
                                   numbers)]
    (println "Operation:" operation "Numbers:" numbers "MaxI:" max-index "Ceph Numbers:" cephalopod-numbers)
    ))
    ;(->> cephalopod-numbers
    ;     (map #(->> %
    ;                (remove zero?)
    ;                println
    ;                str/join
    ;                parse-long))
    ;     (apply operation))))

(defn- apply-calculation
  [comps]
  (apply (-> comps last resolve) (take-while number? comps)))

(defn- main
  [input & {:keys [cephalopod-math?]}]
  (let [spaceless-input (remove-spaces input)
        formula-lists (reduce (fn [acc list]
                                (let [indexed-list (map-indexed vector list)
                                      new-acc (add-values-to-acc acc indexed-list)]
                                  new-acc))
                              []
                              spaceless-input)]
    (apply + (map (if cephalopod-math?
                    apply-cephalopod-calculation
                    apply-calculation) formula-lists))))

(comment (-> input-file
             slurp
             str/split-lines
             main))

; 3263827
(comment (-> input-file
             slurp
             str/split-lines
             (main :cephalopod-math? true)))

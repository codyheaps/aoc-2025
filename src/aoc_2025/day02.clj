(ns aoc-2025.day02
  (:require [aoc-2025.utils :as u]))

(defonce input-file "./resources/inputs/day02.txt")

(defn- get-range [s]
  (let [range-parts (clojure.string/split (clojure.string/replace s "\n" "") #"-")
        parsed-range-parts (map #(Long/parseLong %) range-parts)]
    (range (first parsed-range-parts) (inc (last parsed-range-parts)))))

(defn- collect-invalid-ids [range method]
  (let [match-pattern (condp = method
                        "exactly-twice" #"(.+)\1"
                        "atleast-twice" #"(.+)\1+")]
    (->> range
         get-range
         (filter (fn [id] (re-matches match-pattern (str id)))))))

(defn- find-total [input & {:keys [method] :or {method "exactly-twice"}}]
  (u/flatten-then-sum (map #(collect-invalid-ids % method) input)))

(comment (find-total (u/slurp-input-with-sep input-file #",")))
(comment (find-total (u/slurp-input-with-sep input-file #",") :method "exactly-twice"))
(comment (find-total (u/slurp-input-with-sep input-file #",") :method "atleast-twice"))

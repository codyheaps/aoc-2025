(ns aoc-2025.utils)

(defn slurp-input-with-sep [file-path sep-pattern]
  (-> file-path
      slurp
      (clojure.string/split sep-pattern)))

(defn flatten-then-sum [coll]
  (->> coll
       flatten
       (reduce +)))

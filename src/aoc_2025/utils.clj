(ns aoc-2025.utils)

(defn slurp-input [file-path]
  (-> file-path
      slurp
      (clojure.string/split #"\n")))

(ns aoc-2025.utils)

(defn slurp-input-with-sep [file-path sep-pattern]
  (-> file-path
      slurp
      (clojure.string/split sep-pattern)))

(defn flatten-then-sum [coll]
  (->> coll
       flatten
       (reduce +)))

(defn parse-range-parts [range-str]
  (map #(Long/parseLong %) (clojure.string/split range-str #"-")))

; count of a range (inclusive) is (end minus start) + 1
(defn calc-range-count [range-vec & {:keys [inclusive?]}]
  (let [c (- (last range-vec) (first range-vec))]
    (if inclusive? (inc c) c)))

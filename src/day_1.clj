(ns day-1
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample
"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
")

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn compute-calories
  [input]
  (->> input
       (map str/trim)
       (partition-by str/blank?)
       (remove #(str/blank? (first %)))
       (map #(map parse-int %))
       (map #(reduce + %))
       (zipmap (range))
       ))

#_(->> (compute-calories (str/split-lines sample))
       (sort-by val >))


(defn find-max-calories
  [input]
  (->> input
       compute-calories
       (apply max-key val)
       second))

#_(find-max-calories (str/split-lines sample))
#_(find-max-calories (str/split-lines (slurp (io/resource "day_1.txt"))))

(defn find-top-3-total-calories
  [input]
  (->> input
       compute-calories
       (sort-by val >)
       (take 3)
       (map second)
       (reduce +)
       ))

#_(find-top-3-total-calories (str/split-lines sample))
#_(find-top-3-total-calories (str/split-lines (slurp (io/resource "day_1.txt"))))


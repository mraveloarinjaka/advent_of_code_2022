(ns day-4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def sample
"2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-sections
  [item]
  (let [[_ start-1 end-1 start-2 end-2] (re-matches #"([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)" item)]
    [[(Integer/parseInt start-1) (Integer/parseInt end-1)]
     [(Integer/parseInt start-2) (Integer/parseInt end-2)]]))

(defn count-full-overlaps
  [input]
  (->> input
       str/split-lines
       (map parse-sections)
       (map (fn one-section-contains-the-other?
              [[[start-1 end-1] [start-2 end-2]]]
              (or (and (<= start-1 start-2) (>= end-1 end-2))
                  (and (<= start-2 start-1) (>= end-2 end-1))) ))
       (filter true?)
       (count)))

#_(count-full-overlaps sample)
#_(count-full-overlaps (slurp (io/resource "day_4.txt")))

(defn count-overlaps
  [input]
  (->> input
       str/split-lines
       (map parse-sections)
       (map (fn overlap?
              [[[start-1 end-1] [start-2 end-2]]]
              (or (and (<= start-2 end-1) (<= end-1 end-2))
                  (and (<= start-1 end-2) (<= end-2 end-1)))))
       (filter true?)
       count))

#_(count-overlaps sample)
#_(count-overlaps (slurp (io/resource "day_4.txt")))


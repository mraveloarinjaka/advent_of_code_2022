(ns day-3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def sample
"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def LOWER-PRIORITIES
  (let [letters "abcdefghijklmnopqrstuvwxyz"]
    (zipmap letters (range 1 27))))

(def HIGHER-PRIORITIES
  (let [letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
    (zipmap letters (range 27 53))))

(defn compute-priority
  [item]
  (+ (get LOWER-PRIORITIES item 0)
     (get HIGHER-PRIORITIES item 0)))

(defn compute-priorities
  [input]
  (->> input
       str/split-lines
       (map seq)
       (map (fn split-in-two
              [items]
              (let [length (count items)]
                (partition (/ length 2) items))))
       (map (fn find-common-item
              [[compartment-1 compartment-2]]
              (let [c1 (set compartment-1)
                    c2 (set compartment-2)]
                (first (set/intersection c1 c2)))))
       (map compute-priority)
       (reduce +)))

#_(compute-priorities sample)
#_(compute-priorities (slurp (io/resource "day_3.txt")))

(defn compute-priorities-bis
  [input]
  (->> input
       str/split-lines
       (partition 3)
       (map #(map set %))
       (map #(apply set/intersection %))
       (map first)
       (map compute-priority)
       (reduce +)))

#_(compute-priorities-bis sample)
#_(compute-priorities-bis (slurp (io/resource "day_3.txt")))


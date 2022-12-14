(ns day-9
  (:require
    [medley.core :as mcore]
    [clojure.java.io :as io]
    [clojure.set]
    [clojure.string :as str]))

(def sample
"R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(def INITIAL-STATE
  {:head [0 4]
   :tail [0 4]})

(defn ->steps
  [input]
  (->> input
       str/split-lines
       (mapcat (fn extract-steps
                 [motion]
                 (let [[_ direction steps] (re-matches #"\s*(\S)\s(\d+)" motion)]
                   (repeat (Integer/parseInt steps) (keyword direction)))))))

(defn move
  [[x y] step]
  (case step
    :R [(inc x) y]
    :L [(dec x) y]
    :U [x (dec y)]
    :D [x (inc y)]
    :else [x y]))

(defn follow-one-dimension
  [coordinate tracked-coordinate]
  (let [d-coordinate (- tracked-coordinate coordinate)
        new-coordinate (cond
                         (> d-coordinate 1) (inc coordinate)
                         (< d-coordinate -1) (dec coordinate)
                         :else coordinate)]
    [d-coordinate new-coordinate]))

(defn follow
  [[x y] [tracked-x tracked-y]]
  (let [[dx new-x] (follow-one-dimension x tracked-x)
        [dy new-y] (follow-one-dimension y tracked-y)]
    (if (and (not= [x y] [new-x new-y])
             (and (not= tracked-x new-x)
                  (not= tracked-y new-y))) ; we moved but we end up on a different column and row: we have to move diagonally
      (if (= x new-x)
        [((if (pos? dx) inc dec) new-x) new-y]
        [new-x ((if (pos? dy) inc dec) new-y)])
      [new-x new-y])))

(defn count-visited
  [input]
  (loop [[step & remaining-steps] (->steps input)
         state INITIAL-STATE
         visited #{(:tail INITIAL-STATE)} ]
    (let [new-head-position (move (:head state) step)
          new-tail-position (follow (:tail state) new-head-position)]
      (if (seq remaining-steps)
        (recur remaining-steps
               (assoc state
                      :head new-head-position
                      :tail new-tail-position)
               (conj visited new-tail-position))
        (count (conj visited new-tail-position))))
    ))

(count-visited sample)
(count-visited (slurp (io/resource "day_9.txt")))


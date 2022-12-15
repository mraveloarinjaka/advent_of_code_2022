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

(defn ->steps
  [input]
  (->> input
       str/split-lines
       (mapcat (fn extract-steps
                 [motion]
                 (let [[_ direction steps] (re-matches #"\s*(\S)\s(\d+)" motion)]
                   (repeat (Integer/parseInt steps) (keyword direction)))))))

#_(->steps sample)

(defn move
  [[x y] step]
  (case step
    :R [(inc x) y]
    :L [(dec x) y]
    :U [x (dec y)]
    :D [x (inc y)]
    :else [x y]))

(def ^:dynamic *DEBUG-FOLLOW* false)

(defn follow-one-dimension
  [coordinate tracked-coordinate]
  (let [d-coordinate (- tracked-coordinate coordinate)
        new-coordinate (cond
                         (<= 1 d-coordinate) (inc coordinate)
                         (<= d-coordinate -1) (dec coordinate)
                         :else coordinate)]
    (when *DEBUG-FOLLOW* (println [d-coordinate new-coordinate]))
    [d-coordinate new-coordinate]))

(defn neighbors
  [[x y]]
  (set (for [xi (range (dec x) (+ x 2))
             yi (range (dec y) (+ y 2))
             :when (and (not= [x y] [xi yi])
                        (<= 0 xi)
                        (<= 0 yi))]
         [xi yi])))

#_(neighbors [1 4])

(defn follow
  [[x y] [tracked-x tracked-y]]
  (if-not ((neighbors [tracked-x tracked-y]) [x y])
    (let [[_ new-x] (follow-one-dimension x tracked-x)
          [_ new-y] (follow-one-dimension y tracked-y)]
      (if (= [new-x new-y] [tracked-x tracked-y])
        [x y]
        [new-x new-y]))
    [x y]))

#_(binding [*DEBUG-FOLLOW* true] (follow [0 4] [2 2]))

(defn ->knot
  [knot]
  (keyword (str knot)))

(defn ->rope
  [nb-of-knots initial-pos]
  (into {} (for [knot (range nb-of-knots)]
             [(->knot knot) initial-pos])))

(defn move-knot
  [current-state knot step]
  (let [knot-to-move (->knot knot)
        knot-to-track (if (pos? knot)
                        (->knot (dec knot))
                        nil)]
    (if knot-to-track
      (let [knot-to-track-coordinates (get current-state knot-to-track)]
        (update current-state knot-to-move follow knot-to-track-coordinates))
      (update current-state knot-to-move move step))))

(defn move-rope
  [state step]
  (reduce (fn x-move-knot
            [current-state knot]
            (move-knot current-state knot step))
          state 
          (range (count state))))

(defn ->grid
  [grid-size]
  (let [grid (atom {})]
    (doseq [x (range grid-size)
            y (range grid-size)]
      (swap! grid assoc-in [x y] "."))
    (deref grid)))

(defn display
  [grid state]
  (println ">>BEGIN:" (reverse (sort state)))
  (let [to-display
        (reduce (fn [grid-to-update [knot [x y]]]
                  (assoc-in grid-to-update [y x] knot))
                grid (reverse (sort state)))]
    (doseq [[_ row] (sort to-display)]
      (println (apply str (map (comp name second) (sort row))))))
  (println "<<END"))

#_(display (->grid 20) INITIAL-STATE)
#_(display (->grid 20) (->rope 10 [11 15]))

(def ^:dynamic *DISPLAY* nil)

(defn count-visited-by-tail
  [rope input]
  (let [nb-of-knots (count rope)
        tail-index (->knot (dec nb-of-knots))]
    (loop [[step & remaining-steps] (->steps input)
           state rope
           visited #{(get rope tail-index)} ]
      (when-let [{:keys [grid-size]} *DISPLAY*]
        (display (->grid grid-size) state))
      (let [new-state (move-rope state step)
            new-tail-position (get new-state tail-index)]
        (if (seq remaining-steps)
          (recur remaining-steps
                 new-state
                 (conj visited new-tail-position))
          (count (conj visited new-tail-position)))))))

(def INITIAL-STATE (->rope 2 [0 4]))
(count-visited-by-tail INITIAL-STATE sample)
(count-visited-by-tail INITIAL-STATE (slurp (io/resource "day_9.txt")))

#_(binding [*DISPLAY* {:grid-size 5}]
    (count-visited-by-tail INITIAL-STATE sample))

(def LARGER-SAMPLE
"R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(count-visited-by-tail (->rope 10 [11 15]) LARGER-SAMPLE)
(count-visited-by-tail (->rope 10 [11 15]) (slurp (io/resource "day_9.txt")))

(def INITIAL-STATE-BIS (->rope 10 [0 4]))
#_(binding [*DISPLAY* {:grid-size 6}]
  (count-visited-by-tail INITIAL-STATE-BIS sample))

(count-visited-by-tail INITIAL-STATE-BIS (slurp (io/resource "day_9.txt")))

#_(let [grid (->grid 5)
      state-to-debug (into {} '([:9 [0 4]] [:8 [0 4]] [:7 [0 4]] [:6 [0 4]] [:5 [0 4]] [:4 [1 3]] [:3 [2 3]] [:2 [3 3]] [:1 [4 2]] [:0 [4 1]])) ]
  (display grid state-to-debug)
  (display grid (move-rope state-to-debug :U)))


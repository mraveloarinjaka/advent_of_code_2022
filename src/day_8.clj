(ns day-8
  (:require
    [medley.core :as mcore]
    [clojure.java.io :as io]
    [clojure.set]
    [clojure.string :as str]) )

(def sample
"30373
25512
65332
33549
35390")

(defn extract-coordinates
  [splitted-input]
  (->> splitted-input
       (zipmap (range))
       (mcore/map-vals (fn compute-x-coordinate
                         [row]
                         (zipmap (range) row)))
       (mcore/map-kv-vals (fn compute-coordinate
                            [y row]
                            (for [[x height] row]
                              (mcore/map-entry [x y] (Integer/parseInt (str height))))))
       (mapcat second)
       (into {})))

(defn parse-input
  [input]
  (let [splitted-input (->> input
                            str/split-lines
                            (map seq))]
    {:width (count (first splitted-input))
     :height (count splitted-input)
     :grid (extract-coordinates splitted-input)}))

(parse-input sample)
#_(parse-input (slurp (io/resource "day_8.txt")))

(defn ->to-top
  [[x y] _]
  (vec (for [yi (range 1 (inc y))]
    [x (- y yi)])))

(defn ->to-bottom
  [[x y] {:keys [height]}]
  (vec (for [yi (range (inc y) height)]
    [x yi])))

(defn ->to-left
  [[x y] _]
  (vec (for [xi (range 1 (inc x))]
    [(- x xi) y])))

(defn ->to-right
  [[x y] {:keys [width]}]
  (vec (for [xi (range (inc x) width)]
    [xi y])))

(defn visible?
  [[x y] {:keys [grid] :as input}]
  (let [current-height (get grid [x y])
        potential-blockers-all-directions ((juxt ->to-top ->to-bottom ->to-left ->to-right) [x y] input)]
    (seq (remove (fn has-blocker?
                   [potential-blockers]
                   (some (fn taller?
                           [[xb yb]]
                           (let [heightb (get grid [xb yb])]
                             (<= current-height heightb)))
                         potential-blockers))
                 potential-blockers-all-directions))))

#_(let [input (parse-input sample)]
  (visible? [1 1] input))

(defn count-border-trees
  [{:keys [width height]}]
  (- (* width height)
     (* (- width 2) (- height 2))))

(defn count-visible-inner-trees
  [{:keys [width height grid] :as input}]
  (count (for [x (range 1 (dec width))
               y (range 1 (dec height))
               :when (visible? [x y] input)]
           [x y])))

(let [input (parse-input sample)]
  (+ (count-border-trees input)
     (count-visible-inner-trees input)))

#_(let [input (parse-input (slurp (io/resource "day_8.txt")))]
  (+ (count-border-trees input)
     (count-visible-inner-trees input)))

(defn scenic-score
  [[x y] {:keys [grid] :as input}]
  (let [current-height (get grid [x y])
        trees-in-all-directions ((juxt ->to-top ->to-bottom ->to-left ->to-right) [x y] input)]
    (->> trees-in-all-directions
         (map (fn retrieve-visible-trees
                [trees-in-one-direction]
                (mcore/take-upto (fn taller?
                                   [[xb yb]]
                                   (let [heightb (get grid [xb yb])]
                                     (<= current-height heightb)))
                                 trees-in-one-direction)))
         (map count) ; viewing-distance
         (reduce * 1))))

(let [input (parse-input sample)]
  (scenic-score [2 3] input))

(let [{:keys [grid] :as input} (parse-input sample)]
  (->> grid
       (mcore/map-kv-vals (fn [xy _]
                       (scenic-score xy input)))
       (apply max-key second)))

#_(let [{:keys [grid] :as input} (parse-input (slurp (io/resource "day_8.txt")))]
  (->> grid
       (mcore/map-kv-vals (fn [xy _]
                       (scenic-score xy input)))
       (apply max-key second)))



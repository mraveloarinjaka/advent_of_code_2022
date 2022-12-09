(ns day-5
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [medley.core :as mcore]))

(def sample-stacks-visual "
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3")

(defn ->stacks
  [stacks]
  (->> stacks
       (map seq)
       (map #(apply list %))
       (zipmap (range))))

(def sample-stacks
  (->stacks ["NZ"
             "DCM"
             "P"])) 

(def sample-moves
"move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn execute-one-move
  [stacks [number from to]]
  (let [from (dec from)
        stack-from (get stacks from)
        to (dec to)
        stack-to (get stacks to)]
    (clojure.core/assert (<= number (count stack-from)))
    (-> stacks
        (update from #(drop number %))
        (update to #(apply conj % (take number stack-from))))))

#_(execute-one-move sample-stacks [1 2 1])

(defn process-moves
  ([stacks moves execute-one-move-fn]
   (->> moves
        str/split-lines
        (remove str/blank?)
        (map (fn parse-moves
               [item]
               (let [[_ number from to] (re-matches #"move (\d+) from (\d+) to (\d+)" item)]
                 [(Integer/parseInt number)
                  (Integer/parseInt from)
                  (Integer/parseInt to)])))
        (reduce execute-one-move-fn stacks)
        ((fn extract-stack-tops
           [unsorted-stacks]
           (for [idx (range (count unsorted-stacks))]
             (first (get unsorted-stacks idx)))))
        (apply str)))
  ([stacks moves]
   (process-moves stacks moves execute-one-move)))

#_(process-moves sample-stacks sample-moves)

(def stacks-day-5-visual "
[M]                     [N] [Z]    
[F]             [R] [Z] [C] [C]    
[C]     [V]     [L] [N] [G] [V]    
[W]     [L]     [T] [H] [V] [F] [H]
[T]     [T] [W] [F] [B] [P] [J] [L]
[D] [L] [H] [J] [C] [G] [S] [R] [M]
[L] [B] [C] [P] [S] [D] [M] [Q] [P]
[B] [N] [J] [S] [Z] [W] [F] [W] [R]
 1   2   3   4   5   6   7   8   9")

(def stacks-day-5
  (->stacks ["MFCWTDLB"
             "LBN"
             "VLTHCJ"
             "WJPS"
             "RLTFCSZ"
             "ZNHBGDW"
             "NCGVPSMF"
             "ZCVFJRQW"
             "HLMPR"]))

#_(->> (slurp (io/resource "day_5.txt"))
     str/split-lines
     (take 5))

(defn print-stacks
  [stacks]
  (doseq [stack stacks]
    (println (first stack) (apply str (second stack)))))

#_(print-stacks sample-stacks)
#_(print-stacks stacks-day-5)

#_(process-moves stacks-day-5 (slurp (io/resource "day_5.txt")))

(defn execute-one-move-9001
  [stacks [number from to]]
  (let [from (dec from)
        stack-from (get stacks from)
        to (dec to)
        stack-to (get stacks to)]
    (-> stacks
        (update from #(drop number %))
        (update to #(concat (take number stack-from) %)))))

#_(process-moves sample-stacks sample-moves execute-one-move-9001)
#_(process-moves stacks-day-5 (slurp (io/resource "day_5.txt")) execute-one-move-9001)

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
  (assoc stacks
         from (drop number stack-from)
         to (apply conj stack-to (take number stack-from)))))

#_(execute-one-move sample-stacks [1 2 1])

(apply conj '(1 2 3) (take 3 '(4 5 6)))

(defn process-moves
  [stacks moves]
  (->> moves
       str/split-lines
       (map (fn parse-moves
              [item] (let [[_ number from to] (re-matches #"move (\d+) from (\d+) to (\d+)" item)]
                       [(Integer/parseInt number)
                        (Integer/parseInt from)
                        (Integer/parseInt to)])))
       (reduce execute-one-move stacks)
       vals
       (map first)
       (apply str)))

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

(->> (slurp (io/resource "day_5.txt"))
     str/split-lines
     (take 5))

#_(process-moves stacks-day-5 (slurp (io/resource "day_5.txt"))) ; "TPGMBSWRN"
; "TPGMBSWRN"

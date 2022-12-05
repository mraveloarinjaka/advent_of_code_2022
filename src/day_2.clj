(ns day-2
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [medley.core :as mcore]))

(def sample
"A Y
B X
C Z")

(def OPPONENT-PLAYS
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def PLAYER-PLAYS
  {"X" :rock
   "Y" :paper
   "Z" :scissors})

(def PLAY-SCORE
  {:win 6
   :draw 3
   :loss 0})

(def PLAYER-PLAY-SCORE
  {:rock 1
   :paper 2
   :scissors 3})

(def PLAYS-TO-OUTCOMES
  {[:rock :rock] :draw
    [:rock :paper] :win
    [:rock :scissors] :loss
    [:paper :paper] :draw
    [:paper :scissors] :win
    [:paper :rock] :loss
    [:scissors :scissors] :draw
    [:scissors :rock] :win
    [:scissors :paper] :loss})

(defn evaluate-play
  [play]
  (get PLAYS-TO-OUTCOMES play))

(defn evaluate-round
  [[_ player-play :as play]]
  (let [result (evaluate-play play) 
        play-score (get PLAY-SCORE result)
        player-play-score (get PLAYER-PLAY-SCORE player-play)]
    {:result result
     :score (+ play-score player-play-score)}))

#_(evaluate-round [:rock :paper])

(defn evaluate-rounds
  [input]
  (->> input
       (str/split-lines)
       (map (fn extract-moves
              [input]
              (let [[_ opponent player] (re-matches #"(\S)\s(\S)" input)]
                [opponent player])))
       (map (fn parse-plays
              [[opponent player]]
              [(get OPPONENT-PLAYS opponent)
               (get PLAYER-PLAYS player)]))
       (map evaluate-round)
       (map :score)
       (reduce +)
       ))

#_(evaluate-rounds sample)
#_(evaluate-rounds (slurp (io/resource "day_2.txt")))

(def OUTCOMES-TO-PLAYS
  (->> (group-by second PLAYS-TO-OUTCOMES)
       (mcore/map-vals #(map first %))))

(def PLAYER-OUTCOMES
  {"X" :loss
   "Y" :draw
   "Z" :win})

(defn retrieve-player-play
  [opponent-play outcome]
  (let [plays (get OUTCOMES-TO-PLAYS outcome)
        play (mcore/find-first #(= opponent-play (first %)) plays)]
    (second play)))

#_(retrieve-player-play :rock :draw)

(defn evaluate-rounds-bis
  [input]
  (->> input
       str/split-lines
       (map (fn extract-moves
              [input]
              (let [[_ opponent player] (re-matches #"(\S)\s(\S)" input)]
                [opponent player])))
       (map (fn parse-plays
              [[opponent outcome]]
              [(get OPPONENT-PLAYS opponent)
               (get PLAYER-OUTCOMES outcome)]))
       (map (fn retrieve-player-plays
              [[opponent-play outcome]]
              [opponent-play (retrieve-player-play opponent-play outcome)]))
       (map evaluate-round)
       (map :score)
       (reduce +)))

#_(evaluate-rounds-bis sample)
#_(evaluate-rounds-bis (slurp (io/resource "day_2.txt")))


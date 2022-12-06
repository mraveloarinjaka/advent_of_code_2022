(ns day-6
  (:require [clojure.java.io :as io]))

(def samples ["bvwbjplbgvbhsrlpgdmjqwftvncz"
              "nppdvjthqldpwncqszvftbrmjlhg"
              "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
              "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"])

(defn valid-marker?
  [marker size-marker]
  (= size-marker (count (set marker))))

(defn find-marker
  [input size-marker]
  (loop [[c & to-process] input
         nb-of-processed 1
         potential-marker []]
    (let [new-potential-marker (conj (if (= size-marker (count potential-marker))
                                       (vec (drop 1 potential-marker)) potential-marker)
                                     c)]
      (cond
        (valid-marker? new-potential-marker size-marker)
        #_> {:marker new-potential-marker :position nb-of-processed}
        (seq to-process)
        #_> (recur to-process (inc nb-of-processed) new-potential-marker)
        :else :none))))

(for [sample samples]
  (find-marker sample 4))

(find-marker (slurp (io/resource "day_6.txt")) 4)
(find-marker (slurp (io/resource "day_6.txt")) 14)

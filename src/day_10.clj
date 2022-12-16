(ns day-10
  (:require
    [medley.core :as mcore]
    [clojure.java.io :as io]
    [clojure.set]
    [clojure.string :as str]))

(def SAMPLE
"noop
addx 3
addx -5")

(def LARGER-SAMPLE
"addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(defn parse-instruction
  [[instruction]]
  (keyword instruction))

(ns-unmap *ns* 'parse)
(defmulti parse parse-instruction :default :noop)

(defmethod parse :noop
  [_]
  :noop)

(defmethod parse :addx
  [[ _ sign value]]
  (let [v (Integer/parseInt value)
        V (if (str/blank? sign) v (* -1 v))]
    [:addx V]))

(defn extract-instruction
  [_context instruction]
  (if (= :noop instruction)
    :noop
    (first instruction)))

(defn ->instructions
  [input]
  (->> input
       str/split-lines
       (map #(re-matches #"\s*(\S+)\s*(-*)(\d*)" %))
       (map rest)
       (map #(parse %))))

(->instructions SAMPLE)
#_(->instructions LARGER-SAMPLE)

(ns-unmap *ns* 'execute)
(defmulti execute extract-instruction :default :noop)

(defmethod execute :noop
  [context _]
  (update context :X-values conj nil))

(defmethod execute :addx
  [context [_ value]]
  (update context :X-values conj nil value))

(def ^:dynamic *DISPLAY* false)

(defn update-on-tick
  [{:keys [X cycle X-values] :as context}]
  (when *DISPLAY* (println context))
  (let [signal-strength (* cycle X)
        [X-value & remaining-X-values] X-values]
    (-> context
        (update :signal-strength conj signal-strength)
        (update :X (if X-value #(+ X-value %) identity))
        (assoc :X-values (vec remaining-X-values))
        (update :cycle inc))))

(defn process-instructions
  [instructions]
  (loop [context {:X 1 :X-values [] :cycle 1 :signal-strength []}
         [instruction & remaining-instructions] instructions]
    (let [new-context (-> context
                          (execute instruction)
                          update-on-tick)]
      (if (seq remaining-instructions)
        (recur new-context remaining-instructions)
        (reduce (fn [ctx _] (update-on-tick ctx)) new-context (get new-context :X-values))))))

(process-instructions (->instructions SAMPLE))
(process-instructions (->instructions LARGER-SAMPLE))


(->> (process-instructions (->instructions LARGER-SAMPLE))
     :signal-strength
     (drop 19)
     (partition 40 40 [])
     (map first)
     (reduce +))

(->> (process-instructions (->instructions (slurp (io/resource "day_10.txt"))))
     :signal-strength
     (drop 19)
     (partition 40 40 [])
     (map first)
     (reduce +))

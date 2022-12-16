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

(def LARGER-SAMPLE (slurp (io/resource "day_10_mid_size.txt")))

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
#_(process-instructions (->instructions LARGER-SAMPLE))

(defn compute-signal-strength
  [input]
  (->> (process-instructions (->instructions input))
       :signal-strength
       (drop 19)
       (partition 40 40 [])
       (map first)
       (reduce +)))

(compute-signal-strength LARGER-SAMPLE)
#_(compute-signal-strength (slurp (io/resource "day_10.txt")))

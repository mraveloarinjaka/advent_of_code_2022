(ns day-11
  (:require
    [medley.core :as mcore]
    [clojure.java.io :as io]
    [clojure.set]
    [clojure.string :as str]))

(def SAMPLE (slurp (io/resource "day_11.sample.txt")))

(defn extract-monkey-id
  [attribute]
  (when-let [[_ id] (re-matches #"Monkey (\d+)" attribute)]
    (Integer/parseInt id)))

(defn extract-attribute
  [attribute _]
  (cond
    (extract-monkey-id attribute) #_> :monkey
    (= attribute "Starting items") #_> :items
    (= attribute "Operation") #_> :operation
    (= attribute "Test") #_> :test
    (= attribute "If true") #_> :test-true
    (= attribute "If false") #_> :test-false
    :else :unknown))

(ns-unmap *ns* 'parse)
(defmulti parse extract-attribute)

(defmethod parse :monkey
  [attribute _]
  [:monkey (extract-monkey-id attribute)])

(defmethod parse :items
  [_ value]
  (let [items (->> (str/split value #",\s*")
                   (mapv #(Integer/parseInt %)))]
    [:items items]))

(defn parse-operation
  [value]
  (let [[_ op arg] (re-matches #"new \= old (.) (old|\S*)" value)]
    (case op
      "+" (fn sum [old] (+ old (Integer/parseInt arg)))
      "*" (fn square [old] (* old old))
      (constantly false))))

(defmethod parse :operation
  [_ value]
  [:worry-level-update (parse-operation value)])

(defmethod parse :test
  [_ value]
  (let [[_ divisor] (re-matches #"divisible by (\d+)" value)]
    [:worry-level-test [:divisible-by? (Integer/parseInt divisor)]]))

(defn parse-test-branch
  [value]
  (let [[_ target-monkey] (re-matches #"throw to monkey (\d+)" value)]
    (Integer/parseInt target-monkey)))

(defmethod parse :test-true
  [_ value]
  [:test-true (parse-test-branch value)])

(defmethod parse :test-false
  [_ value]
  [:test-false (parse-test-branch value)])

(defmethod parse :default
  [attribute value]
  [:noop attribute value])

(def INITIAL-NOTES {:current-monkey nil
                    :monkeys #{}})

(defn parse-attributes
  [{:keys [current-monkey] :as notes} [attribute value]]
  (case attribute
    :monkey (-> notes
                (assoc :current-monkey value)
                (update :monkeys conj value))
    (assoc-in notes [current-monkey attribute] value)))

(->> SAMPLE
     str/split-lines
     (map str/trim)
     (remove str/blank?)
     (map (fn [line] (re-matches #"(.+):\s*(.*)" line)))
     (map (fn [[_ attribute value]] [attribute value]))
     (map #(apply parse %))
     (reduce parse-attributes INITIAL-NOTES)
     (#(dissoc % :current-monkey))
     )

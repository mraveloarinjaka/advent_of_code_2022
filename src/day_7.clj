(ns day-7
  (:require
    [medley.core :as mcore]
    [clojure.java.io :as io]
    [clojure.set]
    [clojure.string :as str]))

(def sample
"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(def ROOT-DIR "/")
(def ROOT-PATH '("/"))

(def initial-state {:cwd ROOT-DIR
                    :cpath ROOT-PATH
                    :cpaths #{}
                    :parent "/"
                    :parents {}
                    :adjacency-list {}})
(defn go-up
  [cpath]
  ;(println :go-up cpath)
  (if (= ROOT-PATH cpath)
    ROOT-PATH
    (drop 1 cpath)))

(defmulti parse-command (fn [_ line] (first line)))

(defmethod parse-command "cd"
  [{:keys [cwd parent parents cpath cpaths] :as ctx} [_ destination]]
  ;(println :parse-command destination "cwd:" cwd "parent:" parent "cpath:" cpath)
  (condp = destination
    ROOT-DIR (assoc ctx
                    :cwd ROOT-DIR
                    :cpath ROOT-PATH
                    :cpaths (clojure.set/union cpaths #{ROOT-PATH})
                    :parent ROOT-DIR)
    ".." (let [new-cpath (go-up cpath)]
           (assoc ctx
                  :cwd parent
                  :cpath new-cpath
                  :cpaths (clojure.set/union cpaths #{new-cpath})
                  :parent (first (go-up new-cpath))))
    (let [new-cpath (conj cpath destination)]
      (assoc ctx
             :cwd destination
             :parent cwd
             :cpath new-cpath
             :cpaths (clojure.set/union cpaths #{new-cpath})
             :parents (assoc parents destination cwd)))))

(defmethod parse-command :default
  [ctx _]
  ctx)

(defn line->entry-type
  [[token]]
  (cond
    (= "$" token) :cmd
    (= "dir" token) :dir
    :else :file))

(defmulti parse (fn [_ line] (line->entry-type line)))

(defmethod parse :cmd
  [ctx line]
  (parse-command ctx (rest line)))

(defmethod parse :dir
  [{:keys [cpath] :as ctx} [_ dir]]
  (update-in ctx [:adjacency-list cpath :dirs] clojure.set/union #{dir}))

(defmethod parse :file
  [{:keys [cpath] :as ctx} [size file]]
  (update-in ctx [:adjacency-list cpath :files] clojure.set/union #{{:file file :size (Integer/parseInt size)}}))

(defmethod parse :default
  [ctx line]
  ctx)

(defn parse-output
  [output]
  (->> output
       str/split-lines
       (map #(str/split % #" "))
       (reduce (fn [ctx line]
                 (parse ctx line))
               initial-state)))

#_(parse-output sample)

(defn size
  [adjacency-list cpath {:keys [dirs files]}]
  (let [files-size (reduce + 0 (map :size files))
        cpaths (for [dir dirs] (conj cpath dir))
        dirs-adjacency-list (select-keys adjacency-list cpaths)
        dirs-sizes (mcore/map-kv-vals (partial size adjacency-list) dirs-adjacency-list)
        dirs-size (reduce + 0 (vals dirs-sizes))]
    (+ files-size dirs-size)))

(defn compute-sizes
  ([output size-fn]
   (let [{:keys [adjacency-list]} (parse-output output)]
     (mcore/map-kv-vals (partial size-fn adjacency-list) adjacency-list)))
  ([output]
   (compute-sizes output size)))

#_ (compute-sizes sample)

#_(->> (parse-output (slurp (io/resource "day_7.txt")))
     :adjacency-list)

(defn terminal-node?
  [[_ {:keys [dirs files]}]]
  (empty? dirs))

(defn compute-size-node
  [[_ {:keys [files]}]]
  (reduce + 0 (map :size files)))

(defn size-iterative
  [adjacency-list cpath content]
  (loop [size 0
         unknowns [[cpath content]]
         max-iter -1]
    ;(println "unknowns:  " unknowns)
    ;(println "treated: " treated)
    (if (or (zero? max-iter) (empty? unknowns))
      size
      (let [[first-unknown & remaining-unknowns] unknowns
            [first-unknown-cpath {:keys [dirs]}] first-unknown
            new-size (+ size (compute-size-node first-unknown))]
        (if (terminal-node? first-unknown)
          (recur new-size remaining-unknowns (dec max-iter))
          (let [cpaths (for [dir dirs] (conj first-unknown-cpath dir))
                new-unknowns (seq (select-keys adjacency-list cpaths))]
            (recur new-size (concat new-unknowns remaining-unknowns) (dec max-iter))))))))

#_(->> (compute-sizes sample size-iterative)
     (filter (fn [[_ dir-size]] (< dir-size 100000)))
     (map second)
     (reduce + 0))

#_(->> (compute-sizes (slurp (io/resource "day_7.txt")) size-iterative)
     (filter (fn [[_ dir-size]] (< dir-size 100000)))
     (map second)
     (reduce + 0))

(def TOTAL-SPACE 70000000)
(def MIN-UNUSED-SPACE 30000000)

(defn to-delete
  [input]
  (let [fs (compute-sizes input size-iterative)
        used-space (get fs ROOT-PATH)]
    (->> fs
         (mcore/map-vals (fn [dir-size] {:dir-size dir-size}))
         (mcore/map-vals (fn [{:keys [dir-size] :as dir-info}]
                           (assoc dir-info :new-used-space (- used-space dir-size))))
         (mcore/map-vals (fn [{:keys [new-used-space] :as dir-info}]
                           (assoc dir-info :new-unused-space (- TOTAL-SPACE new-used-space))))
         (filter (fn [[_ {:keys [new-unused-space] :as dir-info}]]
                   (<= MIN-UNUSED-SPACE new-unused-space)))
         (apply min-key (fn [[_ {:keys [new-unused-space]}]]
                          new-unused-space)))))

(to-delete sample)
(to-delete (slurp (io/resource "day_7.txt")))

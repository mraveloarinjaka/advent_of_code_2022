(ns day-7
  (:require
    [medley.core :as mcore]
    [clojure.java.io :as io]
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

(defmulti parse-command first)

(defmethod parse-command "cd"
  [[_ cwd] ctx]
  {:cwd cwd})

(defmethod parse-command :default
  [cmd ctx]
  {:cmd cmd :ctx ctx})

(defmulti parse first)

(defmethod parse "$"
  [line ctx]
  (parse-command (rest line) ctx))

(defmethod parse :default
  [line ctx]
  {:line line :ctx ctx})

(->> sample
     str/split-lines
     (map #(str/split % #" "))
     (map #(parse % {})))


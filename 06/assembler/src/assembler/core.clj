(ns assembler.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn- first-2-chars [line]
  (if (>= (count line) 2)
      (subs line 0 2)))

(defn- remove-whitespace [file]
  (->> (str/split-lines (slurp file))
       (remove str/blank?)))

(defn- not-comment? [line]
  (-> (str/trim line)
      first-2-chars
      (not= "//")))

(defn -main
  "Entry Point"
  [& args]
  (let [insts (->> (remove-whitespace (first args))
                   (filterv not-comment?))]
    insts))

(-main "/home/johnboy14/coursera/nand2tetris/06/add/Add.asm")

(ns assembler.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn- remove-whitespace [file]
  (->> (str/split-lines (slurp file))
       (remove str/blank?)))

(defn- not-comment? [line]
  (-> (str/trim line)
      (subs 0 2)
      (not= "//")))

(defn -main
  "Entry Point"
  [& args]
  (let [insts (->> (remove-whitespace (first args))
                   (filterv not-comment?))]
    insts))

(-main "/home/johnboy14/coursera/nand2tetris/06/add/Add.asm")

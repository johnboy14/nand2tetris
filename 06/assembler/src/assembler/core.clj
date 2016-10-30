(ns assembler.core
  (:require [clojure.string :as str]
            [clojure.pprint :as p])
  (:gen-class))

(defn- translate-A-instruction
  "Translates A instruction into 16 bit binary representation"
  [inst]
  (->> (map str (rest inst))
       (reduce str)
       read-string
       (p/cl-format nil "~2,15,'0r")
       (str "0")))

(defn- translate-C-instruction [inst]
  "111")

(defn- remove-whitespace [file]
  (->> (str/split-lines (slurp file))
       (remove str/blank?)))

(defn- not-comment? [line]
  (-> (str/trim line)
      (subs 0 2)
      (not= "//")))

(defn- translate-to-binary [inst]
  (case (first inst)
    \@ (translate-A-instruction inst)
    (translate-C-instruction inst)))

(defn -main
  "Entry Point"
  [& args]
  (let [insts (->> (remove-whitespace (first args))
                   (filterv not-comment?)
                   (mapv translate-to-binary))]
    insts))

(-main "/home/johnboy14/coursera/nand2tetris/06/add/Add.asm")

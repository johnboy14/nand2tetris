(ns assembler.core
  (:require [clojure.string :as str]
            [clojure.pprint :as p])
  (:gen-class))

(def comp-table
  {"0"   "0101010"
   "1"   "0111111"
   "-1"  "0111010"
   "D"   "0001100"
   "A"   "0110000"
   "!D"  "0001101"
   "!A"  "0110001"
   "-D"  "0001111"
   "-A"  "0110011"
   "D+1" "0011111"
   "A+1" "0110111"
   "D-1" "0001110"
   "A-1" "0110010"
   "D+A" "0000010"
   "D-A" "0010011"
   "A-D" "0000111"
   "D&A" "0000000"
   "D|A" "0010101"
   "M"   "1110000"
   "!M"  "1110001"
   "-M"  "1110011"
   "M+1" "1110111"
   "M-1" "1110010"
   "D+M" "1000010"
   "D-M" "1010011"
   "M-D" "1000111"
   "D&M" "1000000"
   "D|M" "1010101"})

(def dest-table
  {nil   "000"
   "M"   "001"
   "D"   "010"
   "MD"  "011"
   "A"   "100"
   "AM"  "101"
   "AD"  "110"
   "AMD" "111"})

(def jmp-table
  {nil   "000"
   "JGT" "001"
   "JEQ" "010"
   "JGE" "011"
   "JLT" "100"
   "JNE" "101"
   "JLE" "110"
   "JMP" "111"})

(defn- dest-inst [inst]
  (let [equals-idx (str/index-of inst "=")]
    (if equals-idx
      (subs inst 0 equals-idx))))

(defn- comp-inst [inst]
  (let [equals-idx (str/index-of inst "=")
        equals-idx (if equals-idx (+ 1 equals-idx) 0)
        jmp-prefix (str/index-of inst ";")]
    (if jmp-prefix
      (subs inst equals-idx jmp-prefix)
      (subs inst equals-idx (count inst)))))

(defn- jmp-inst [inst]
  (let [jmp-prefix (str/index-of inst ";")]
    (if jmp-prefix
      (subs inst (+ 1 jmp-prefix) (count inst)))))

(defn- translate-comp-inst [inst])

(defn- translate-C-instruction
  "Translates C instruction into 16 bit binary representation"
  [inst]
  (-> (str "111")
      (str (comp-table (comp-inst inst)))
      (str (dest-table (dest-inst inst)))
      (str (jmp-table (jmp-inst inst)))))

(defn- translate-A-instruction
  "Translates A instruction into 16 bit binary representation"
  [inst]
  (->> (map str (rest inst))
       (reduce str)
       read-string
       (p/cl-format nil "~2,15,'0r")
       (str "0")))

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

(defn- write-to-file [seq file]
  (with-open [wr (clojure.java.io/writer file)]
    (doseq [l seq]
      (.write wr (str l "\n")))))

(defn -main
  "Entry Point"
  [& args]
  (let [insts (->> (remove-whitespace (first args))
                   (filterv not-comment?)
                   (mapv translate-to-binary))]
    (write-to-file insts (second args))
    (println "Translated " (first args) " to file " (second args))))

(comment
  "Assemble Symbol free Assembly programs"
  (-main
    "/home/johnboy14/coursera/nand2tetris/06/add/Add.asm"
    "/home/johnboy14/coursera/nand2tetris/06/add/Add.hack")
  (-main
    "/home/johnboy14/coursera/nand2tetris/06/max/MaxL.asm"
    "/home/johnboy14/coursera/nand2tetris/06/max/MaxL.hack")
  (-main
    "/home/johnboy14/coursera/nand2tetris/06/rect/RectL.asm"
    "/home/johnboy14/coursera/nand2tetris/06/rect/RectL.hack")
  (-main
    "/home/johnboy14/coursera/nand2tetris/06/pong/PongL.asm"
    "/home/johnboy14/coursera/nand2tetris/06/pong/PongL.hack"))

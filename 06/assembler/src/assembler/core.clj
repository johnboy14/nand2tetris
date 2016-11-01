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

(def symbol-table
  {"SP"     "0"
   "LCL"    "1"
   "ARG"    "2"
   "THIS"   "3"
   "THAT"   "4"
   "R0"     "0"
   "R1"     "1"
   "R2"     "2"
   "R3"     "3"
   "R4"     "4"
   "R5"     "5"
   "R6"     "6"
   "R7"     "7"
   "R8"     "8"
   "R9"     "9"
   "R10"    "10"
   "R11"    "11"
   "R12"    "12"
   "R13"    "13"
   "R14"    "14"
   "R15"    "15"
   "SCREEN" "16384"
   "KBD"    "24576"})

(defn- remove-mid-line-comments [line]
  (if-let [idx (str/index-of line "//")]
    (-> (subs line 0 idx)
        str/trim)
    line))

(defn- extract-variable-name [inst]
  (str (reduce str (rest inst))))

(defn- isLabel? [inst]
  (= \( (first inst)))

(defn- isAInst? [inst]
  (= \@ (first inst)))

(defn- not-comment? [line]
  (-> (subs line 0 2)
      (not= "//")))

(defn- not-label? [line]
  (-> (subs line 0 1)
      (not= "(")))

(defn- isVariable? [inst]
  (and (isAInst? inst) (not (Character/isDigit (second inst)))))

(defn- int->16bit-a-inst [n]
  (str "0" (p/cl-format nil "~2,15,'0r" n)))

(defn- dest-inst
  "Retrieve Destination instruction from C Instruction"
  [inst]
  (if-let [equals-idx (str/index-of inst "=")]
    (subs inst 0 equals-idx)))

(defn- comp-inst
  "Retrieve Comp instruction from C Instruction"
  [inst]
  (let [equals-idx (str/index-of inst "=")
        equals-idx (if equals-idx (inc equals-idx) 0)
        jmp-prefix (str/index-of inst ";")]
    (if jmp-prefix
      (subs inst equals-idx jmp-prefix)
      (subs inst equals-idx (count inst)))))

(defn- jmp-inst
  "Retrieve JMP instruction from C Instruction"
  [inst]
  (if-let [jmp-prefix (str/index-of inst ";")]
    (subs inst (+ 1 jmp-prefix) (count inst))))

(defn- translate-C-instruction
  "Translates C instruction into 16 bit binary representation"
  [inst]
  (-> (str "111")
      (str (comp-table (comp-inst inst)))
      (str (dest-table (dest-inst inst)))
      (str (jmp-table (jmp-inst inst)))))

(defn- translate-A-instruction
  "Translates A instruction into 16 bit binary representation"
  [symbol-table inst]
  (let [a-inst (->> (map str (rest inst)) (reduce str))]
    (if (and (isVariable? inst) (contains? symbol-table a-inst))
      (int->16bit-a-inst (read-string (symbol-table a-inst)))
      (int->16bit-a-inst (read-string a-inst)))))

(defn- translate-to-binary [symbol-table inst]
  (if (isAInst? inst)
    (translate-A-instruction symbol-table inst)
    (translate-C-instruction inst)))

(defn- add-labels-to-symbol-table
  "Extract Symbols from Instructions and update symbol table with the new symbol and the
   address of the next instruction"
  ([symbols insts current-inst label-count]
   (if (< current-inst (count insts))
     (let [inst (nth insts current-inst)]
       (if (isLabel? inst)
         (let [label-name (subs inst 1 (dec (count inst)))
               next-inst (str (- (inc current-inst) (inc label-count)))]
           (if-not (contains? symbols label-name)
             (recur (assoc symbols label-name next-inst)
                    insts (inc current-inst) (inc label-count))
             (recur symbols insts (inc current-inst) label-count)))
         (recur symbols insts (inc current-inst) label-count)))
     symbols))
  ([symbols insts]
   (add-labels-to-symbol-table symbols insts 0 0)))

(defn- add-variables-to-symbol-table
  "If A Instruction and non numeric, check symbol table for match,
   else add to symbol table at current memory address"
  ([symbols insts current-inst mem-address]
   (if (< current-inst (count insts))
     (let [inst (nth insts current-inst)
           variable-name (extract-variable-name inst)]
       (if (isVariable? inst)
        (if (contains? symbols variable-name)
          (recur symbols insts (inc current-inst) mem-address)
          (recur (assoc symbols variable-name (str mem-address))
                 insts (inc current-inst) (inc mem-address)))
        (recur symbols insts (inc current-inst) mem-address)))
     symbols))
  ([symbols insts]
   (add-variables-to-symbol-table symbols insts 0 16)))

(defn- write-to-file [seq file]
  (with-open [wr (clojure.java.io/writer file)]
    (doseq [l seq]
      (.write wr (str l "\n")))))

(defn -main
  "Entry Point"
  [& args]
  (let [instructions                (str/split-lines (slurp (first args)))
        without-comments-whitespace (->> (remove str/blank? instructions)
                                         (mapv str/trim)
                                         (filterv not-comment?)
                                         (mapv remove-mid-line-comments)) ;;First Pass
        updated-symbol-table        (->  (add-labels-to-symbol-table symbol-table without-comments-whitespace)
                                         (add-variables-to-symbol-table without-comments-whitespace))
        insts-with-labels           (filterv not-label? without-comments-whitespace);;Second Pass
        insts (->> (mapv #(translate-to-binary updated-symbol-table %) insts-with-labels))] ;;Third Pass
    (write-to-file insts (second args))
    (println "Translated " (first args) " to file " (second args))
    insts))
(ns assembler.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [assembler.core :refer :all]))

(deftest a-test
  (testing "Output for Rect.asm == Rect.cmp file."
    (is (= (-main "dev-resources/Rect.asm" "dev-resources/Rect.hack")
           (str/split-lines (slurp "dev-resources/Rect.cmp"))))
    (is (= (-main "dev-resources/Pong.asm" "dev-resources/Pong.hack")
           (str/split-lines (slurp "dev-resources/Pong.cmp"))))))

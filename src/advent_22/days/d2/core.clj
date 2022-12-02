(ns advent-22.days.d2.core
  (:require [clojure.string :as s]))

(def input
  (slurp "src/advent_22/days/d2/input.txt"))

(def score-by-choice
  {"X" 1   ;; rock
   "Y" 2   ;; paper
   "Z" 3}) ;; scissors

(def abc->xyz
  {"A" "X"
   "B" "Y"
   "C" "Z"})

(defn determine-round-outcome [opp me]
  (cond
    (= opp me) "draw"
    (and (= opp "X") (= me "Y")) "win"
    (and (= opp "X") (= me "Z")) "loss"
    (and (= opp "Y") (= me "X")) "loss"
    (and (= opp "Y") (= me "Z")) "win"
    (and (= opp "Z") (= me "X")) "win"
    (and (= opp "Z") (= me "Y")) "loss"))

(defn determine-round-score 
  [opp-choice my-choice]
  (let [choice-score  (score-by-choice my-choice)
        outcome       (determine-round-outcome (abc->xyz opp-choice) my-choice)
        outcome-score (condp = outcome
                        "draw" 3
                        "win" 6
                        "loss" 0)]
    (+ choice-score outcome-score)))

(defn get-round-pairs []
  (->> input 
       (s/split-lines)
       (map #(s/split % #" "))))

(defn calc-round-scores []
  (->> (get-round-pairs)
       (map #(determine-round-score (first %) (second %)))))

(defn calc-total-score []
  (reduce + (calc-round-scores)))

;; PART 2

(def xyz->outcome
  {"X" "loss"
   "Y" "draw"
   "Z" "win"})

(defn determine-choice [opp outcome]
  (cond
    (= outcome "draw") opp
    (and (= opp "X") (= outcome "win")) "Y"
    (and (= opp "X") (= outcome "loss")) "Z"
    (and (= opp "Y") (= outcome "win")) "Z"
    (and (= opp "Y") (= outcome "loss")) "X"
    (and (= opp "Z") (= outcome "win")) "X"
    (and (= opp "Z") (= outcome "loss")) "Y"))

(defn determine-round-score2
  [opp-choice outcome]
  (let [choice-score  (score-by-choice (determine-choice opp-choice outcome)) 
        outcome-score (condp = outcome
                        "draw" 3
                        "win" 6
                        "loss" 0)]
    (+ choice-score outcome-score)))

(defn calc-round-scores2 []
  (->> (get-round-pairs)
       (map #(determine-round-score2 
              (abc->xyz (first %)) 
              (xyz->outcome (second %))))))

(defn calc-total-score2 []
  (reduce + (calc-round-scores2)))
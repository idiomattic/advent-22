(ns advent-22.days.d1.core
  (:require [clojure.string :as s]))

(def input 
  (slurp "src/advent_22/days/d1/input.txt"))

(defn get-calorie-counts []
  (->> input 
       (s/split-lines)
       (partition-by empty?)
       (filter #(not-every? empty? %))
       (map (fn [list] (reduce + (map #(Integer/parseInt %) list))))))

(defn get-max-calorie-count []
  (apply max (get-calorie-counts)))

(defn get-three-max-calorie-count []
  (reduce 
    +
    (take 3 (sort > (get-calorie-counts)))))
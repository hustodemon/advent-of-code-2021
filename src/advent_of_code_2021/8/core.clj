(ns advent-of-code-2021.8.core
  (:require
   [advent-of-code-2021.utils :as utils]
   [clojure.set :as set]
   [clojure.string :as string]))



(def data (utils/slurp-lines "8/input"))
(def test-data (utils/slurp-lines "8/test-input"))


(defn- parse-grp [g]
  (string/split g #"\s*\s\s*"))


(defn- parse-line [l]
  (map parse-grp (string/split l #"\s*\|\s*")))


(defn- is-easily-decodable? [grp]
  (contains? #{2 3 4 7} (count grp)))

;; todo transducers!
(println "Part 1: "
         (->> data
              (map parse-line)
              (mapcat second)
              (filter is-easily-decodable?)
              (count)))


;; Part 2
(defn- into-set [c]
  (into #{} c))


(defn- parse-grp-2 [g]
  (map into-set (string/split g #"\s*\s\s*")))


(defn- parse-line-2 [l]
  (map parse-grp-2 (string/split l #"\s*\|\s*")))


(def ffilter (comp first filter))


(defn- compute-dictionary [refs] ;; bad time complexity, but whatevs
  (let [;; easy ones 
        one   (ffilter #(= 2 (count %)) refs)
        seven (ffilter #(= 3 (count %)) refs)
        four  (ffilter #(= 4 (count %)) refs)
        eight (ffilter #(= 7 (count %)) refs)
        ;; hard ones, 5 segments
        three (ffilter #(and (= 2 (count (set/difference % seven)))
                             (= 5 (count %)))
                       refs)
        two   (ffilter #(and (= 3 (count (set/difference % four)))
                             (= 5 (count %)))
                       refs)
        five  (ffilter #(and (= 2 (count (set/difference % four)))
                             (= 5 (count %))
                             (not= three %))
                       refs)
        ;; hard ones, 6 segments
        nine  (ffilter #(and (= 2 (count (set/difference % four)))
                             (= 6 (count %)))
                       refs)
        zero  (ffilter #(and (= 2 (count (set/difference % five)))
                             (= 6 (count %)))
                       refs)
        six   (ffilter #(and (= 5 (count (set/difference % one)))
                             (= 6 (count %)))
                       refs)]
    {zero 0, one 1, two 2, three 3, four 4, five 5, six 6, seven 7, eight 8, nine 9}))


(defn- compute-line [[ref input]]
  (let [dict   (compute-dictionary ref)
        digits (map dict input)]
    (->> (reverse digits)
         (map * (iterate (partial * 10) 1))
         (reduce +))))


(defn compute-2 [data]
  (transduce
   (comp
    (map parse-line-2)
    (map compute-line))
   +
   data))


(println "Part 2: " (compute-2 data))


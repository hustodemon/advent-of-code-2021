(ns advent-of-code-2021.9.core
  (:require
   [advent-of-code-2021.utils :as utils]))


(def data (utils/slurp-res "9/input"))

(def test-data
  "2199943210
3987894921
9856789892
8767896789
9899965678")


(defn- size-x [parsed-data]
  (count (take-while #(not= \newline %) parsed-data)))


(defn- parse [s]
  (let [numbers (map (comp read-string str) (remove #(= \newline %) s))
        size-x  (size-x s)
        lines   (partition size-x numbers)]
    ;; we need to convert to nested vectors
    (vec (map vec lines))))


(defn- neighbor-locations [y x]
  [[(inc y) x]
    [(dec y) x]
    [y (inc x)]
    [y (dec x)]])


(defn- neighbors [grid y x]
  (map
   #(get-in grid % 10)
   (neighbor-locations y x)))


(defn- find-sinks [grid]
  (for [x     (range (count (first grid)))
        y     (range (count grid))
        :let  [me (get-in grid [y x])]
        :when (empty? (filter (fn [neigh] (<= neigh me)) (neighbors grid y x)))]
    [y x]))


(defn compute-1 [grid]
  (apply + (map #(inc (get-in grid %))
                (find-sinks grid))))


(println "Part 1: " (compute-1 (parse data)))


;; Part 2, in the end I decided to implement a solution using a search algorithm (not very nice one)
;; (I wanted to come up with a basin-merging algorithm, that would go line-by-line, but I chickened out)
;; it's also pretty ugly, that the points are [y x], not vice-versa, but i don't care now. next time
;; i'll use the {:x X, :y Y} format
(defn dfs [start-point grid]
  (loop [[fst-pt & rest-pts] [start-point]
         done-pts-set        #{}]
    (if fst-pt
      (let [neigh-locations (apply neighbor-locations fst-pt)
            to-explore      (filter (fn [neigh-pos] (< (get-in grid neigh-pos 10) 9)) neigh-locations)
            to-explore      (remove done-pts-set to-explore)
            rest-pts        (concat rest-pts to-explore)]
        (recur rest-pts (conj done-pts-set fst-pt)))
      done-pts-set)))


(defn compute-2 [grid]
  (let [sinks       (find-sinks grid)
        basins (map #(dfs % grid) sinks)
        sizes  (sort > (map count basins))]
    (apply * (take 3 sizes))))


(println "Part 2: " (compute-2 (parse data)))


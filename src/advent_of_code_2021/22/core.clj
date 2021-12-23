(ns advent-of-code-2021.22.core
  (:require
   [advent-of-code-2021.utils :as utils]))


(defn- parse-line [s]
  (let [split  (->> s
                    (re-seq #"(\S+) .=([^\.]+)\.\.([^,]+),.=([^\.]+)\.\.([^,]+),.=([^\.]+)\.\.([^,]+)"  )
                    first
                    rest)
        instr  (-> split first keyword)
        coords (->> split rest (map read-string))]
    (zipmap [:instr :min-x :max-x :min-y :max-y :min-z :max-z]
            (concat [instr] coords))))


(defn- parse-data [f]
  (->> f
       (utils/slurp-lines)
       (map parse-line)))


(def test-data (parse-data "22/test-input"))
(def data (parse-data "22/input"))


(defn- pt-in-cube? [{:keys [x y z]} {:keys [min-x max-x min-y max-y min-z max-z]}]
  (and
   (>= x min-x)
   (<= x max-x)
   (>= y min-y)
   (<= y max-y)
   (>= z min-z)
   (<= z max-z)))


(defn- pt-state-after-step [pt step]
  (if (pt-in-cube? pt step)
    (assoc pt :state (:instr step))
    pt))


(defn compute [data]
  (count
   (for [x (range -50 51)
         y (range -50 51)
         z (range -50 51)
         :let [res (reduce pt-state-after-step {:x x :y y :z z} data)]
         :when (= (:state res) :on)]
     res)))


(println "takes time...")
(println "Part 1: "(time (compute data)))

(ns advent-of-code-2021.17.core)


(def test-data
  {:min-x 20,  :max-x 30
   :min-y -10, :max-y -5})


(def data
  {:min-x 70,  :max-x 125
   :min-y -159, :max-y -121})


;; part 1
;; considering the trajectory, the answer is the sum of sequence
;; this is not really kosher - we assume the area to be below the x axis
;; a general solution could be implementing by "shifting" the area under
;; the x axis
(defn compute-1
  [{:keys [min-y max-y]}]
  {:pre [(neg? min-y)
         (neg? max-y)]}
  (let [max-hit-y-velo (Math/abs (inc min-y))]
    (/ (* (inc max-hit-y-velo)
          max-hit-y-velo)
       2)))


(println "Part 1: " (compute-1 data))


(ns advent-of-code-2021.6.core)


(def test-data [3 4 3 1 2])


(defn cyclic-dec [n]
  (if (< 0 n) (dec n) 6))


(defn step [fish]
  (let [count-producing (count (filter #(= % 0) fish))]
    (concat
     (map cyclic-dec fish)
     (repeat count-producing 8))))


(defn steps [fish] (iterate step fish))


(defn fish-count-after-days [fish count-days]
  (count (nth (steps fish) count-days)))

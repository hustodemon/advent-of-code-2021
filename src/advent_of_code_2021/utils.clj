(ns advent-of-code-2021.utils
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]))


(defn slurp-res [res-path]
  (-> res-path
      io/resource
      slurp))


(defn slurp-lines [res-path]
  (-> res-path
      slurp-res
      string/split-lines))

(ns advent-of-code-2021.utils
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io]))


(defn slurp-lines [res-path]
  (-> res-path
      io/resource
      slurp
      (string/split #"\n")))

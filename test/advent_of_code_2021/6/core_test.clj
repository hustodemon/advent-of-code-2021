(ns advent-of-code-2021.6.core-test
  (:require [advent-of-code-2021.6.core :as sut]
            [clojure.test :as t :refer [deftest testing is]]))


(deftest misc
  (testing "cyclic decrement"
    (is (= 4 (sut/cyclic-dec 5)))
    (is (= 0 (sut/cyclic-dec 1)))
    (is (= 6 (sut/cyclic-dec 0)))
    (is (= 19234 (sut/cyclic-dec 19235))))

  (testing "shift vector"
    (is (= [] (sut/shift-vector [])))
    (is (= [2 3 1] (sut/shift-vector [1 2 3]))))

  (testing "fish to count-time-to-give-birth vector"
    (is (= [0 0 0 0 0 0 0 0 0] (sut/fish->time-to-give-birth-counts [])))
    (is (= [2 3 1 0 0 0 0 0 0] (sut/fish->time-to-give-birth-counts [0 0 1 1 1 2])))))


(deftest computation
  (testing "single step fn"
    (is (= [] (sut/step [])))
    (is (= [1 2 1 6 0 8] (sut/step [2 3 2 0 1])))
    (is (= [0 1 0 5 6 0 1 2 2 3 7 8] (sut/step [1 2 1 6 0 1 2 3 3 4 8]))))

  (testing "fish count fn"
    (is (= 26 (sut/fish-count-after-days sut/test-data 18)))
    (is (= 5934 (sut/fish-count-after-days sut/test-data 80))))

  (testing "part 2: step"
    (is (= [3 4 5 6 7 8 3 0 2] (sut/step-2 [2 3 4 5 6 7 8 1 0])))))

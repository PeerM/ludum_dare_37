(ns ludum_cljs.simulation
  (:require [clojure.test :as test]))

(def inital_state {:miners 2 :bricks 1})
(defn mine_alot [nr_miners] {:pre {:miners (* -1 nr_miners)}},
                             :post {:miners (* 1 nr_miners) :bricks (* 2 nr_miners)})
(def mine (mine_alot 1))
(def make_miner {:pre {:bricks -5}, :post {:miners 1}})
(def example_blocks [mine mine make_miner])
(defn positive_bricks? [state] (<= 0 (:bricks state)))
(def validations [{:name "bricks not negativ" :pred positive_bricks?}])
(defn combine_partial_states [states] (apply merge-with (concat [+] states)))
(defn violated_validation [state validations] (filter (fn [rule] (not ((get rule :pred) state))) validations))
(defn step [state blocks] (let [intermediate (combine_partial_states (concat (map :pre blocks) [state]))
                                validation_errors (violated_validation state validations)
                                result (combine_partial_states (concat (map :post blocks) [intermediate]))]
                            {:result result :validation-errors validation_errors}))




; tests
(test/deftest test_combine_partial_states
  (test/is (= {:miners 1 :bricks 5} (combine_partial_states [{:miners 2} {:miners -1 :bricks 5}]))))

(test/deftest test_step
  (test/is (= {:miners 3 :bricks 0} (:result (step inital_state example_blocks)))))

(test/deftest test_positive_bricks?
  (test/is (true? (positive_bricks? {:bricks 2 :miners 2})))
  (test/is (true? (positive_bricks? {:bricks 0 :miners -2})))
  (test/is (false? (positive_bricks? {:bricks -1 :miners -2}))))

(def test_validations [{:name "bricks must be even" :pred #(even? (:bricks %))}
                       {:name "bricks must be odd" :pred #(odd? (:bricks %))}])
(test/deftest test_violated_validation
  (test/is (= (take 1 test_validations) (violated_validation {:bricks 3} test_validations))))


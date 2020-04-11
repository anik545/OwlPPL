(ns models
  (:use [anglican core emit runtime])
  (:gen-class))

(defquery coin
  (let [theta (sample (uniform-continuous 0 1))] ;; 1. prior
    (observe (binomial 10 theta) 9)
    theta))

(defquery hmm
  (let [
    observations [false false false]
    init-dist    (categorical {true 1.})
    trans-dists  {true  (flip 0.7)
                  false (flip 0.3)}
    obs-dists    {true  (flip 0.9)
                  false (flip 0.1)}]
    (reduce
      (fn [states obs]
        (let [state (sample (get trans-dists
                                 (peek states)))]
          (observe (get obs-dists state) obs)
          (conj states state)))
      [(sample init-dist)]
      observations)
      )
)

(defquery dpmm
  (let [theta (sample (uniform-continuous 0 1))]
    (observe (binomial 10 theta) 9)
    theta))

(defquery linreg
  (let [theta (sample (uniform-continuous 0 1))] 
    (observe (binomial 10 theta) 9)
    theta))

(defquery sprinkler
  (let [theta (sample (uniform-continuous 0 1))] ;; 1. prior
    (observe (binomial 10 theta) 9)
    theta))

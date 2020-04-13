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
  (let [m (sample (normal 0. 2.))
        c (sample (normal 0. 2.))
        observations [[0 0] [1 2] [2 4] [3 6] [4 8] [5 10] [6 12] [7 14] [8 16] [9 18]]]
  (map
    (fn [[x y]] (observe (normal (+ c (* m x)) 1.) y) )
    observations)
    m))

(defquery sprinkler
  (let [is-cloudy (sample (flip 0.5))
        is-raining (cond (= is-cloudy true)
                         (sample (flip 0.8))
                         (= is-cloudy false)
                         (sample (flip 0.2)))
        sprinkler-dist (cond (= is-cloudy true)
                             (flip 0.1)
                             (= is-cloudy false)
                             (flip 0.5))
        wet-grass-dist (cond (or (= is-raining true) (= sprinkler-dist true))
                             (flip 1.)
                             :else (flip 0.))]
    (observe wet-grass-dist true)
    is-raining))
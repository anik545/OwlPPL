(ns models
  (:use [anglican core emit runtime])
  (:gen-class))


(defquery coin
  (let [theta (sample (uniform-continuous 0 1))] ;; 1. prior
    (observe (binomial 10 theta) 9)
    theta))

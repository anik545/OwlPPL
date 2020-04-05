(ns models
  (:use [anglican core emit runtime])
  (:gen-class))


(defquery coin
  (let [prob (sample (beta 1 1))] ;; 1. prior
    (loop [[f & r :as s] [true true false true true]]
      (when (seq s)
        (observe (flip prob) f) ;; 2. incorporate data
        (recur r)))
    prob))

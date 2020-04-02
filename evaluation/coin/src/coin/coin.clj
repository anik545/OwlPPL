(ns coin.coin
  (:use [anglican emit runtime]))

(defquery coin
  (let [bet (sample (beta 5 3))]
    (observe (flip bet) true)
    (predict (> bet 0.7))))

(defmacro time1
  "Evaluates expr and return the time it took."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))
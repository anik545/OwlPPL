(defquery coin
  (let [theta (sample (uniform 0 1))]
    (observe (binomial 10 theta) 9)
    (predict (theta))))

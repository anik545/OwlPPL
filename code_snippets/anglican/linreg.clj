(defquery linreg_model
  (let [m (sample (normal 0. 2.))
        c (sample (normal 0. 2.))
        observations [[0 0] [1 2]
                      [2 4] [3 6]
                      [4 8] [5 10]
                      [6 12] [7 14]]
        ]
    (map
     (fn [[x y]]
       (observe
        (normal (+ c (* m x)) 1.)
        y))
     observations)
    m
  )
)

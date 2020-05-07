(defquery sprinkler
  (let [
    cloudy (sample (flip 0.5))
    rain
    (cond 
      (= cloudy true) (sample (flip 0.8))
      (= cloudy false) (sample (flip 0.2))
    )
    sprinkler
    (cond 
      (= cloudy true) (flip 0.1) 
      (= cloudy false) (flip 0.5))
    wet-grass
    (cond 
      (or (= rain true) (= sprinkler true))
        (flip 1.)
      :else (flip 0.))]
  (observe wet-grass true)
  rain))
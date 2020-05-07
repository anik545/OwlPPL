(defquery hmm
  (let [
    observations [false false false]
    init-dist    (categorical {true 1.})
    transition  {true  (flip 0.7) false (flip 0.3)}
    emission    {true  (flip 0.9) false (flip 0.1)}]
    (reduce
      (fn [states obs]
        (let [state (sample (get transition (peek states)))]
          (observe (get emission state) obs)
          (conj states state)))
      [(sample init-dist)]
      observations)))
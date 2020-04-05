(ns eval_clojure
  (:use [anglican core emit runtime])
  (:require [models])
  (:gen-class))

(defmacro time1
  "Evaluates expr and return the time it took."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn smc [model]
  (doall (->> (doquery :smc model [] :number-of-particles 10000)
   (take 10000)
   (map :result)))
)

(defn mh [model]
  (doall (->> (doquery :lmh model [])
   (take 10000)
   (map :result)))
)

(defn rej [model]
  (doall (->> (doquery :importance model [])
   (take 10000)
   (map :result)))
)

(defn imp [model]
  (doall (->> (doquery :importance model [])
   (take 10000)
   (map :result)))
)

; type of args is arrayseq
(defn timeit [& args]
  (let [model (first args) 
        method (second args)]
  (prn (time1
    ((ns-resolve 'eval_clojure (symbol method))
    (ns-resolve 'models (symbol model)))
      ))
  )
)


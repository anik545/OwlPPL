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

(def n 1000)

(defn smc [model]
  (doall (->> (doquery :smc model [] :number-of-particles 1000)
   (take n)
   (map :result)))
)

(defn mh [model]
  (doall (->> (doquery :lmh model [])
   (take n)
   (map :result)))
)

(defn pimh [model]
  (doall (->> (doquery :pimh model [] :number-of-particles 	1000)
   (take n)
   (map :result)))
)

(defn rej [model]
  (doall (->> (doquery :importance model [])
   (take n)
   (map :result)))
)

(defn imp [model]
  (doall (->> (doquery :importance model [])
   (take n)
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


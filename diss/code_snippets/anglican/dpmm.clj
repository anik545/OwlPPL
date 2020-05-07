

(defm create-sethuraman-stick-breaking-process
  [concentration]
  (let
   []
    sample-index))

(defm DPmem
  [concentration base]
  (let [get-value-from-cache-or-sample
                (mem
         (fn [args stick-index]
           (apply base args)))
        get-stick-picking-procedure-from-cache
        (mem
         (fn [args]
           (create-sethuraman-stick-breaking-process
            concentration)))]
    (fn [& varargs]
      (let [index ((get-stick-picking-procedure-from-cache
                    varargs))]
        (get-value-from-cache-or-sample varargs index)))))

(defquery dp-example-1
  [input-data (list 1. 1.1 1.2 3.1 3.2 3.15 3.24)]
  (let
   [residuals (mem (fn [k] (sample (beta 1 1))))

    sample-index (fn sample-index [k]
                   (if (sample (flip (residuals k)))
                     k
                     (sample-index (+ k 1))))

    vars (mem (fn [i] ((/ 10 (sample (gamma 1. 10.))))))
    means (mem (fn [i] (sample (normal 0 (vars k)))))]

    (map
     (fn [value]
       (observe (apply normal (obs-param)) value))
     input-data)

    ; predict a data point from the DP mixture
    (sample (apply normal (obs-param)))))

open Ppl

(* stochastic recursion *)
let rec geometric' p n = 
  let* c = (bernoulli p) in 
  if c then (return n) else (geometric' p (n+1))

let geometric p = geometric' p 0  


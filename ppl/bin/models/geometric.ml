open Ppl
open Printf
(* stochastic recursion *)
let rec geometric' p n = 
  let* c = (bernoulli p) in 
  if c then (return n) else (geometric' p (n+1))

let geometric p = geometric' p 0  

let g2 = 
  let* x = geometric 0.5 in
  condition (x > 2)
    (return x)
(* Bind(geomer) *)

let g2' = 
  let d = 
    let* x = geometric 0.5 in
    return x
  in

  let d' = condition' (fun x -> if x > 2 then 0. else 1. /. float_of_int x) d in
  d'

(* let h = hist_dist_discrete ~fname:"geo.png" (fmap float_of_int (geometric 0.5))
   let () = Owl_plplot.Plot.output h *)
let inferred = mh' 500 g2
let () = printf "%d " @@ sample inferred 
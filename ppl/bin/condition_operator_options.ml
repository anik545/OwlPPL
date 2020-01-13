open Ppl
open Owl_plplot

(* normal given x>0  (half normal) *)
let d = condition' (fun x -> if Stdlib.(>) x 0. then 0. else 1.) (normal 0. 1.)
let p = prior d
let x = mh' 500 d
let m = sample_mean ~n:1000 x (* ~ 0.79 (sqrt(2)/sqrt(pi)) => giving -0.79???*)

(* standard normal *)
let d' = condition' (fun _ -> 1.) (normal 0. 1.)
let p' = prior d'
let x' = mh' 500 d'
let m' = sample_mean ~n:1000 x' (* ~ 0 *)


(* let h = Owl_stats.(histogram (`N 10) l)
   let h' = Owl_stats.(histogram (`N 10) l') *)

let () = 
  let n = 10000 in 
  let pl = Plot.create ~m:2 ~n:1 "fig.jpg" in

  Plot.subplot pl 0 0;
  let pl = hist_dist_continuous ~h:pl ~n x in

  Plot.subplot pl 1 0;
  let pl = hist_dist_continuous ~h:pl ~n x' in

  Plot.output pl

let model =
  let* a = uniform [0;1] in
  let* b = uniform [0;1] in
  let* c = uniform [0;1] in
  let d = return ((a,b),c) in
  let d = condition' (fun ((a,b),c) -> if a+b+c=3 then 1. else -.Float.infinity) d in
  let* (a,_),_ = d in
  return a

let model1_a =
  let (let+) cond dist = Conditional(cond, dist ()) in 
  let (let*) = (let*) in

  let* a = uniform [0;1] in
  let* b = uniform [0;1] in
  let* c = uniform [0;1] in
  (* an instance of the >> operator in haskell (sequence) in do notation - used in monad-bayes *)
  (* let c = Conditional((fun _ -> (if a+b+c = 3 then 0. else -.infinity)), return a) in
     c *)
  let+ _ = fun _ -> (if a+b+c = 3 then 0. else -.infinity) in
  return a

let model1_b =
  let condition b d = Conditional((fun _ -> if b then 1. else 0.), d) in
  let* a = uniform [0;1] in
  let* b = uniform [0;1] in
  let* c = uniform [0;1] in
  condition (a+b+c=3) 
    (return a)


(* TODO: are both models below the same? *)
let model_desugar_cond_end: int dist =
  Bind(uniform [0;1],fun a ->
      (Bind(uniform [0;1],fun b-> 
           (Bind(uniform [0;1], fun c->
                (Conditional(
                    (fun _ -> if a+b+c=3 then 1. else 0.), 
                    (return a)))
              ))
         ))
    )

let model_desugar_cond_start: (int * int * int) dist  = 
  Conditional((fun (a,b,c) -> if a+b+c=3 then 1. else 0.),
              Bind(uniform [0;1],fun a ->
                  (Bind(uniform [0;1],fun b-> 
                       (Bind(uniform [0;1], fun c-> return (a,b,c)
                            ))
                     ))
                ))

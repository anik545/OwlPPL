open Ppl
open Core
open Models

let models = [
  (single_coin, Primitives.(beta 10. 2.))
]

let infer_strats n = [
  MH(n);
  (* PIMH(n);
     SMC(n);
     PC(n) *)
]
(* Using kl discrete for continuous distributions will diverge with number of samples *)
let diff1 = KL.kl_continuous ~n:10 Primitives.(normal 0. 1.) (normal 0. 1.)
let diff2 = KL.kl_continuous ~n:100 Primitives.(normal 0. 1.) (normal 0. 1.)
let diff3 = KL.kl_continuous ~n:1000 Primitives.(normal 0. 1.) (normal 0. 1.)
let diff4 = KL.kl_continuous ~n:10000 Primitives.(normal 0. 1.) (normal 0. 1.)
let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4
let diff1 = KL.kl_discrete ~n:10 Primitives.(binomial 10 0.5) (binomial_ 10 0.5)
let diff2 = KL.kl_discrete ~n:100 Primitives.(binomial 10 0.5) (binomial_ 10 0.5)
let diff3 = KL.kl_discrete ~n:1000 Primitives.(binomial 10 0.5) (binomial_ 10 0.5)
let diff4 = KL.kl_discrete ~n:10000 Primitives.(binomial 10 0.5) (binomial_ 10 0.5)
let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4
(* let diff5 = KL.kl_continuous ~n:10 Primitives.(beta 10. 2.) (mh' 500 single_coin)
   let () = Printf.printf "%f\n" diff5 *)

let test_model (m,true_dist) =

  let kl_div = fun d -> 
    KL.kl_continuous ~n:10000
      true_dist d
  in

  let inferred_dists = List.map (infer_strats 100) ~f:(fun strat -> strat,infer m strat) in
  let results = List.map inferred_dists ~f:(fun (s,d) -> s,(kl_div d),(sample_mean ~n:1000 d)) in
  (* let strat_kl_divs = List.map (inferred_dists) ~f:(fun x-> fst x,kl_div (snd x)) in

     let means = List.map inferred_dists ~f:(sample_mean ~n:3000) in *)
  let () = List.iter results 
      ~f:(fun (x,y,z) -> 
          Printf.printf("strategy: %s - kl: %f - mean: %f\n") (show_infer_strat x) y z) in
  ()

let () = List.iter models ~f:test_model
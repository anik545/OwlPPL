open Ppl
open Core
open Models

let models = [
  (* tuples of model (posterior), and exact dist *)
  (single_coin, Primitives.(beta 10. 2.))
]

let infer_strats n = [
  MH(n);
  PIMH(n);
  Importance(n);
  (* SMC(n); *)
  (* PC(n) *)
]

let test_model (m,true_dist) =

  let kl_div = fun d -> 
    KL.kl_continuous ~n:1000
      true_dist d
  in

  let inferred_dists = List.map (infer_strats 100) ~f:(fun strat -> strat,infer m strat) in
  let results = List.map inferred_dists ~f:(fun (s,d) -> s,(kl_div d),(sample_mean ~n:1000 d)) in
  (* let strat_kl_divs = List.map (inferred_dists) ~f:(fun x-> fst x,kl_div (snd x)) in

     let means = List.map inferred_dists ~f:(sample_mean ~n:3000) in *)
  let () = 
    List.sort ~compare:(fun (_,y,_) (_,y1,_) -> Float.compare y y1) results |> 
    List.iter 
      ~f:(fun (x,y,z) -> 
          Printf.printf("strategy: %s - kl: %f - mean: %f\n") (show_infer_strat x) y z) in
  ()

let () = List.iter models ~f:test_model
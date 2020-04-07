open Ppl
open Core

let print_kls_continuous model ?(n = 1000) infer_strats true_dist =
  let inferred_dists = List.map infer_strats ~f:(fun x -> (x, infer model x)) in
  let kls =
    List.map inferred_dists ~f:(fun (x, d) ->
        (x, Evaluation.kl_continuous ~n true_dist d))
  in
  List.iter kls ~f:(fun (s, kl) ->
      Printf.printf "%s - kl= %f\n" (show_infer_strat s) kl);
  Printf.printf "\n"

let print_kls_discrete ?(n = 1000) model infer_strats true_dist =
  let inferred_dists = List.map infer_strats ~f:(fun x -> (x, infer model x)) in
  let kls =
    List.map inferred_dists ~f:(fun (x, d) ->
        (x, Evaluation.kl_discrete ~n true_dist d))
  in
  List.iter kls ~f:(fun (s, kl) ->
      Printf.printf "%s - kl=%f\n" (show_infer_strat s) kl);
  Printf.printf "\n"

(* discrete prior, discrete posterior, hard conditioning *)
let sprinkler () =
  let infer_strats =
    [
      Enum; Rejection (100, Hard); Rejection (100, Soft); MH 10; MH 100; MH 1000;
    ]
  in
  let ri =
    Primitives.categorical
      [ (true, 0.707927677329624472); (false, 0.292072322670375473) ]
  in
  let model = Sprinkler.grass_model' in
  print_kls_discrete model infer_strats ri

(* continuous prior, continuous posterior, discrete observation/soft conditioning *)
let single_coin () =
  let ri = Primitives.beta 10. 2. in
  let infer_strats =
    [
      (* Rejection(1000, Hard); Rejection(1000,Soft); 
       RejectionTrans(1000, Hard); RejectionTrans(1000,Soft);  *)
      Importance 100;
      (* MH(10); MH(100); MH(1000);
       SMC(10); SMC(100); SMC(1000);
       PIMH(100);
       PC(100) *)
    ]
  in
  let model = Single_coin.single_coin in
  print_kls_continuous model infer_strats ri ~n:100000

(* let () = sprinkler () *)
let () = single_coin ()

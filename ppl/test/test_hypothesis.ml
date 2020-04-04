open Ppl
open KL
open Core

let coin,coin_soln = Models.(single_coin, single_coin_exact)
let sprinkler,sprinkler_soln = Models.(grass_model, grass_model_exact)

let model_with_soln_discrete = [("sprinkler",sprinkler,sprinkler_soln)]
let model_with_soln_continuous = [("coin",coin,coin_soln)]

(* TODO: get these working with the models *)
let infs = [
  Rejection(300,Soft);
  Importance(100);
  MH(500);
  SMC(100)
]
let infs_ks = [
  Rejection(300,Soft);
  Importance(100);
  MH(500);
  SMC(100)
]

let apply_infs model infs = 
  List.map infs ~f:(fun i -> infer model i)

let do_chi () = 
  let inferreds = 
    List.map 
      model_with_soln_discrete
      ~f:(fun (n,d,exact) -> n,apply_infs d infs,exact) 
  in
  let pvals = 
    List.map 
      inferreds
      ~f:(fun (n,ds,exact) -> n,List.map ds ~f:(fun d -> chi_sq ~n:100000 d exact))
  in
  pvals

let do_ks () = 
  let inferreds = 
    List.map 
      model_with_soln_continuous
      ~f:(fun (n,d,exact) -> n,apply_infs d infs_ks,exact) 
  in
  let pvals = List.map 
      inferreds
      ~f:(fun (n,ds,exact) -> n,List.map ds ~f:(fun d -> kolmogorov_smirnov d exact))
  in
  pvals
;;

let print_headers oc infs = 
  fprintf oc ",";
  let s = String.concat ~sep:"," @@ List.map infs ~f:(fun i -> print_infer_strat i) in
  fprintf oc "%s\n" s

let print_line oc (line: string * (Owl_stats.hypothesis list)) =
  let model_name, pvals = line in
  fprintf oc "%s," model_name;
  let s = String.concat ~sep:"," @@ 
    List.map 
      pvals 
      ~f:(fun p -> string_of_float (Float.round_significant p.p_value ~significant_digits:3)) in
  fprintf oc "%s\n" s

let args = Sys.get_argv ()
let root_dir = "/home/anik/Files/work/project/diss/data"

(* chi *)
let chi () =
  let fname = if Array.length args >= 2 then (Sys.get_argv ()).(1) else sprintf "%s/hypothesis-chi.csv" root_dir in
  let oc = Out_channel.create fname in

  let ks = do_chi () in
  print_headers oc infs;
  List.iter ks ~f:(fun line -> print_line oc line);
  Out_channel.close oc

(* ks *)
let ks () = 
  let fname = if Array.length args >= 3 then (Sys.get_argv ()).(2) else sprintf "%s/hypothesis-ks.csv" root_dir in
  let oc = Out_channel.create fname in
  let ks = do_ks () in
  print_headers oc infs_ks;
  List.iter ks ~f:(fun line -> print_line oc line);
  Out_channel.close oc

let () = chi ()
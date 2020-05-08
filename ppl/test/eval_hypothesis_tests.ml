(* Performs hypothesis tests to check inference and writes data to a csv to be plotted *)

open Ppl
open Evaluation
open Core

let root_dir = "/home/anik/Files/work/project/diss/data/hypothesis"

let coin, coin_soln = Models.(single_coin, single_coin_exact)

let sprinkler, sprinkler_soln = Models.(grass_model, grass_model_exact)

let hmm, hmm_soln = Models.(hmm_1, hmm_1_exact)

let linreg, linreg_soln = Models.(linreg_m, linreg_m_exact)

let models_with_soln_discrete =
  [ ("Sprinkler", sprinkler, sprinkler_soln); ("HMM", hmm, hmm_soln) ]

let models_with_soln_continuous =
  [ ("Biased Coin", coin, coin_soln) (* ("linreg", linreg, linreg_soln)  *) ]

(* TODO: get these working with the models *)
let infs_chi =
  [ Rejection (300, Soft); Importance 100; MH 500; SMC 100; PC 100; PIMH 10 ]

let infs_ks =
  [ Rejection (300, Soft); Importance 1000; MH 500; SMC 100; PC 100; PIMH 10 ]

let apply_infs model infs = List.map infs ~f:(fun i -> infer model i)

type 'a hyp_test =
  ?n:int -> ?alpha:float -> 'a dist -> 'a Primitive.t -> Owl_stats.hypothesis

let run_test ?(n' = 100000) (method' : 'a hyp_test) models infs =
  let inferreds =
    List.map models ~f:(fun (n, d, exact) -> (n, apply_infs d infs, exact))
  in
  let pvals =
    List.map inferreds ~f:(fun (n, ds, exact) ->
        printf "!!!!!!%s!!!!!!\n%!" n;
        let x = ref 0 in
        ( n,
          List.map ds ~f:(fun d ->
              printf "----- %s ----\n%!"
                (show_infer_strat (List.nth_exn infs !x));
              incr x;
              method' ~n:n' d exact) ))
  in
  pvals

let print_headers oc infs =
  fprintf oc ",";
  let s =
    String.concat ~sep:"," @@ List.map infs ~f:(fun i -> print_infer_strat i)
  in
  fprintf oc "%s\n" s

let print_line oc (line : string * Owl_stats.hypothesis list) =
  let model_name, pvals = line in
  fprintf oc "%s," model_name;
  let s =
    String.concat ~sep:","
    @@ List.map pvals ~f:(fun p ->
           string_of_float
             (Float.round_significant p.p_value ~significant_digits:3))
  in
  fprintf oc "%s\n" s

let print_test_to_file method' models infs fname =
  let fname = sprintf "%s/%s" root_dir fname in
  let ks = run_test ~n':50 method' models infs in
  let oc = Out_channel.create fname in
  print_headers oc infs;
  List.iter ks ~f:(fun line -> print_line oc line);
  Out_channel.close oc

let args = Sys.get_argv ()

let () =
  match args.(1) with
  | "chi" ->
      print_test_to_file chi_sq models_with_soln_discrete infs_chi
        "hypothesis-chi1.csv"
  | "ks" ->
      print_test_to_file kolmogorov_smirnov models_with_soln_continuous infs_ks
        "hypothesis-ks1.csv"
  | _ -> printf "invalid test type"

(* chi () *)

open Ppl
open Core

let root_dir = "/home/anik/Files/work/project/diss/data/perf/by_datasize"

let string_of_float f = sprintf "%.15f" f

let print_2d arr =
  Array.iter
    ~f:(fun r ->
      Array.iter ~f:(printf "%f,") r;
      printf "\n")
    arr

(* save a string matrix (2d array) as a csv file *)
let write_to_csv ~fname data =
  let oc = Out_channel.create (sprintf "%s/%s" root_dir fname) in
  Array.iter data ~f:(fun line ->
      fprintf oc "%s" @@ String.concat ~sep:"," (Array.to_list line);
      fprintf oc "\n");
  Out_channel.close oc

let x_vals = Owl.Arr.(to_array (logspace ~base:10. 2. 4. 5))

let use_model m i () =
  printf "doing model\n%!";
  let n = 1000 in
  let inf = infer m i in
  let x = take_k_samples n inf in
  x

let by_particles model inf =
  let open Owl.Arr in
  let header = [| "n"; print_infer_strat_short (inf 0) |] in
  let particles = logspace ~base:10. 2. 4. 5 in
  let x =
    map
      (fun n ->
        printf "n: %f\n" n;
        snd @@ time (use_model model (inf (int_of_float n))))
      particles
    |> to_array
    |> Array.map ~f:string_of_float
  in
  let pstrs = Array.map ~f:string_of_float (to_array particles) in
  Array.append [| header |] (Array.transpose_exn @@ [| x; pstrs |])

let map_2d arr ~f = Array.map arr ~f:(fun a -> Array.map a ~f)

(* generate data to condition on, memoise to ensure same lists are used *)
let list_gen =
  memo (fun n ->
      List.init n ~f:(fun n ->
          let n = float_of_int n in
          (n, (n *. 2.) +. 1.)))

let by_data_length_linreg () =
  (* lengths of data *)
  let ns = Owl.Arr.to_array @@ Owl.Arr.linspace 1. 10000. 20 in
  let infs = [| Rejection (100, Soft); Importance 100; MH 500; SMC 50 |] in

  let header =
    Array.append [| "n" |] (Array.map infs ~f:print_infer_strat_short)
  in

  (* generate data to condition on, memoise to ensure same lists are used *)
  let list_gen =
    memo (fun n ->
        List.init n ~f:(fun n ->
            let n = float_of_int n in
            (n, (n *. 2.) +. 1.)))
  in

  let model : (float * float) list -> (float * float * float) dist =
    Models.linreg
  in

  let d =
    Array.map infs ~f:(fun inf_strat ->
        Array.map ns ~f:(fun n ->
            printf "inf: %s, datasize: %f\n"
              (print_infer_strat_short inf_strat)
              n;
            snd
            @@ time (use_model (model (list_gen (int_of_float n))) inf_strat)))
  in
  let data = Array.transpose_exn @@ Array.append [| ns |] d in
  Array.append [| header |] (map_2d data ~f:string_of_float)

let by_data_length_linreg_single_inf ?(num_times = 3) ?(num_x = 20) inf :
    string array array =
  (* lengths of data *)
  let ns = Owl.Arr.to_array @@ Owl.Arr.linspace 1. 10000. num_x in

  let header = [| "n"; print_infer_strat_short inf; "err" |] in

  (* generate data to condition on, memoise to ensure same lists are used *)
  let list_gen =
    memo (fun n ->
        List.init n ~f:(fun n ->
            let n = float_of_int n in
            (n, (n *. 2.) +. 1.)))
  in

  let model : (float * float) list -> (float * float * float) dist =
    Models.linreg
  in

  let mean_std_of_times n f =
    let arr = Array.init n ~f:(fun _ -> snd @@ time f) in
    let mean = Owl_stats.mean arr in
    [| mean; 1.96 *. Owl_stats.std ~mean arr |]
  in
  let d =
    Array.map ns ~f:(fun n ->
        printf "inf: %s, datasize: %f\n" (print_infer_strat_short inf) n;
        let f = use_model (model (list_gen (int_of_float n))) inf in
        mean_std_of_times num_times f)
  in
  (print_2d @@ Array.(transpose_exn @@ append [| ns |] @@ transpose_exn d));
  let data = Array.(transpose_exn @@ append [| ns |] @@ transpose_exn d) in
  Array.append [| header |] (map_2d data ~f:string_of_float)

let by_data_length_dpmm_single_inf ?(num_times = 3) ?(num_x = 20) inf :
    string array array =
  (* lengths of data *)
  let ns = Owl.Arr.to_array @@ Owl.Arr.linspace 1. 10000. num_x in

  let header = [| "n"; print_infer_strat_short inf; "err" |] in

  (* generate data to condition on, memoise to ensure same lists are used *)
  let list_gen =
    memo (fun n -> List.init n ~f:(fun _ -> Owl_stats.uniform_rvs ~a:0. ~b:10.))
  in

  let model : float list -> (float * float) list dist =
    Models.cluster_parameters
  in

  let mean_std_of_times n f =
    let arr = Array.init n ~f:(fun _ -> snd @@ time f) in
    let mean = Owl_stats.mean arr in
    [| mean; 1.96 *. Owl_stats.std ~mean arr |]
  in
  let d =
    Array.map ns ~f:(fun n ->
        printf "inf: %s, datasize: %f\n" (print_infer_strat_short inf) n;
        let f = use_model (model (list_gen (int_of_float n))) inf in
        mean_std_of_times num_times f)
  in
  (print_2d @@ Array.(transpose_exn @@ append [| ns |] @@ transpose_exn d));
  let data = Array.(transpose_exn @@ append [| ns |] @@ transpose_exn d) in
  Array.append [| header |] (map_2d data ~f:string_of_float)

let mh, imp, rej, smc, pc, pimh =
  (MH 500, Importance 50, Rejection (100, Soft), SMC 50, PC 10, PIMH 10)

let str_to_inf = function
  | "mh" -> mh
  | "imp" -> imp
  | "rej" -> rej
  | "smc" -> smc
  | "pc" -> pc
  | "pimh" -> pimh
  | s -> failwith (s ^ " is not an inference method")

let infs =
  if Array.length @@ Sys.get_argv () > 1 then
    Array.sub (Sys.get_argv ()) ~pos:1 ~len:(Array.length (Sys.get_argv ()) - 1)
    |> Array.map ~f:str_to_inf
  else [| mh; rej; imp; smc |]

(* let _ =
  Array.map infs ~f:(fun inf ->
      by_data_length_linreg_single_inf inf ~num_times:10
      |> write_to_csv
           ~fname:
             ("linreg_by_data_length_" ^ print_infer_strat_short inf ^ ".csv")) *)

let _ =
  Array.map infs ~f:(fun inf ->
      by_data_length_dpmm_single_inf inf ~num_times:2
      |> write_to_csv
           ~fname:("dpmm_by_data_length_" ^ print_infer_strat_short inf ^ ".csv"))

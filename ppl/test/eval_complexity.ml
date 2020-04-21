open Ppl
open Core

let root_dir = "/home/anik/Files/work/project/diss/data/"

let string_of_float f = sprintf "%.15f" f

(* save a string matrix (2d array) as a csv file *)
let write_to_csv ~fname data =
  let oc = Out_channel.create (root_dir ^ fname) in
  Array.iter data ~f:(fun line ->
      Array.iter line ~f:(fprintf oc "%s,");
      fprintf oc "\n");
  (* for i = 1 to Array.length data do
     for j = 1 to Array.length data.(i) do
      fprintf oc "%s," data.(i).(j)
      done;
      fprintf oc ";\n";
      done; *)
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
  let ns = Owl.Arr.to_array @@ Owl.Arr.linspace 1. 1000. 5 in
  let infs =
    [|
      Rejection (100, Soft);
      Importance 100
      (* MH 500;  *)
      (* SMC 100  *);
    |]
  in

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

(* let () =
   by_particles Models.single_coin (fun n -> SMC n)
   |> write_to_csv ~fname:"smc_by_particles"


   let () =
   by_particles Models.single_coin (fun n -> PIMH n)
   |> write_to_csv ~fname:"pimh_by_particles" *)

(* let x = sample_mean ~n:20 @@ fmap fst3 @@ infer (Models.linreg (list_gen(100000))) (MH 500) *)
(* let x = sample_mean ~n:20000 @@ infer (Models.single_coin) (MH 10000) *)
(* let ()  = printf "%f\n" @@ x *)

let () =
  by_data_length_linreg () |> write_to_csv ~fname:"linreg_by_data_length.csv"

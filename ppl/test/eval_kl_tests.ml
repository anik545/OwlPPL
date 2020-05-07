(* Runs KL diverence on inferred distributions to check correctness of inference *)
open Ppl
open Core

type 'a kl_method =
  | Not_Cumulative of (?n:int -> 'a Primitive.t -> 'a dist -> float)
  | Cumulative of
      (int array -> 'a Primitive.t -> 'a dist -> (int * float) array)

let p xs =
  let one_col_mat xs = Owl.(Mat.of_array xs (Array.length xs) 1) in
  let ( @@@ ) = Fn.compose in
  let open Owl_plplot in
  let h = Plot.create "b.png" in
  let x = one_col_mat @@ Array.map ~f:(float_of_int @@@ fst) xs in
  let y = one_col_mat (Array.map ~f:(Float.abs @@@ fst @@@ snd) xs) in
  Plot.loglog ~h ~x y;
  Plot.output h

let moving_average n arr =
  let l = Array.to_list arr in
  let n = float_of_int n in
  let open Float in
  if float_of_int (List.length l) < n then failwith "List is too small"
  else
    let rec aux2 acc i = function
      | hd :: tl when i < n -> aux2 (acc + hd) (i + 1.) tl
      | _ -> acc / n
    in
    let rec aux acc l =
      match l with
      | [] -> List.rev acc
      | _ :: tl ->
          let avgn = aux2 0. 0. l in
          if float_of_int (List.length tl) < n then List.rev (avgn :: acc)
          else aux (avgn :: acc) tl
    in
    List.to_array @@ aux [] l

let ( @@@ ) = Core.Fn.compose

let means_stds ?(n = 20) (f : int -> (int * float) array) =
  let xs' =
    Array.init n ~f:(fun _ ->
        printf ".%!";
        f 0)
  in
  let idxs = Array.map ~f:fst xs'.(0) in
  let xs't = Array.transpose_exn xs' in
  let means =
    Array.map ~f:(fun arr -> Array.map ~f:snd arr |> Owl_stats.mean) xs't
  in
  let stds =
    Array.map ~f:(fun arr -> Array.map ~f:snd arr |> Owl_stats.std) xs't
  in
  printf "\n";
  Array.zip_exn idxs (Array.zip_exn means stds)

let root_dir = "/home/anik/Files/work/project/diss/data/kl"

let write_to_csv ~fname data =
  let oc = Out_channel.create (sprintf "%s/%s" root_dir fname) in
  Array.iter data ~f:(fun line ->
      fprintf oc "%s" @@ String.concat ~sep:"," (Array.to_list line);
      fprintf oc "\n");
  Out_channel.close oc

(* For generating csv to be plotted *)
(* create array of x,y pairs *)
let gen_csv ?(window = 10) ?(runs = 5) ~fname model exact infer_strat kl_method
    x_vals =
  printf "%s\n%!" (print_infer_strat infer_strat);
  let _ = window in
  let inferred = infer model infer_strat in
  let arr =
    match kl_method with
    | Cumulative kl_method ->
        let f _ = kl_method x_vals exact inferred in
        let arr = means_stds ~n:runs f in
        let mns =
          Array.map ~f:(fun (_, (mn, _)) -> mn) arr |> moving_average window
        in
        let stds =
          Array.map ~f:(fun (_, (_, std)) -> std) arr |> moving_average window
        in

        let x_vals =
          Array.sub x_vals ~pos:0 ~len:(Array.length x_vals - (window - 1))
        in
        Array.zip_exn x_vals (Array.zip_exn mns stds)
    (* kl_method x_vals exact inferred *)
    | Not_Cumulative kl_method ->
        let _ = kl_method in
        failwith "ohno"
    (* Array.map x_vals ~f:(fun n -> (n, kl_method ~n exact inferred)) *)
  in
  let name = sprintf "%s/%s" root_dir fname in
  let oc = Out_channel.create name in
  p arr;
  fprintf oc "n,kl,err\n";
  Array.iter arr ~f:(fun (x, (mean, std)) ->
      fprintf oc "%d,%f,%f\n" x (Float.abs mean) (Float.abs (2. *. std)));
  Out_channel.close oc

open Models

let kl_to_use = Cumulative Evaluation.kl_cum_discrete

let x_vals =
  Owl.Arr.to_array (Owl.Arr.logspace ~base:10. 2. 5. 100)
  |> Array.map ~f:(fun x -> int_of_float x)

let mh, imp, rej, smc, pimh, pc =
  (MH 1000, Importance 10, Rejection (10, Soft), SMC 10, PIMH 10, PC 10)

let str_to_inf = function
  | "mh" -> mh
  | "imp" -> imp
  | "rej" -> rej
  | "smc" -> smc
  | "pc" -> pc
  | "pimh" -> pimh
  | s -> failwith (s ^ " is not an inference method")

let infs =
  if Array.length @@ Sys.get_argv () > 2 then
    Array.sub (Sys.get_argv ()) ~pos:2 ~len:(Array.length (Sys.get_argv ()) - 2)
    |> Array.map ~f:str_to_inf
  else [| mh; rej; imp; smc |]

let gen_for_inf inf model_name (model : 'a dist) (soln : 'a Primitive.t)
    (kl : 'a kl_method) =
  let fname = sprintf "%s_%s.csv" (print_infer_strat_short inf) model_name in
  gen_csv ~fname ~window:10 ~runs:5 model soln inf kl x_vals

let kld = Cumulative Evaluation.kl_cum_discrete

let klc = Cumulative Evaluation.kl_cum_continuous

let _ =
  match (Sys.get_argv ()).(1) with
  | "coin" ->
      Array.map
        ~f:(fun i -> gen_for_inf i "coin" single_coin single_coin_exact klc)
        infs
  | "sprinkler" ->
      Array.map
        ~f:(fun i ->
          gen_for_inf i "sprinkler" grass_model grass_model_exact kld)
        infs
  | _ -> failwith "invalid model"

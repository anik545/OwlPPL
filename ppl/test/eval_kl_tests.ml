(* Runs KL diverence on inferred distributions to check correctness of inference *)

open Ppl
open Core

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

let mov len xs =
  let xs = Array.map ~f:fst xs in
  let xs' = Array.sub xs ~pos:0 ~len:(Array.length xs - (len - 1)) in
  let ys = Array.map ~f:snd xs in
  let ys' = moving_average len ys in
  (Array.zip_exn xs', ys')

(* This stuff runs kl divergence function on models *)
(* let x_vals = [|100;500;1000;10000|] *)
let x_vals = Owl.Arr.to_array (Owl.Arr.logspace ~base:10. 2. 5. 1000)

let x_vals = Array.map x_vals ~f:(fun x -> int_of_float x)

let root_dir = "/home/anik/Files/work/project/diss/data/kl"

type 'a kl_method =
  | Not_Cumulative of (?n:int -> 'a Primitive.t -> 'a dist -> float)
  | Cumulative of
      (int array -> 'a Primitive.t -> 'a dist -> (int * float) array)

(* For generating csv to be plotted *)
(* create array of x,y pairs *)
let gen_csv ?fname model exact infer_strat kl_method =
  let inferred = infer model infer_strat in
  let arr =
    match kl_method with
    | Cumulative kl_method -> kl_method x_vals exact inferred
    | Not_Cumulative kl_method ->
        Array.map x_vals ~f:(fun n -> (n, kl_method ~n exact inferred))
  in
  (* let arr = kl_method x_vals exact inferred in *)
  match fname with
  | Some name ->
      let name = sprintf "%s/%s" root_dir name in
      let oc = Out_channel.create name in
      Array.iter arr ~f:(fun (x, y) -> fprintf oc "%d,%f\n" x (Float.abs y));
      let () = Out_channel.close oc in
      arr
  | None -> arr

let gen_csv' ?fname ?(window = 1) model exact infer_strats kl_method =
  let x_vals =
    Owl.Arr.to_array (Owl.Arr.logspace ~base:10. 2. 5. 100)
    |> Array.map ~f:(fun x -> int_of_float x)
  in

  let inferreds = Array.map ~f:(fun i -> infer model i) infer_strats in
  let arr =
    Array.transpose_exn
    @@ Array.mapi inferreds ~f:(fun i inf ->
           printf "\n%s%!" (show_infer_strat infer_strats.(i));
           match kl_method with
           | Cumulative kl_method ->
               moving_average window
               @@ Array.map ~f:(fun (a, b) ->
                      printf "%d\n" a;
                      b)
               @@ kl_method x_vals exact inf
           | Not_Cumulative kl_method ->
               Array.map x_vals ~f:(fun n -> kl_method ~n exact inf)
           (* Array.map x_vals ~f:(fun n -> kl_method ~n exact inf) *))
  in
  let x_vals =
    Array.sub x_vals ~pos:0 ~len:(Array.length x_vals - (window - 1))
  in
  match fname with
  | Some name ->
      let name = sprintf "%s/%s" root_dir name in
      let oc = Out_channel.create name in
      let s =
        String.concat ~sep:","
          (List.map ~f:print_infer_strat_short @@ Array.to_list infer_strats)
      in
      fprintf oc "samples,%s\n" s;
      Array.iteri arr ~f:(fun i a ->
          fprintf oc "%d,%s\n" x_vals.(i)
            (String.concat ~sep:","
               ( Array.to_list
               @@ Array.map a ~f:(fun x -> string_of_float (Float.abs x)) )));
      let () = Out_channel.close oc in
      arr
  | None -> arr

open Models

(* coin *)
(* let _ = gen_csv ~fname:"coin_mh.csv" single_coin single_coin_exact (MH 500) Evaluation.kl_continuous *)
(* let _ = gen_csv ~fname:"coin_rej.csv" single_coin single_coin_exact (Rejection(300,Soft)) Evaluation.kl_continuous *)
(* let _ = gen_csv ~fname:"coin_imp.csv" single_coin single_coin_exact (Importance 500) Evaluation.kl_continuous *)
(* let _ = gen_csv ~fname:"coin_smc.csv" single_coin single_coin_exact (SMC 500) Evaluation.kl_continuous *)

let is = [| MH 500; Rejection (300, Soft); Importance 500; SMC 100 |]

let _ =
  gen_csv' ~fname:"kl_coin_all.csv" ~window:5 single_coin single_coin_exact is
    (Cumulative Evaluation.kl_cum_continuous)

(* sprinkler *)
(* let _ =
   gen_csv ~fname:"sprinkler_mh.csv" grass_model grass_model_exact (MH 500)
    (Cumulative Evaluation.kl_cum_discrete)

   let _ =
   gen_csv ~fname:"sprinkler_rej.csv" grass_model grass_model_exact
    (Rejection (300, Soft))
    (Cumulative Evaluation.kl_cum_discrete)

   let _ =
   gen_csv ~fname:"sprinkler_imp.csv" grass_model grass_model_exact
    (Importance 500) (Cumulative Evaluation.kl_cum_discrete)

   let _ =
   gen_csv ~fname:"sprinkler_smc.csv" grass_model grass_model_exact (SMC 500)
    (Cumulative Evaluation.kl_cum_discrete) *)

(* let _ = gen_csv' ~fname:"kl_sprinkler_all.csv" ~window:10 grass_model grass_model_exact is (Cumulative Evaluation.kl_cum_discrete) *)

(* This stuff tests that the kl divergence function works *)
let test_kl_function () =
  (* Using kl discrete for continuous distributions will diverge with number of samples *)
  (* converge to 0 *)
  let diff1 =
    Evaluation.kl_continuous ~n:10 Primitive.(normal 0. 1.) (normal 0. 1.)
  in
  let diff2 =
    Evaluation.kl_continuous ~n:100 Primitive.(normal 0. 1.) (normal 0. 1.)
  in
  let diff3 =
    Evaluation.kl_continuous ~n:1000 Primitive.(normal 0. 1.) (normal 0. 1.)
  in
  let diff4 =
    Evaluation.kl_continuous ~n:10000 Primitive.(normal 0. 1.) (normal 0. 1.)
  in
  let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4 in
  (* diverges to ?? *)
  let diff1 =
    Evaluation.kl_continuous ~n:10
      Primitive.(continuous_uniform 0. 1.)
      (beta 10. 2.)
  in
  let diff2 =
    Evaluation.kl_continuous ~n:100
      Primitive.(continuous_uniform 0. 1.)
      (beta 10. 2.)
  in
  let diff3 =
    Evaluation.kl_continuous ~n:1000
      Primitive.(continuous_uniform 0. 1.)
      (beta 10. 2.)
  in
  let diff4 =
    Evaluation.kl_continuous ~n:10000
      Primitive.(continuous_uniform 0. 1.)
      (beta 10. 2.)
  in
  let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4 in
  (* converge to 0 *)
  let diff1 =
    Evaluation.kl_discrete ~n:10 Primitive.(binomial 10 0.5) (binomial 10 0.5)
  in
  let diff2 =
    Evaluation.kl_discrete ~n:100 Primitive.(binomial 10 0.5) (binomial 10 0.5)
  in
  let diff3 =
    Evaluation.kl_discrete ~n:1000 Primitive.(binomial 10 0.5) (binomial 10 0.5)
  in
  let diff4 =
    Evaluation.kl_discrete ~n:10000
      Primitive.(binomial 10 0.5)
      (binomial 10 0.5)
  in
  let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4 in
  (* converge to 0.5  *)
  let diff1 =
    Evaluation.kl_discrete ~n:10
      Primitive.(discrete_uniform [ 0; 1 ])
      (binomial 1 0.9)
  in
  let diff2 =
    Evaluation.kl_discrete ~n:100
      Primitive.(discrete_uniform [ 0; 1 ])
      (binomial 1 0.9)
  in
  let diff3 =
    Evaluation.kl_discrete ~n:1000
      Primitive.(discrete_uniform [ 0; 1 ])
      (binomial 1 0.9)
  in
  let diff4 =
    Evaluation.kl_discrete ~n:10000
      Primitive.(discrete_uniform [ 0; 1 ])
      (binomial 1 0.9)
  in
  let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4 in
  ()

open Ppl
open Core

(* This stuff runs kl divergence function on models *)
(* let x_vals = [|100;500;1000;10000|] *)
let x_vals = Owl.Arr.to_array (Owl.Arr.logspace ~base:10. 2. 4. 5)
let x_vals = Array.map x_vals ~f:(fun x -> int_of_float x)

let root_dir = "/home/anik/Files/work/project/diss/data"

(* For generating csv to be plotted *)
(* create array of x,y pairs *)
let gen_csv ?fname model exact infer_strat 
    (kl_method:?n:int -> 'a Primitives.primitive -> 'a dist -> likelihood) = 
  let inferred = infer model infer_strat in
  let arr = Array.map x_vals ~f:(fun n -> n, kl_method ~n exact inferred) in
  match fname with
  | Some name -> 
    let name = sprintf "%s/%s" root_dir name in
    let oc = Out_channel.create name in
    Array.iter arr ~f:(fun (x,y) -> fprintf oc "%d,%f\n" x (Float.abs y));
    let () = Out_channel.close oc in
    arr
  | None -> arr

let gen_csv' ?fname model exact infer_strats
    (kl_method:?n:int -> 'a Primitives.primitive -> 'a dist -> likelihood) = 
  let inferreds = Array.map ~f:(fun i -> infer model i) infer_strats in
  let arr = Array.transpose @@
    Array.map inferreds ~f:(fun inf -> 
        Array.map x_vals ~f:(fun n -> kl_method ~n exact inf))
  in
  let arr = Option.value_exn arr in
  match fname with
  | Some name -> 
    let name = sprintf "%s/%s" root_dir name in
    let oc = Out_channel.create name in
    let s = String.concat ~sep:"," (List.map ~f:(print_infer_strat_short) @@ Array.to_list infer_strats) in
    fprintf oc "samples,%s\n" s;

    Array.iteri arr ~f:(fun i a ->
        fprintf oc "%d,%s\n"
          x_vals.(i)
          (String.concat
             ~sep:","
             (Array.to_list @@
              Array.map a
                ~f:(fun x-> string_of_float (Float.abs x)))
          )
      );

    let () = Out_channel.close oc in
    arr
  | None -> arr

open Models
(* coin *)
(* let _ = gen_csv ~fname:"coin_mh.csv" single_coin single_coin_exact (MH 500) KL.kl_continuous *)
(* let _ = gen_csv ~fname:"coin_rej.csv" single_coin single_coin_exact (Rejection(300,Soft)) KL.kl_continuous *)
(* let _ = gen_csv ~fname:"coin_imp.csv" single_coin single_coin_exact (Importance 500) KL.kl_continuous *)
(* let _ = gen_csv ~fname:"coin_smc.csv" single_coin single_coin_exact (SMC 500) KL.kl_continuous *)

(* let is = [|MH 500;Rejection(300,Soft);Importance 500;SMC 500|] *)
(* let _ = gen_csv' ~fname:"kl_coin_all.csv" single_coin single_coin_exact is KL.kl_continuous *)

(* sprinkler *)
let _ = gen_csv ~fname:"sprinkler_mh.csv" grass_model grass_model_exact (MH 500) KL.kl_discrete
let _ = gen_csv ~fname:"sprinkler_rej.csv" grass_model grass_model_exact (Rejection(300,Soft)) KL.kl_discrete
let _ = gen_csv ~fname:"sprinkler_imp.csv" grass_model grass_model_exact (Importance 500) KL.kl_discrete
let _ = gen_csv ~fname:"sprinkler_smc.csv" grass_model grass_model_exact (SMC 500) KL.kl_discrete

(* let is = [|MH 500;Rejection(300,Soft);Importance 500;SMC 500|] *)
(* let _ = gen_csv' ~fname:"kl_sprinkler_all.csv" grass_model grass_model_exact is KL.kl_discrete *)

(* This stuff tests that the kl divergence function works *)
let test_kl_function ()  =
  (* Using kl discrete for continuous distributions will diverge with number of samples *)
  (* converge to 0 *)
  let diff1 = KL.kl_continuous ~n:10 Primitives.(normal 0. 1.) (normal 0. 1.) in
  let diff2 = KL.kl_continuous ~n:100 Primitives.(normal 0. 1.) (normal 0. 1.) in
  let diff3 = KL.kl_continuous ~n:1000 Primitives.(normal 0. 1.) (normal 0. 1.) in
  let diff4 = KL.kl_continuous ~n:10000 Primitives.(normal 0. 1.) (normal 0. 1.) in
  let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4 in

  (* diverges to ?? *)
  let diff1 = KL.kl_continuous ~n:10 Primitives.(continuous_uniform 0. 1.) (beta 10. 2.) in
  let diff2 = KL.kl_continuous ~n:100 Primitives.(continuous_uniform 0. 1.) (beta 10. 2.) in
  let diff3 = KL.kl_continuous ~n:1000 Primitives.(continuous_uniform 0. 1.) (beta 10. 2.) in
  let diff4 = KL.kl_continuous ~n:10000 Primitives.(continuous_uniform 0. 1.) (beta 10. 2.) in
  let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4 in

  (* converge to 0 *)
  let diff1 = KL.kl_discrete ~n:10 Primitives.(binomial 10 0.5) (binomial 10 0.5) in
  let diff2 = KL.kl_discrete ~n:100 Primitives.(binomial 10 0.5) (binomial 10 0.5) in
  let diff3 = KL.kl_discrete ~n:1000 Primitives.(binomial 10 0.5) (binomial 10 0.5) in
  let diff4 = KL.kl_discrete ~n:10000 Primitives.(binomial 10 0.5) (binomial 10 0.5) in
  let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4 in

  (* converge to 0.5  *)
  let diff1 = KL.kl_discrete ~n:10 Primitives.(discrete_uniform [0;1]) (binomial 1 0.9) in
  let diff2 = KL.kl_discrete ~n:100 Primitives.(discrete_uniform [0;1]) (binomial 1 0.9) in
  let diff3 = KL.kl_discrete ~n:1000 Primitives.(discrete_uniform [0;1]) (binomial 1 0.9) in
  let diff4 = KL.kl_discrete ~n:10000 Primitives.(discrete_uniform [0;1]) (binomial 1 0.9) in
  let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4 in
  ()

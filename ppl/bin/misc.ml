open Ppl
open Core
open Single_coin

(* open Sprinkler *)

open Ppl
open Core

let one_col_mat xs = Owl.(Mat.of_array xs (Array.length xs) 1)

let one_row_mat xs = Owl.(Mat.of_array xs 1 (Array.length xs))

(* let test =
   let open Owl_plplot in
   let h = Plot.create ~m:1 ~n:2 "a.png" in
   let d = infer Single_coin.single_coin (MH(100)) in
   (* let d = beta 10. 2. in *)
   let d' = Primitive.beta 10. 2. in

   let emp = CSamples.from_dist ~n:100000 d in
   Plot.subplot h 0 0;
   Plot.plot_fun ~h (CSamples.to_pdf emp) (0.) 1.;
   Plot.plot_fun ~h (Primitive.(pdf d')) (0.) 1.;

   Plot.subplot h 0 1;
   Plot.plot_fun ~h (CSamples.to_cdf emp) (0.) 1.;
   Plot.plot_fun ~h (Primitive.(cdf d')) (0.) 1.;

   Plot.output h

   let () = exit 0 *)

(* let x_vals = Array.init 3 ~f:(fun i->50*i+10) *)

let x_vals =
  Array.map ~f:int_of_float
  @@ Owl.Arr.to_array (Owl.Arr.logspace ~base:10. 2. 4. 50)

(* let xs =
   Evaluation.kl_cum_continuous x_vals exact_coin (infer single_coin (MH 1000)) *)

let xs =
  Evaluation.kl_cum_discrete x_vals Sprinkler.grass_model_exact
    (infer Sprinkler.grass_model'' (MH 1000))

let () = Array.iter ~f:(fun (x, y) -> printf "%d,%f\n" x y) xs

let ( @@@ ) = Fn.compose

let () =
  let open Owl_plplot in
  let h = Plot.create "b.png" in
  Plot.loglog ~h
    ~x:(one_col_mat (Array.map ~f:(fun x -> float_of_int (fst x)) xs))
    (one_col_mat (Array.map ~f:(abs_float @@@ snd) xs));
  Plot.output h

let () = printf "\n"

(* let xs =
   Evaluation.kl_cum_discrete x_vals grass_model_exact
    (infer grass_model'' (SMC(100)))

   let () = Array.iter ~f:(fun (x, y) -> printf "%d,%f\n" x y) xs *)

open Core
open Dist
open Helpers
module D = Empirical.Discrete
module C = Empirical.ContinuousArr
module P = Primitive

type 'a samples = 'a D.t

type 'a dist = 'a Dist.dist

let kl_discrete ?(n = 1000) p q =
  (* q is samples, p is exact dist*)
  let q = D.from_dist q ~n in
  let pdf_p = Primitive.pdf p in
  let pdf_q = D.to_pdf q in
  let support_q = D.support q in
  let f x =
    let p_x = pdf_p x in
    match pdf_q x with 0. -> raise Undefined | q_x -> p_x *. log (p_x /. q_x)
  in
  List.sum (module Float) ~f support_q

let kl_cum_discrete ns p q =
  (* ns is increasing array of sample numbers to storer *)
  let q_emp = ref D.empty in
  let pdf_p = Primitive.pdf p in
  let total_n = ns.(Array.length ns - 1) + 1 in
  let i = ref 0 in
  let arr = Array.create ~len:(Array.length ns) (0, 0.) in
  let j = ref 0 in
  while !i < total_n do
    q_emp := D.add_sample !q_emp (sample q);
    if Array.mem ns !i ~equal:Int.equal then (
      let kl =
        let support_q = D.support !q_emp in
        let pdf_q = D.to_pdf !q_emp in
        let f x =
          let p_x = pdf_p x in
          match pdf_q x with
          | 0. -> raise Undefined
          | q_x ->
              (* printf "for %b p=%f,q=%f\n" x p_x q_x; *)
              p_x *. log (p_x /. q_x)
        in
        List.sum (module Float) ~f support_q
      in
      (* TODO: is this right - should i be dividing by i? *)
      arr.(!j) <- (!i, kl);
      j := !j + 1 )
    else ();
    i := !i + 1
  done;
  arr

let samples_to_pdf samples =
  let open Owl_stats in
  let n = Array.length samples in
  (* sturges rule *)
  let num_bins = int_of_float (1. +. (3.3 *. log (float_of_int n))) in
  let h = histogram (`N num_bins) samples |> normalise_density in
  let pdf_q = Option.value h.density ~default:[||] in
  let mps =
    Array.mapi pdf_q ~f:(fun i _ -> (h.bins.(i + 1) +. h.bins.(i)) /. 2.)
  in
  (mps, pdf_q)

let kl_helper ps qs =
  let open Float in
  Array.map2_exn ps qs ~f:(fun p_x q_x ->
      if q_x = 0. then 0. else p_x * log (p_x / q_x))
  |> Array.sum (module Float) ~f:ident
  |> abs

let kl_cont samples exact =
  let n = float_of_int @@ Array.length samples in
  let mps, q = samples_to_pdf samples in
  let p = Array.map mps ~f:(Primitive.pdf exact) in
  kl_helper p q /. n

let kl_cum_continuous ns p q =
  let q_emp = ref C.empty in
  let total_n = ns.(Array.length ns - 1) + 1 in
  let i = ref 0 in
  let arr = Array.create ~len:(Array.length ns) (0, 0.) in
  let j = ref 0 in
  while !i < total_n do
    q_emp := C.add_sample !q_emp (sample q);
    if Array.mem ns !i ~equal:Int.equal then (
      (* let kl =
         let support_q = Array.to_list @@ C.values !q_emp in
         let support_q = List.dedup_and_sort ~compare:Float.compare support_q in
         let pdf_q = C.to_pdf !q_emp in
         let k = ref 1. in
         let f x =
          let p_x = pdf_p x in
          let ret =
            match pdf_q x with
            | 0. -> 0.
            | q_x ->
              p_x *. log (p_x /. q_x)
          in
          if Float.is_finite ret then (k:=!k+.1.; ret) else 0.
         in
         (* TODO: is this right - should I be dividing by i? *)
         List.sum
          (module Float)
          ~f support_q /. !k
         in *)
      let kl = kl_cont (C.values !q_emp) p in
      (* TODO: is this right - should I be dividing by i? *)
      arr.(!j) <- (!i, kl);
      (* arr.(!j) <- (!i, kl /. float_of_int !i); *)
      incr j )
    else ();
    incr i
  done;
  arr

let kl_continuous ?(n = 10000) p q =
  (* convert to owl histogram, 
     then treat as a discrete with midpoint of each bin as the value *)

  (* type histogram =
     { bins : float array
     ; counts : int array
     ; weighted_counts : float array option
     ; normalised_counts : float array option
     ; density : float array option
     }
  *)
  let samples = Array.init n ~f:(fun _ -> sample q) in
  let open Owl_stats in
  let h =
    histogram (`N ((n / 10) + 10)) samples |> normalise |> normalise_density
  in
  let pdf_q = Option.value h.density ~default:[||] in
  let sum = ref 0. in
  for i = 0 to Array.length pdf_q - 1 do
    let mp = (h.bins.(i + 1) +. h.bins.(i)) /. 2. in
    let f_p = Primitive.pdf p mp in
    let f_q = pdf_q.(i) in
    if Float.(f_q = 0.) then () else sum := !sum +. (f_p *. log (f_p /. f_q))
  done;
  !sum /. float_of_int n

(* d is inferred dist, d' is exact dist, default 1% signifigance level *)
(* Only works for float dist, make it work for dists of any comparable type (take in a first class comparator module like maps) *)
let kolmogorov_smirnov ?(n = 10000) ?(alpha = 0.01) d d' =
  (* let ecdf_map d: (float, float) Core.Map = Core.Map.of_sorted_array (module Float) (Owl_stats.ecdf (take_k_samples n d)) in *)
  (* let ecdf x = ecdf_map d in *)
  let h = Owl_stats.ks_test ~alpha (take_k_samples n d) (Primitive.cdf d') in
  if h.reject then Printf.printf "two dists are not equal with p=%f\n" h.p_value
  else Printf.printf "two dists are equal with p=%f\n" h.p_value;
  h

(* d is inferred dist, d' is exact dist, default 1% signifigance level *)

(** Only works for float dist, make it work for dists of any comparable type (take in a first class comparator module like maps) *)
let chi_sq ?(n = 10000) ?(alpha = 0.01) d d' : Owl_stats.hypothesis =
  let open P in
  let supp =
    match Primitive.support d' with
    | DiscreteFinite xs -> xs
    | _ -> raise Undefined
    (* Can't do this test on continuous dists*)
  in
  let samples = D.from_dist ~n d in
  let num_observed x = Float.of_int @@ D.get_num samples x in
  let num_expected x = Float.of_int n *. Primitive.pdf d' x in
  let test_stat =
    List.sum
      (module Float)
      supp
      ~f:(fun x -> ((num_observed x -. num_expected x) ** 2.) /. num_expected x)
  in
  let df = Float.of_int @@ (List.length supp - 1) in
  printf "ts: %f" test_stat;
  let p = Owl_stats.chi2_cdf ~df test_stat in
  if Float.(p < alpha) then
    Printf.printf "two dists are not equal with p=%f\n" p
  else Printf.printf "two dists are equal with p=%f\n" p;
  { reject = Float.(p > test_stat); p_value = p; score = p }

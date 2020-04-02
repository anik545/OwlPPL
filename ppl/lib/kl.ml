open Core
open Dist.GADT_Dist
open Sigs
open Helpers

exception Undefined
module type KL_sig = sig
  type 'a samples
  type 'a primitive

  val kl_discrete: ?n:int -> 'a primitive -> 'a dist -> float
  val kl_continuous: ?n:int -> float primitive -> float dist -> float
  val kolmogorov_smirnov: ?n:int -> ?alpha:float -> float dist -> float primitive -> Owl_stats.hypothesis
  val chi_sq: ?n:int -> ?alpha:float -> 'a dist -> 'a primitive -> Owl_stats.hypothesis
end

module KL_Div(P: Primitives)(D: Samples.Samples): 
  KL_sig with type 'a samples = 'a D.t 
          and type 'a primitive = 'a P.primitive = 
struct

  type 'a samples = 'a D.t
  type 'a primitive = 'a P.primitive


  let kl_discrete ?(n=1000) p q = 
    (* q is samples, p is exact dist*)
    let q = D.from_dist q ~n in
    let pdf_p = P.pdf p in
    let pdf_q = D.to_pdf q in
    let support_q = D.support q in

    let f x = 
      let p_x = pdf_p x in
      match pdf_q x with
      | 0. -> raise Undefined
      | q_x -> p_x *. log (p_x /. q_x)
    in
    List.sum (module Float) ~f support_q

  let kl_cum_discrete ns p q = 
    (* ns is increasing array of sample numbers to storer *)
    let q_emp = ref D.empty in
    let pdf_p = P.pdf p in

    let total_n = Array.get ns (Array.length ns - 1) in
    let i = ref 0  in
    let arr = Array.create ~len:(Array.length ns) (0,0.) in
    let j = ref 0 in
    while !i < total_n do
      q_emp := D.add_sample !q_emp (sample q);
      if Array.mem ns !i ~equal:(Int.equal) then 
        let kl = 
          let support_q = D.support !q_emp in
          let pdf_q = D.to_pdf !q_emp in
          let f x = 
            let p_x = pdf_p x in
            match pdf_q x with
            | 0. -> raise Undefined
            | q_x -> p_x *. log (p_x /. q_x)
          in
          List.sum (module Float) ~f support_q
        in
        arr.((!j))<-(!i,kl);
        j:=!j+1;
      else ()
      ;
      i:= !i + 1
    done;
    arr

  let kl_continuous ?(n=10000) p q = 
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
    let h = histogram (`N (n/10+10)) (samples) 
            |> normalise
            |> normalise_density
    in
    let pdf_q = match h.density with Some pdf_q -> pdf_q | None -> [||] in
    let sum = ref 0. in 
    for i = 0 to Array.length pdf_q - 1 do
      let mp = (h.bins.(i + 1) +. h.bins.(i)) /. 2. in
      let f_p = P.pdf p mp in
      let f_q = pdf_q.(i) in
      if  Float.(f_q = 0.) then () else sum := !sum +. (f_p *. log (f_p /. f_q))
    done;
    !sum /. float_of_int n

  (* d is inferred dist, d' is exact dist, default 1% signifigance level *)
  (* Only works for float dist, make it work for dists of any comparable type (take in a first class comparator module like maps) *)
  let kolmogorov_smirnov ?(n=10000) ?(alpha=0.01) d d' =
    (* let ecdf_map d: (float, float) Core.Map = Core.Map.of_sorted_array (module Float) (Owl_stats.ecdf (take_k_samples n d)) in *)
    (* let ecdf x = ecdf_map d in *)
    let h = Owl_stats.ks_test ~alpha (take_k_samples n d) (P.cdf d') in
    if h.reject then
      Printf.printf "two dists are not equal with p=%f\n" h.p_value
    else
      Printf.printf "two dists are equal with p=%f\n" h.p_value
    ;
    h

  (* d is inferred dist, d' is exact dist, default 1% signifigance level *)
  let chi_sq ?(n=10000) ?(alpha=0.01) d d' : Owl_stats.hypothesis= 
    let supp = match P.support d' with
        Discrete xs -> xs
      | Continuous -> raise Undefined (* Can't do this test on continuous dists*)
    in
    let samples = D.from_dist ~n d in
    let num_observed x = Float.of_int @@ D.get_num samples x in
    let num_expected x = Float.of_int n *. (P.pdf d' x) in
    let test_stat = List.sum (module Float) supp ~f:(fun x -> (((num_observed x) -. (num_expected x))**2. /. (num_expected x))) in
    let df = Float.of_int @@ List.length supp - 1 in
    let p = Owl_stats.chi2_cdf ~df test_stat in
    if Float.(p < alpha) then
      Printf.printf "two dists are not equal with p=%f\n" p
    else
      Printf.printf "two dists are equal with p=%f\n" p
    ;
    {reject = Float.(p > test_stat);p_value=p;score=p}
end

module KL = KL_Div(Primitive_dists.Primitive_Dists)(Samples.DiscreteSamples)
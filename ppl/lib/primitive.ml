open Core

exception NotImplemented

type 'a support =
  | DiscreteFinite of 'a list
  | DiscreteInfinite
  | ContinuousFinite of ('a * 'a) list (* set of endpoints *)
  | ContinuousInfinite
  | Merged of 'a support * 'a support

(* type 'a support = Discrete of 'a list | Continuous *)

let merge_supports = function
  | DiscreteFinite xs, DiscreteFinite xs' -> DiscreteFinite (xs @ xs')
  | ContinuousFinite xs, ContinuousFinite xs' -> ContinuousFinite (xs @ xs')
  | x, x' when Poly.(x = x') -> x
  | x, x' -> Merged (x, x')

module type PRIM_DIST = sig
  type t

  val sample : unit -> t

  val pdf : t -> float

  val cdf : t -> float

  (* TODO: add ppf function *)
  val ppf : t -> float

  val support : t support
end

type 'a t = (module PRIM_DIST with type t = 'a)

let binomial n p =
  ( module struct
    type t = int

    let sample () = Owl_stats_dist.binomial_rvs ~n ~p

    let pdf = Owl_stats_dist.binomial_pdf ~n ~p

    let cdf = Owl_stats_dist.binomial_cdf ~n ~p

    let ppf _ = raise NotImplemented

    let support = DiscreteFinite (List.init n ~f:ident)
  end : PRIM_DIST
    with type t = int )

let categorical (type a) xs =
  let xs_arr = Array.of_list_map ~f:fst xs in
  let ps = Array.of_list_map ~f:snd xs in
  ( module struct
    type t = a

    let sample () =
      (* List.iter xs ~f:(fun (_, b) -> printf "%f\n%!" b); *)
      xs_arr.(Owl_stats.categorical_rvs ps)

    let pdf x =
      let rec lookup l = function
        | (a, p) :: xs -> if Stdlib.(a = x) then p else lookup l xs
        | [] -> 0.
        (* not found *)
      in
      lookup x xs

    let cdf _ = raise NotImplemented

    let ppf _ = raise NotImplemented

    let support = DiscreteFinite (List.map ~f:fst xs)
  end : PRIM_DIST
    with type t = a )

let normal mu sigma =
  ( module struct
    type t = float

    let sample () = Owl_base_stats.gaussian_rvs ~mu ~sigma

    let pdf = Owl_stats_dist.gaussian_pdf ~mu ~sigma

    let cdf = Owl_stats_dist.gaussian_cdf ~mu ~sigma

    let ppf = Owl_stats_dist.gaussian_ppf ~mu ~sigma

    let support = ContinuousInfinite
  end : PRIM_DIST
    with type t = float )

let discrete_uniform (type a) xs =
  ( module struct
    type t = a

    let sample () = (Owl_base_stats.sample (Array.of_list xs) 1).(0)

    let pdf _ = 1. /. float_of_int (List.length xs)

    let cdf _ = raise NotImplemented

    let ppf _ = raise NotImplemented

    let support = DiscreteFinite xs
  end : PRIM_DIST
    with type t = a )

let beta a b =
  ( module struct
    type t = float

    let sample () = Owl_stats_dist.beta_rvs ~a ~b

    let pdf = Owl_stats_dist.beta_pdf ~a ~b

    let cdf = Owl_stats_dist.beta_cdf ~a ~b

    let ppf = Owl_stats_dist.beta_ppf ~a ~b

    let support = ContinuousFinite [ (0., 1.) ]
  end : PRIM_DIST
    with type t = float )

let gamma shape scale =
  ( module struct
    type t = float

    let sample () = Owl_stats_dist.gamma_rvs ~shape ~scale

    let pdf = Owl_stats_dist.gamma_pdf ~shape ~scale

    let cdf = Owl_stats_dist.gamma_cdf ~shape ~scale

    let ppf _ = raise NotImplemented

    let support = ContinuousInfinite
  end : PRIM_DIST
    with type t = float )

let continuous_uniform a b =
  ( module struct
    type t = float

    let sample () = Owl_stats_dist.uniform_rvs ~a ~b

    let pdf = Owl_stats_dist.uniform_pdf ~a ~b

    let cdf = Owl_stats_dist.uniform_cdf ~a ~b

    let ppf = Owl_stats_dist.uniform_ppf ~a ~b

    let support = ContinuousFinite [ (a, b) ]
  end : PRIM_DIST
    with type t = float )

let pdf (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.pdf

let logpdf (type a) d x =
  let (module D : PRIM_DIST with type t = a) = d in
  log (D.pdf x)

let cdf (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.cdf

(* let ppf (type a) (d:a primitive) = let (module D) = d in D.ppf *)
let sample (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.sample ()

let support (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.support

let ppf (type a) d =
  let (module D : PRIM_DIST with type t = a) = d in
  D.ppf

let create_primitive (type a) ~sample ~pdf ~cdf ~support ~ppf =
  let module D = struct
    type t = a

    let sample = sample

    let pdf = pdf

    let cdf = cdf

    let ppf = ppf

    let support = support
  end in
  (module D : PRIM_DIST with type t = a)

open Owl.Maths

let poisson l =
  ( module struct
    type t = int

    let sample () =
      let l' = ref (exp (-.l)) in
      let k = ref 0 in
      let p = ref 1. in
      while Float.(!p > !l') do
        k := !k + 1;
        let u = Owl_stats_dist.uniform_rvs ~a:0. ~b:1. in
        p := !p *. u
      done;
      !k - 1

    let pdf k =
      let open Float in
      (l ** float_of_int k) * exp (-l) / fact k

    let cdf k = gammainc (float_of_int (k + 1)) l /. fact k

    let ppf _ = raise NotImplemented

    let support = DiscreteInfinite
  end : PRIM_DIST
    with type t = int )

(* open Owl.Maths
   let pdf k l = 
   let open Float in
   (l ** (float_of_int k)*exp(-l))/ fact k  *)

open Monad

module Dist(P: Primitive_dists.Primitives) = struct
  exception Undefined
  type prob = float
  type likelihood = float
  type 'a samples = ('a * prob) list
  type 'a no_dup_vals_samples = ('a , prob) Core.Map.Poly.t

  type _ dist = 
      Return: 'a -> 'a dist
    | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
    | Primitive: 'a P.primitive -> 'a dist
    | Conditional: ('a -> float) * 'a dist -> 'a dist

  let condition (c: 'a -> likelihood) d = Conditional (c,d)
  let primitive p = Primitive p

  module DistMonad: (Monad with type 'a t = 'a dist) = struct
    type 'a t = 'a dist
    let return x = Return x
    let bind d f = Bind (d,f)
  end
  include Make_Extended(DistMonad)

  let uniform xs = Primitive (P.discrete_uniform xs)
  let categorical xs = Primitive (P.categorical xs)
  let bernoulli p = categorical [(true, p); (false, 1. -. p)]
  let choice p x y = Bind ((bernoulli p), (fun c -> if c then x else y))
  let binomial_ n p = Primitive (P.binomial n p) 
  let binomial n p = P.binomial n p (* TODO: ?? why was it like this ??*)

  (* continuous *)
  let normal mu sigma = Primitive (P.normal mu sigma)
  let gamma shape scale = Primitive (P.gamma shape scale)
  let beta a b = Primitive (P.beta a b)
  let c_uniform a b = Primitive (P.continuous_uniform a b)

  let rec sample: 'a. 'a dist -> 'a = function
      Return x -> x
    | Bind (d,f) -> let y = f (sample d) in sample y
    | Primitive d -> P.sample d
    | Conditional (_,_) -> raise Undefined

  let rec sample_with_score: 'a. 'a dist -> ('a * likelihood) = function
      Return x -> (x, 1.)
    | Bind (d,f) -> 
      let y, score =  (sample_with_score d) in
      let d = f y in
      let a,b = (sample_with_score d) in
      (a,b+.score)

    | Primitive d -> let x  = P.sample d in (x, P.pdf d x) (* this instead?? *)
    (* this is how its done in prior - is it right??? *)
    (* | Primitive d -> let x  = P.sample d in (x, 1.)  *)
    | Conditional (lik,d) -> 
      let x,s = sample_with_score d in
      (x,s +. lik x)

  let sample_mean ?(n=100000) d = 
    let rec loop n d sofar = 
      if n = 0 
      then sofar
      else loop (n-1) d ((sample d) +. sofar)
    in
    (loop n d 0.) /. float_of_int n
  let take_k_samples k d = Core.Array.init k ~f:(fun _ -> sample d)

  let hist_dist ?h ?(n=5000) ?(fname="fig.jpg") d = 
    let open Owl_plplot in 
    let samples = take_k_samples n d in

    let pl = match h with 
      | None -> Plot.create ~m:1 ~n:1 fname 
      | Some h -> h 
    in
    Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (Mat.of_array samples n 1) 0);
    pl

  let undup xs = 
    let map = Core.Map.Poly.of_alist_fold xs ~f:(+.) ~init:0. in
    map

  let dist_of_n_samples n (d: 'a dist): 'a list dist = 
    sequence @@ Core.List.init n ~f:(fun _ -> (d))

  (* TODO: create a prob_map module *)
  let weighted_dist ?(n=300) (d: 'a dist) : ('a, int) Core.Map.Poly.t =
    let rec loop n map =
      if n = 0 then map else
        let s = sample d in 
        let map = Core.Map.Poly.update map s ~f:(fun x -> match x with None -> 1 | Some y -> y + 1) in
        loop (n-1) map
    in
    loop n Core.Map.Poly.empty
end


open Dist(Primitive_dists.Primitive_Dists)

module GADT_Dist = Dist(Primitive_dists.Primitive_Dists)
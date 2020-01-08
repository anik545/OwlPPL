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

  let condition' (c: 'a -> likelihood) d = Conditional (c,d)
  let primitive p = Primitive p

  let condition b d = Conditional((fun _ -> if b then 1. else 0.), d)
  let score s d = Conditional((fun _ -> s),d)
  let observe x dst d = Conditional((fun _ -> P.pdf dst x),d)

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
      let y, s =  (sample_with_score d) in
      let a,s1 = (sample_with_score (f y)) in
      (a,s*.s1)

    | Primitive d -> let x  = P.sample d in (x, P.pdf d x) (* this instead?? *)
    (* this is how its done in prior - is it right??? *)
    (* | Primitive d -> let x  = P.sample d in (x, 1.)  *)
    | Conditional (lik,d) -> 
      let x,s = sample_with_score d in
      (x,s *. lik x)


  let rec prior': 'a.'a dist -> 'a dist = function
      Conditional (_,d) -> prior' d
    | Bind (d,f) -> Bind((prior' d), f)
    | d -> d

  let rec prior: 'a.'a dist -> ('a*prob) dist = function
      Conditional (c,d) ->
      let* (x,s) = prior d in
      return (x, s *. (c x))
    | Bind (d,f) ->
      let* (x,s) = prior d in
      let* y = f x in
      return (y, s)
    | d ->
      let* x = d in
      return (x,1.)

  let rec prior1: 'a.'a dist -> ('a*prob) dist = function
      Conditional (c,d) ->
      let* (x,s) = prior1 d in
      return (x, s *. (c x))
    | Bind (d,f) ->
      let* (x,s) = prior1 d in
      let* y,s1 = prior1 (f x) in
      return (y, s*.s1)
    | Primitive d -> let* x = Primitive d in return (x, P.pdf d x)
    | d ->
      let* x = d in
      return (x,1.)

  let dist_of_n_samples n (d: 'a dist): 'a list dist = 
    sequence @@ Core.List.init n ~f:(fun _ -> (d))

end


open Dist(Primitive_dists.Primitive_Dists)

module GADT_Dist = Dist(Primitive_dists.Primitive_Dists)
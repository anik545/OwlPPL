open Monad
open Sigs

module type Ops = sig 
  type 'a dist
  val ( +~ ) : int dist -> int dist -> int dist
  val ( -~ ) : int dist -> int dist -> int dist
  val ( *~ ) : int dist -> int dist -> int dist
  val ( /~ ) : int dist -> int dist -> int dist
  val ( +.~ ) : float dist -> float dist -> float dist
  val ( -.~ ) : float dist -> float dist -> float dist
  val ( *.~ ) : float dist -> float dist -> float dist
  val ( /.~ ) : float dist -> float dist -> float dist
  val ( &~ ) : bool dist -> bool dist -> bool dist
  val ( |~ ) : bool dist -> bool dist -> bool dist
  val not : bool dist -> bool dist
  val ( ^~ ) : string dist -> string dist -> string dist
end


module type Dist = sig 
  module P: Primitives
  type prob = float
  type likelihood = float
  type 'a samples = ('a * prob) list
  type  _ dist = 
      Return: 'a -> 'a dist
    (* bind is lazy since it contains a function *)
    | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
    (* | Bind_var: 'a var_dist * ('a -> 'b dist) -> 'b dist *)
    | Primitive: 'a P.primitive -> 'a dist
    | Conditional: ('a -> float) * 'a dist -> 'a dist
    (* | Conditional: ('a -> float) * 'a var_dist -> 'a dist *)

  val condition' : ('a -> likelihood) -> 'a dist -> 'a dist
  val condition : bool -> 'a dist -> 'a dist
  val score : likelihood -> 'a dist -> 'a dist
  val observe : 'a -> 'a P.primitive -> 'b dist -> 'b dist
  module DistMonad: Monad
  include Monad.EMonad with type 'a t := 'a dist


  include Primitive_Distributions with type 'a primitive := 'a dist

  val bernoulli: likelihood -> bool dist

  val choice : likelihood -> 'a dist -> 'a dist -> 'a dist
  val sample : 'a dist -> 'a
  val sample_n : int -> 'a dist -> 'a array
  val sample_with_score : 'a dist -> 'a * likelihood
  val prior' : 'a dist -> 'a dist
  val prior : 'a dist -> ('a * likelihood) dist
  val prior_with_score : 'a dist -> ('a * likelihood) dist
  val dist_of_n_samples : int -> 'a dist -> 'a list dist
  module PplOps: Ops with type 'a dist := 'a dist
end


module Dist(P: Primitives) = struct
  module P = P
  exception Undefined
  type prob = float
  type likelihood = float
  type 'a samples = ('a * prob) list

  (* enable watching intermediate variables through plots *)
  (* type 'a var_dist = string * 'a dist *)
  type  _ dist = 
      Return: 'a -> 'a dist
    (* bind is lazy since it contains a function *)
    | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
    (* | Bind_var: 'a var_dist * ('a -> 'b dist) -> 'b dist *)
    | Primitive: 'a P.primitive -> 'a dist
    | Conditional: ('a -> float) * 'a dist -> 'a dist
    (* | Conditional: ('a -> float) * 'a var_dist -> 'a dist *)

  (* TODO: uncomment + fix *)
  (* | Independent: 'a dist * 'b dist -> ('a * 'b) dist
     let (and+) d1 d2 = Independent(d1,d2) *)

  let condition' (c: 'a -> likelihood) d = Conditional (c,d)
  let condition b d = Conditional((fun _ -> if b then 1. else 0.), d)
  let score s d = Conditional((fun _ -> s),d)
  let observe x dst d = Conditional((fun _ -> P.pdf dst x),d)
  (* TODO: *)
  (* let observe_list lst dst d = Core.List.fold_left ~f:(observe) *)

  module DistMonad: (Monad with type 'a t = 'a dist) = struct
    type 'a t = 'a dist
    let return x = Return x
    let bind d f = Bind (d,f)
  end
  include Make_Extended(DistMonad)

  let discrete_uniform xs = Primitive (P.discrete_uniform xs)
  let categorical xs = Primitive (P.categorical xs)
  let bernoulli p = categorical [(true, p); (false, 1. -. p)]
  let choice p x y = Bind ((bernoulli p), (fun c -> if c then x else y))
  let binomial n p = Primitive (P.binomial n p)

  (* continuous *)
  let normal mu sigma = Primitive (P.normal mu sigma)
  let gamma shape scale = Primitive (P.gamma shape scale)
  let beta a b = Primitive (P.beta a b)
  let continuous_uniform a b = Primitive (P.continuous_uniform a b)

  let rec sample: 'a. 'a dist -> 'a = function
      Return x -> x
    | Bind (d,f) -> let y = f (sample d) in sample y
    | Primitive d -> P.sample d
    | Conditional (_,_) -> raise Undefined
  (* | Independent (d1,d2) -> sample d1, sample d2 *)

  let rec sample_n: 'a. int -> 'a dist -> 'a array = fun n -> function
      Return x -> Array.init n (fun _ -> x)
    | Bind (d,f) -> let y = Array.map f (sample_n n d) in Array.map sample y
    | Primitive d -> Array.init n (fun _ -> P.sample d)
    | Conditional (_,_) -> raise Undefined

  (* same as sample (prior_with_score d) *)
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

  (* same as sample_with_score *)
  let rec prior_with_score: 'a.'a dist -> ('a*prob) dist = function
      Conditional (c,d) ->
      let* (x,s) = prior_with_score d in
      return (x, s *. (c x))
    | Bind (d,f) ->
      let* (x,s) = prior_with_score d in
      let* y,s1 = prior_with_score (f x) in
      return (y, s*.s1)
    | Primitive d -> let* x = Primitive d in return (x, P.pdf d x)
    | Return x ->
      return (x,1.)
  (* | Independent(d1,d2) -> 
     let* x,s1 = prior_with_score d1 
     and* y,s2 = prior_with_score d2 in
     ((x,y),s1*.s2) *)

  let dist_of_n_samples n (d: 'a dist): 'a list dist = 
    sequence @@ Core.List.init n ~f:(fun _ -> d)

  module PplOps = struct
    let ( +~ ) = liftM2 ( + )
    let ( -~ ) = liftM2 ( - )
    let ( *~ ) = liftM2 ( * )
    let ( /~ ) = liftM2 ( / )
    let ( +.~ ) = liftM2 ( +. )
    let ( -.~ ) = liftM2 ( -. )
    let ( *.~ ) = liftM2 ( *. )
    let ( /.~ ) = liftM2 ( /. )
    let ( &~ ) = liftM2 ( && )
    let ( |~ ) = liftM2 ( || )
    let not = liftM not

    let ( ^~ ) = liftM2 ( ^ )
  end
end


open Dist(Primitive_dists.Primitive_Dists)

module GADT_Dist = Dist(Primitive_dists.Primitive_Dists)

module Ppl = GADT_Dist
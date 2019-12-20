module type Monad = sig
  type 'a t
  val return: 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (let*): 'a t -> ('a -> 'b t) -> 'b t

  val liftM: ('a -> 'b) -> 'a t -> 'b t
  val liftM2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val mapM: ('a -> 'b t) -> 'a list -> 'b list t
  val sequence: 'a t list -> 'a list t
end

module type PrimitiveDists = sig
  type 'a d
  val normal: float -> float -> float d
  val uniform: 'a list -> 'a d
  val categorical: ('a*float) list -> 'a d
  val beta: float -> float -> float d
  val binomial: int -> float -> int d
  val c_uniform: float -> float -> float d
  val gamma: float -> float -> float d
  val bernoulli: float -> bool d
end

module type Sampleable = sig
  type 'a sampleable
  val sample: 'a sampleable -> 'a
end

module type PrimitiveSampleable = sig
  include PrimitiveDists
  include Sampleable with type 'a sampleable := 'a d
  val pdf: 'a d -> 'a -> float
end

module type SampleMonad = sig
  include Sampleable
  include Monad with type 'a t := 'a sampleable
end

module type DistOps = sig
  type 'a t
  val ( +~ ): int t -> int t -> int t
  val ( -~ ): int t -> int t -> int t
  val ( *~ ): int t -> int t -> int t

  val ( +~. ): float t -> float t -> float t
  val ( -~. ): float t -> float t -> float t
  val ( *~. ): float t -> float t -> float t
end

module type SampleDistMonad = sig
  type 'a dist
  module S: PrimitiveSampleable
  include Sampleable with type 'a sampleable := 'a dist
  include Monad with type 'a t := 'a dist
  include PrimitiveDists with type 'a d := 'a dist
  include DistOps with type 'a t := 'a dist
  val condition: ('a -> float) -> 'a dist -> 'a dist
  val observe: 'a dist -> ('a -> 'b S.d) -> 'b -> 'a dist
  val observe_multiple: 'a dist -> ('a -> 'b S.d) -> 'b list -> 'a dist
  val prior: 'a dist -> ('a * float) dist 
end


module Dist (S: PrimitiveSampleable): SampleDistMonad = struct
  open Core
  module S=S

  type prob = float
  exception Undefined

  type _ dist =
    Return: 'a -> 'a dist
  | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
  | Primitive: 'a S.d -> 'a dist
  | Conditional: ('a -> prob) * 'a dist -> 'a dist
  
  let (>>=) d f = Bind(d,f)
  let return x = Return x

  let condition likelihood prior = Conditional(likelihood, prior)

  let observe prior lik obs = condition (fun x -> S.pdf (lik x) obs) prior
  let observe_multiple prior lik obss = List.fold obss ~init:prior ~f:(fun d obs-> observe d lik obs)

  let rec sample: 'a. 'a dist -> 'a = function
    Return x -> x
  | Bind (d,f) -> let y = f (sample d) in sample y
  | Primitive d -> S.sample d
  | Conditional (_,_) -> raise Undefined

  let (let*) = (>>=)

  let fmap f xs =
    let* x = xs in
    return (f x)

  let liftM = fmap

  let liftM2 f ma mb =
    let* a = ma in
    let* b = mb in
    return (f a b)

  let sequence mlist =
    let mcons p q =
      let* x = p in
      let* y = q in
      return (x::y)
    in List.fold_right mlist ~f:mcons ~init:(return [])

  let rec mapM f xs =
    match xs with
      | [] -> return []
      | x::xs ->
          let* x' = f x in
          let* xs' = mapM f xs in
          return (x'::xs')

  (* discrete *)
  let uniform xs = Primitive (S.uniform xs)
  let categorical xs = Primitive (S.categorical xs)
  let bernoulli p = categorical [(true, p); (false, 1. -. p)]
  (* let choice p x y = Bind ((bernoulli p), (fun c -> if c then x else y)) *) (*TODO*)
  let binomial n p = Primitive (S.binomial  n p)

  (* continuous *)
  let normal mu sigma = Primitive (S.normal mu sigma)
  let beta a b = Primitive (S.beta a b)
  let c_uniform a b = Primitive (S.c_uniform a b)
  let gamma a b = Primitive (S.gamma a b)

  (* TODO: split into extender module? or submodule? idk *)
  (* operators on probability distributions *)
  let ( +~ ) = liftM2 ( + )
  let ( -~ ) = liftM2 ( - )
  let ( *~ ) = liftM2 ( * )

  let ( +~. ) = liftM2 ( +. )
  let ( -~. ) = liftM2 ( -. )
  let ( *~. ) = liftM2 ( *. )

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

end

module type Infer = sig 
  module D:SampleDistMonad
  (* Inference is a procedure which transforms dists*)
  val infer: ?iterations:int -> 'a D.dist -> 'a D.dist
end


module SampleablePrimitives: PrimitiveSampleable = struct
  open Core
  type prob = float
  exception Undefined

  type _ sampleable =
    Normal: float * float -> float sampleable
  | Uniform: 'a list -> 'a sampleable
  | Categorical: ('a * prob) list -> 'a sampleable
  | Beta: float * float -> float sampleable
  | Binomial: int * float -> int sampleable
  | C_Uniform: float * float -> float sampleable
  | Gamma: float*float -> float sampleable
 
  let sample: type a.a sampleable -> a = function
    | Normal(mu,sigma) -> Owl_base_stats.gaussian_rvs ~mu:mu ~sigma:sigma
    | Uniform xs -> (Owl_base_stats.sample (Array.of_list xs) 1).(0)
    | Categorical xs ->
      let xs = List.sort xs ~compare:(fun a b ->  if Float.(snd a -. snd b < 0.) then 1 else -1) in
      let r = Owl_base_stats.uniform_rvs ~a:0.0 ~b:1. in
      let rec loop p_left remaining = match p_left with
          (v,p)::xs -> if Float.(remaining < p) || Stdlib.(xs = []) then v else loop xs (remaining -. p)
        | [] -> raise Undefined
      in
      loop xs r
    | Beta (a, b) -> Owl_stats_dist.beta_rvs ~a ~b
    | Binomial (n, p) -> Owl_stats_dist.binomial_rvs ~n ~p
    | C_Uniform (a,b) -> Owl_stats_dist.uniform_rvs ~a ~b
    | Gamma (shape,scale) -> Owl_stats_dist.gamma_rvs ~shape ~scale
    
  let pdf: type a.a sampleable -> a -> float = function
    (* cont *)
    | Normal(mu,sigma) -> Owl_stats_dist.gaussian_pdf ~mu ~sigma
    | Beta (a, b) -> Owl_stats_dist.beta_pdf ~a ~b
    | C_Uniform (a, b) -> Owl_stats_dist.uniform_pdf ~a ~b
    | Gamma (shape, scale) -> Owl_stats_dist.gamma_pdf ~shape ~scale

    (* discrete *)
    | Uniform us -> fun _ -> 1. /. (float_of_int (List.length us))
    | Categorical cs -> fun x -> 
      let rec lookup l = function 
        | (a,p)::xs -> if Stdlib.(a = x) then p else lookup l xs
        | [] -> 0. (* not found *)
      in
      lookup x cs
    | Binomial (n,p) -> Owl_stats_dist.binomial_pdf ~n ~p

  type 'a d = 'a sampleable

  let uniform xs = Uniform xs
  let c_uniform a b = C_Uniform (a,b)
  let normal mu sigma = Normal(mu,sigma)
  let categorical xs = Categorical xs
  let binomial  n p = Binomial(n,p)
  let beta a b = Beta(a,b)
  let gamma k theta = Gamma(k,theta)
  let bernoulli p = Categorical [(true, p); (false, 1.-.p)]

end


module Dist' : SampleDistMonad = Dist(SampleablePrimitives)

let x = 
  let open Dist' in 

  let rec die = function 
  | 0 -> return 0
  | 1 -> uniform [1;2;3;4;5;6]
  | n -> (die 1) +~ (die (n-1))

  in sample (die 4)

let y = 
  let open Dist' in

  let m = 
    let* mu = normal 0. 100. in
    let* tau = gamma 1. 0.1 in
    return (mu, tau)
  in
  let obs = [8.;9.;7.;7.;8.;10.] in
  let post = observe_multiple m (fun (mu,tau) -> S.normal mu (1. /. tau)) obs in
  post

module type Model = sig
  module D:SampleDistMonad
  type t
  val model: t D.dist
end

module MH (D:SampleDistMonad) : Infer with module D = D and type 'a D.dist = 'a D.dist = struct
  module D = D
  open D open Core

  let mh n d =
    let proposal = prior d in
    let rec iterate ?(n=n) (x,s) =
      if n = 0 then return [] else
        let* (y,r) = proposal in
        let* accept = bernoulli @@ Float.min 1. (r /. s) in
        let next = if accept then (y,r) else (x,s) in
        let* rest = iterate ~n:(n-1) next in
        return (next::rest)
    in
    liftM (List.map ~f:fst) (proposal >>= iterate)
  
  let infer ?(iterations=1000) d = liftM (fun x -> List.nth_exn x (iterations-1)) (mh iterations d)
end

module NormalModel(D:SampleDistMonad): Model = struct
  module D = D
  open D

  type t = (float*float)

  let model = 
    let m = 
      let* mu = normal 0. 100. in
      let* tau = gamma 1. 0.1 in
      return (mu, tau)
    in
    let obs = [8.;9.;7.;7.;8.;10.] in
    let post = observe_multiple m (fun (mu,tau) -> S.normal mu (1. /. tau)) obs in
    post
end

module InferModel 
(I: Infer) 
(M: Model with type 'a D.dist := 'a I.D.dist) 
      = 
struct
  include M
  let posterior_dist =  I.infer M.model
  let sample_posterior = I.D.sample posterior_dist
end

module NModel = NormalModel(Dist')
module MH' = MH(Dist')
(* module Test = InferModel(MH')(NModel:Model with type 'a D.dist = 'a MH'.D.dist) *)

(* let x = Test.posterior_dist *)
(* let s = Test.sample_posterior *)

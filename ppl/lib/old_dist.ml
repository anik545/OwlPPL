open Core

type prob = float
exception Undefined
exception NotImplemented

type _ sampleable = 
    Normal: float * float -> float sampleable 
  | Uniform: 'a list -> 'a sampleable 
  | Categorical: ('a * prob) list -> 'a sampleable
  | Beta: float * float -> float sampleable
  | Binomial: int * float -> int sampleable
  | C_Uniform: float * float -> float sampleable

(* TODO: find a way to save the names of variables, so we can infer any of them *)
type _ dist = 
    Return: 'a -> 'a dist
  | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
  | Primitive: 'a sampleable -> 'a dist
  | Conditional: ('a -> prob) * 'a dist -> 'a dist

(* type _ dist = 
    Return: 'a -> 'a dist
   | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
   | Primitive: 'a sampleable -> 'a dist
   | Conditional: ('a -> prob) * 'a dist -> 'a dist *)

(* Helpers for creating common distributions *)

(* discrete *)
let uniform xs = Primitive (Uniform xs)
let categorical xs = Primitive (Categorical xs)
let bernoulli p = categorical [(true, p); (false, 1. -. p)]
let choice p x y = Bind ((bernoulli p), (fun c -> if c then x else y))
let binomial n p = Binomial (n,p)

(* continuous *)
let normal mu sigma = Primitive (Normal (mu, sigma))
let beta a b = Primitive (Beta (a,b))
let c_uniform a b = Primitive (C_Uniform(a,b))

(* MONAD/FUNCTOR FUNCTIONS *)
let return x = Return x
let (>>=) d f  = Bind (d,f)

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

(* operators on probability distributions *)
let ( +~ ) = liftM2 ( + )
let ( -~ ) = liftM2 ( - )
let ( *~ ) = liftM2 ( * )

let condition c d = Conditional (c,d)

(* SAMPLING *)

(* type a is a locally abstract type *)
let sample_primitive: type a.a sampleable -> a = function
    Normal(mu,sigma) -> Owl_base_stats.gaussian_rvs ~mu:mu ~sigma:sigma
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

let pdf: type a.a sampleable -> a -> float = function
  (* cont *)
  | Normal(mu,sigma) -> Owl_stats_dist.gaussian_pdf ~mu ~sigma
  | Beta (a, b) -> Owl_stats_dist.beta_pdf ~a ~b
  | C_Uniform (a, b) -> Owl_stats_dist.uniform_pdf ~a ~b

  (* discrete *)
  | Uniform us -> fun _ -> 1. /. (float_of_int (List.length us))
  | Categorical cs -> fun x -> 
    let rec lookup l = function 
      | (a,p)::xs -> if Stdlib.(a = x) then p else lookup l xs
      | [] -> 0. (* not found *)
    in
    lookup x cs
  | Binomial (n,p) -> Owl_stats_dist.binomial_pdf ~n ~p


let observe dst noise value = condition (fun x -> pdf (noise x) value) dst

let rec sample: 'a. 'a dist -> 'a = function
    Return x -> x
  | Bind (d,f) -> let y = f (sample d) in sample y
  | Primitive d -> sample_primitive d
  | Conditional (_,_) -> raise Undefined

let sample_mean ?(n=100000) d = 
  let rec loop n d sofar = 
    if n = 0 
    then sofar
    else loop (n-1) d ((sample d) +. sofar)
  in
  (loop n d 0.) /. float_of_int n

let take_k_samples k d = Array.init k ~f:(fun _ -> sample d)

let hist_dist ?h ?(n=5000) ?(fname="fig.jpg") d = 
  let open Owl_plplot in 
  let samples = take_k_samples n d in

  let pl = match h with 
    | None -> Plot.create ~m:1 ~n:1 fname 
    | Some h -> h 
  in

  Plot.histogram ~h:pl ~bin:50 Owl.(Mat.col (Mat.of_array samples n 1) 0);
  pl

type 'a samples = ('a * prob) list
type 'a no_dup_vals_samples = ('a , prob) Map.Poly.t

let undup xs = 
  let map = Map.Poly.of_alist_fold xs ~f:(+.) ~init:0. in
  map

let dist_of_n_samples n (d: 'a dist): 'a list dist = 
  sequence @@ List.init n ~f:(fun _ -> (d))

let kl_samples (p: 'a samples) (q: 'a sampleable) = 
  (* d' has exact pdf, d can only take (weighted) samples *)
  let samples = undup p in
  let xs = Map.Poly.to_alist samples in
  let f (x,p_x) = match pdf q x with
    | 0. -> raise Undefined
    | q_x -> p_x *. log (p_x /. q_x)
  in
  List.sum (module Float) ~f xs

let kl_dist ?(n=1000) (p: ('a * prob) dist) (q: 'a sampleable) = 
  (* d' has exact pdf, d can only take (weighted) samples *)
  let sample_dist = dist_of_n_samples n p in
  let samples = undup (sample sample_dist) in
  kl_samples (Map.Poly.to_alist samples) q



(* TODO: create a prob_map module *)
let weighted_dist ?(n=300) (d: 'a dist) : ('a, int) Core.Map.Poly.t =
  let rec loop n map =
    if n = 0 then map else
      let s = sample d in 
      let map = Map.Poly.update map s ~f:(fun x -> match x with None -> 1 | Some y -> y + 1) in
      loop (n-1) map
  in
  loop n Map.Poly.empty
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

type _ dist = 
    Return: 'a -> 'a dist
  | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
  | Primitive: 'a sampleable -> 'a dist
  | Conditional: ('a -> prob) * 'a dist -> 'a dist

(* Creating distributions *)
let uniform xs = Primitive (Uniform xs)
let categorical xs = Primitive (Categorical xs)
let bernoulli p = categorical [(true, p); (false, 1. -. p)]
let binomial n p = Binomial (n,p)

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

(* operators on probability distributions *)
let ( +~ ) = liftM2 ( + )
let ( -~ ) = liftM2 ( - )
let ( *~ ) = liftM2 ( * )

let condition c d = Conditional (c,d)

(* SAMPLING *)

(* type a is a locally abstract type *)
let sample_from: type a.a sampleable -> a = function
    Normal(mu,sigma) -> Owl_base_stats.gaussian_rvs ~mu:mu ~sigma:sigma
  | Uniform xs -> (Owl_base_stats.sample (Array.of_list xs) 1).(0)
  | Categorical xs ->
    let r = Owl_base_stats.uniform_rvs ~a:0.0 ~b:1. in
    let rec loop (p_left:(a * prob) list) (remaining:prob) = match p_left with
        (v,p)::xs -> if Float.(<=) p remaining || (Stdlib.(=) xs []) then v else loop xs (remaining -. p)
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
        | (a,p)::xs -> if Stdlib.(=) a x then p else lookup l xs
        | [] -> 0. (* not found *)
      in
        lookup x cs
  | Binomial (n,p) -> Owl_stats_dist.binomial_pdf ~n ~p

let rec sample: 'a. 'a dist -> 'a = function
    Return x -> x
  | Bind (d,f) -> let y = f (sample d) in sample y
  | Primitive d -> sample_from d
  | Conditional (_,_) -> raise Undefined

(* INFERENCE *)
let rec prior': 'a.'a dist -> 'a dist = function
    Conditional (_,d) -> prior' d
  | Bind (d,f) -> Bind ((prior' d),f)
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

type 'a samples = ('a * prob) list

let resample (xs: 'a samples): ('a samples) dist =
  let n = List.length xs in
  let old_dist = categorical xs in
  sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) (old_dist)))

let flatten xss = 
  let rec f xs p = match xs with ((x,q)::t) -> (x,p*.q)::(f t p) | [] -> [] in
  let rec flatten' = 
    function 
      (xs, p)::xs' -> (f xs p) @ flatten' xs'
    | [] -> []
  in 
  flatten' xss

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
    fmap (List.map ~f:fst) (proposal >>= iterate)
    
let mh' n d = fmap (fun x -> List.nth_exn x (n-1)) (mh n d)

let flip f a b = f b a

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

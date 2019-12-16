open Core

type prob = float
exception Undefined
exception NotImplemented

type _ sampleable = 
    Normal: float * float -> float sampleable 
  | Uniform: 'a list -> 'a sampleable 
  | Categorical: ('a * prob) list -> 'a sampleable
  | Beta: (float * float) -> float sampleable

type _ dist = 
    Return: 'a -> 'a dist
  | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
  | Primitive: 'a sampleable -> 'a dist
  | Conditional: ('a -> prob) * 'a dist -> 'a dist

(* Creating distributions *)
let uniform xs = Primitive (Uniform xs)
let categorical xs = Primitive (Categorical xs)
let normal mu sigma = Primitive (Normal (mu, sigma))
let bernoulli p = categorical [(true, p); (false, 1. -. p)]
let beta a b = Primitive (Beta (a,b))

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
  | Beta (a, b) -> Owl_stats_dist.beta_rvs ~a:a ~b:b

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

let pdf (s:'a sampleable) (x:'a) :float = 
  match s with
    (* cont *)
    | Normal(mu,sigma) -> Owl_stats_dist.gaussian_pdf ~mu:mu ~sigma:sigma x
    | Beta (a, b) -> Owl_stats_dist.beta_pdf ~a:a ~b:b x

    (* discrete *)
    | Uniform us -> 1. /. (float_of_int (List.length us))
    | Categorical cs -> 
        let rec lookup l = function 
          | (a,p)::xs -> if Stdlib.(=) a x then p else lookup l xs
          | [] -> 0. (* not found *)
        in
          lookup x cs

let flip f a b = f b a



let sample_mean ?(n=100000) d = 
  let rec loop n d sofar = 
    if n = 0 
    then sofar
    else loop (n-1) d ((sample d) +. sofar)
  in
    (loop n d 0.) /. float_of_int n
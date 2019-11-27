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
let fmap f xs = xs >>= (fun x -> return (f x)) (*liftM*)

let condition c d = Conditional (c,d)

(* SAMPLING *)

(* type a is a locally abstract type *)
let sample_from: type a.a sampleable -> a = function
    Normal(mu,sigma) -> Owl_base_stats.gaussian_rvs mu sigma
  | Uniform xs -> (Owl_base_stats.sample (Array.of_list xs) 1).(0)
  | Categorical xs ->
    let r = Owl_base_stats.uniform_rvs 0.0 1. in
    let rec loop p_left remaining = match p_left with
        (v,p)::xs -> if p <= remaining || xs = [] then v else loop xs (remaining -. p)
      | [] -> raise Undefined
    in
    loop xs r
  | Beta (a, b) -> Owl_base_stats_dist_uniform.std_uniform_rvs ()

let rec sample: 'a. 'a dist -> 'a = function
    Return x -> x
  | Bind (d,f) -> let y  = f (sample d) in sample y
  | Primitive d -> sample_from d
  | Conditional (c,d) -> raise Undefined

(* INFERENCE *)
let rec prior': 'a.'a dist -> 'a dist = function
    Conditional (c,d) -> prior' d
  | Bind (d,f) -> Bind ((prior' d),f)
  | d -> d

let rec prior: 'a.'a dist -> ('a*prob) dist = function
    Conditional (c,d) -> prior d >>= (fun (x,s) -> return (x, s *. (c x)))
  | Bind (d,f) -> prior d >>= (fun (x,s) -> f x >>= (fun y -> return (y,s)))
  | d -> d >>= (fun x-> return (x,1.))

type 'a samples = ('a * prob) list


let rec sequence (xs:('a dist) list) : ('a list) dist = match xs with
    [] -> return []
  | m::ms -> m >>= (fun x -> sequence ms >>= (fun xs -> return (x::xs)))

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

let (let*) = (>>=)
let return = return

let mh d = 
  let proposal = prior d in
  let rec iterate (x,s) = 
    let* (y,r) = proposal in
    let* accept = bernoulli @@ min 1. (r /. s) in
    let next = if accept then (y,r) else (x,s) in
    let* rest = iterate next in
    return (next::rest)
  in
  fmap (List.map ~f:fst) (proposal >>= iterate)

let mh' n d = fmap (fun x -> List.nth_exn x n) (mh d)

let importance' n d = 0


let flip f a b = f b a

let coin_tossing () =
  let weight: prob dist = bernoulli 0.8 >>= (fun isFair -> if isFair then return 0.5 else beta 5. 1.) in 

  let toss (b:bool) (d:prob dist): prob dist = condition (fun w -> if b then w else 1. -. w) d in 

  let tosses (bs: bool list) (d: prob dist): prob dist = List.fold bs ~init:d ~f:(flip toss) in


  let observations = [true; true; false; false; false; false; false] in
  let posterior_weight = tosses observations weight in

  let pr = prior posterior_weight in
  (* let s = fun () -> sample pr in *)
  pr

type point_t  = float * float
type param  = float * float

let pdf (s:'a sampleable) (a:'a) :float = 
  match s with
  | Normal(mu,sigma) -> 0.5
  | Uniform xs -> raise NotImplemented
  | Categorical xs -> raise NotImplemented
  | Beta (a, b) -> raise NotImplemented


let linreg = 
  let linear: param dist = normal 0. 1. >>= (fun a -> normal 0. 1. >>= fun b -> return (a,b)) in 
  let point (x,y) d = condition (fun (a,b) -> pdf (Normal ((a *. x +. b),1.)) y) d in
  let points ps d = List.fold ~f:(flip point) ~init:d ps in

  let obs = [(0.,0.);(1.,1.);(2.,2.)] in
  let posterior = points obs linear in
  posterior





let x = linreg
let s = sample (mh x)
(* let () = print_float @@ x() *)
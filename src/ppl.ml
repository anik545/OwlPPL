open Core
type 'a prob = 'a

module type MONAD =
sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
  val fmap: ('a -> 'b) -> 'a t -> 'b t
end

module type DIST = sig
  type prob
  type 'a dist
  include MONAD with type 'a t := 'a dist
  (* val return: 'a -> 'a dist
     val (>>=): 'a dist -> ('a -> 'a dist) -> 'a dist *)
  val condition: ('a -> prob) -> 'a dist -> 'a dist
  val sample: ('a dist) -> 'a
  val uniform: ('a list) -> ('a dist)

  val categorical: ('a * prob) list -> 'a dist
end



module type SAMPLE = sig
  type 'a d
  val sample: 'a d -> 'a
end

module type DIST1 = sig
  type prob = float
  type 'a dist
  include MONAD with type 'a t := 'a dist
  include SAMPLE with type 'a d := 'a dist
  (* val return: 'a -> 'a dist
     val (>>=): 'a dist -> ('a -> 'a dist) -> 'a dist *)
  val condition: ('a -> prob) -> 'a dist -> 'a dist
  (* val sample: 'a dist -> 'a *)
  (* val categorical: ('a * prob) list -> 'a dist *)
  val run: unit
end

module type SAMPLE_DIST = sig
  include SAMPLE
  include DIST1
end

(* module type OLD_SAMPLE_DIST = sig
   type 'a dist
   include SAMPLE with type 'a d := 'a dist
   include DIST with type 'a dist := 'a dist
   end


   module Explicit: OLD_SAMPLE_DIST  = struct 
   type prob = float
   type 'a dist = ('a * float) list

   let condition f d = d
   let return x = [(x,1.0)]
   let (>>=) ((a,p)::ds) f = f a
   let sample ((a,p)::ds) = a

   let normalize xs = 
    let tot = List.map xs snd |> List.reduce ~f:((+.)) in 
    match tot with Some t -> List.map xs ~f:(fun (x,y) ->(x, y /. t))
                 | None -> xs

   let uniform l = List.map l ~f:(fun x->  (x,1.)) 
                  |> normalize 

   let categorical l = normalize l

   let bernoulli p = categorical [(true, p); (false, 1. -. p)]

   end *)

module SampleableDist = struct
  type prob = float
  exception Undefined

  type _ sampleable = 
      Normal: float * float -> float sampleable 
    | Uniform: 'a list -> 'a sampleable 
    | Categorical: ('a * prob) list -> 'a sampleable
    | Beta: (float * float) -> float sampleable

  exception NotImplemented

  let sample_normal (Normal(mu,sigma)) = Owl_base_stats.gaussian_rvs mu sigma
  let sample_uniform (Uniform xs) = (Owl_base_stats.sample (Array.of_list xs) 1).(0)

  let sample_categorical (Categorical xs) =
    let r = Owl_base_stats.gaussian_rvs 0.0 1. in
    let rec loop p_left remaining = match p_left with
        (v,p)::xs -> if p <= remaining || xs = [] then v else loop xs (remaining -. p)
      | [] -> raise Undefined
    in
    loop xs r

  (* How do I write this function to have signature 'a sampleable -> 'a ???*)

  let sample_from s = match s with
      Normal(mu,sigma) -> Owl_base_stats.gaussian_rvs mu sigma
    | Uniform xs -> (Owl_base_stats.sample (Array.of_list xs) 1).(0)
    | Categorical xs -> sample_categorical (Categorical xs)
    | Beta (a, b) -> 0.4
  (* let r = Owl_base_stats.uniform_rvs 0.0 1.0 in
     let rec loop ps_left remaining curr_idx =
     match ps_left with 
      x::xs -> if x < remaining then loop xs (remaining -. x) (curr_idx + 1) else curr_idx
     | [] -> curr_idx
     in 
     loop ps r 0 *)

  type _ dist = 
      Return: 'a -> 'a dist
    | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
    | Primitive: 'a sampleable -> 'a dist
    | Conditional: ('a -> prob) * 'a dist -> 'a dist

  let rec sample: 'a. 'a dist -> 'a = function
      Return x -> x 
    | Bind (d,f) -> let y  = f (sample d) in sample y
    | Primitive d -> sample_categorical d
    | Conditional (c,d) -> raise Undefined

  let rec float_sample: float dist -> float = function
      Return x -> x 
    | Bind (d,f) -> let y  = f (sample d) in float_sample y
    | Primitive d -> sample_from d
    | Conditional (c,d) -> raise Undefined

  let condition c d = Conditional (c,d)

  let return x = Return x
  let (>>=) d f  = Bind (d,f)
  let fmap f xs = xs >>= (fun x -> return (f x)) (*liftM*)

  let rec prior': 'a.'a dist -> 'a dist = function
      Conditional (c,d) -> prior' d
    | Bind (d,f) -> Bind ((prior' d),f)
    | d -> d

  let rec prior: 'a.'a dist -> ('a*prob) dist = function
      Conditional (c,d) -> prior d >>= (fun (x,s) -> return (x, s *. (c x)))
    | Bind (d,f) -> prior d >>= (fun (x,s) -> f x >>= (fun y -> return (y,s)))
    | d -> d >>= (fun x-> return (x,1.))

  type 'a samples = ('a * prob) list
  (* 
  let resample xs =
    let n = List.length xs in
    List.init n (fun _ -> (fmap (fun x-> (x,1.)) (categorical xs))) *)

  (* let rec sequence_list xss = match xss with 
      [] -> List.return []
     | (m::ms) ->  List.bind m ~f:(fun x -> List.bind (sequence_list ms) ~f:(fun xs -> List.return (x::xs)) )  *)

  let importance' n d = 0


  let uniform xs = Primitive (Uniform xs)
  let categorical xs = Primitive (Categorical xs)
  let normal mu sigma = Primitive (Normal (mu, sigma))
  let bernoulli p = categorical [(true, p); (false, 1. -. p)]
  let beta a b = Primitive (Beta (a,b))

  let weight: prob dist = bernoulli 0.8 >>= (fun isFair -> if isFair then return 0.5 else beta 5. 1.)

  let toss (b:bool) (d:prob dist): prob dist = condition (fun w -> if b then w else 1. -. w) d

  (* bool -> prob dist -> prob dist *)
  (* let flip b d = 
     let r = Owl_base_stats.uniform_rvs 0. 1. in
     if r < 0.5 then 
      raise NotImplemented
     else 
      raise NotImplemented *)

  let flip f d b = if b then d else f b d

  let tosses (bs: bool list) (d: prob dist): prob dist = List.fold bs ~init:d ~f:(flip toss) 


  let observations = [true; true; false; false; false; false; false]
  let posterior_weight = tosses observations weight

  let pr = prior' posterior_weight
  let s = float_sample pr
  let run = ()
  (* let (x,y) = sample (prior posterior_weight) in
     print_float x; print_float y *)

  (* let run = print_float @@ sample posterior_weight *)
end

module SampleList = struct 
  type 'a d = 'a list
  let sample xs = (Owl_base_stats.sample (Array.of_list xs) 1).(0) 
end

(* To sample from a ('a * prob) list, sampling with the correct probability *)
module SampleTupleList = struct 
  type 'a d = 'a list 

  (* Assume the probs add up to 1 *)
  (* 
  let sample xs = 
    let r = Owl_base_stats.uniform_rvs 0.0 1.0 in
    let rec loop ps_left remaining curr_idx =
      match ps_left with 
        x::xs -> if x < remaining then loop xs (remaining -. x) (curr_idx + 1) else curr_idx
      | [] -> curr_idx
    in 
    loop ps r 0 *)
end


(* module Dist: DIST1  = struct 
   type prob = float
   exception Undefined
   type _ dist = 
      Return: 'a -> 'a dist
    | Bind: 'a dist * ('a -> 'b dist) -> 'b dist
    | Primitive: 'a dist -> 'a dist
    | Conditional: ('a -> prob) * 'a dist -> 'a dist

   let rec sample: 'a. 'a dist -> 'a = function
      Return x -> x 
    | Bind (d,f) -> let y  = f (sample d) in sample y
    | Primitive d -> sample d
    | Conditional (c,d) -> raise Undefined

   let condition c d = Conditional (c,d)

   let return x = Return x
   let (>>=) d f  = Bind (d,f)

   let normalize xs = 
    let tot = List.map xs snd |> List.reduce ~f:((+.)) in 
    match tot with Some t -> List.map xs ~f:(fun (x,y) ->(x, y /. t))
                 | None -> xs

   end *)
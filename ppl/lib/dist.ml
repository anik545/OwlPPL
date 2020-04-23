open Monad

exception Undefined

module Prob = Sigs.LogProb

type prob = Prob.t

type likelihood = Prob.t

open Prob

type 'a samples = ('a * prob) list

(* enable watching intermediate variables through plots *)
(* type 'a var_dist = string * 'a dist *)
type _ dist =
  | Return : 'a -> 'a dist
  (* bind is lazy since it contains a function *)
  | Bind : 'a dist * ('a -> 'b dist) -> 'b dist
  (* | Bind_var: 'a var_dist * ('a -> 'b dist) -> 'b dist *)
  | Primitive : 'a Primitive.t -> 'a dist
  | Conditional : ('a -> prob) * 'a dist -> 'a dist

(* | Conditional: ('a -> float) * 'a var_dist -> 'a dist *)
(* | Observe: 'a Primitive.primitive * 'a * 'a dist -> 'a dist *)
(* TODO: uncomment + fix *)
(* | Independent: 'a dist * 'b dist -> ('a * 'b) dist
   let (and+) d1 d2 = Independent(d1,d2) *)
(* let observe x dst d = Observe(dst,x,d) *)
let condition' c d = Conditional (Core.Fn.compose of_float c, d)

let condition b d = Conditional ((fun _ -> if b then one else zero), d)

let score s d = Conditional ((fun _ -> of_float s), d)

let observe x dst d = Conditional ((fun _ -> of_float @@ Primitive.pdf dst x), d)

let from_primitive p = Primitive p

(* TODO: *)
(* let observe_list lst dst d = Core.List.fold_left ~f:(observe) *)

include Make_Extended (struct
  type 'a t = 'a dist

  let return x = Return x

  let bind d f = Bind (d, f)
end)

let discrete_uniform xs = Primitive (Primitive.discrete_uniform xs)

let categorical xs =
  (* Core.List.iter ~f:(fun (_, y) -> Printf.printf "%f," y) xs;
  Printf.printf "\n"; *)
  let xs =
    Core.List.filter
      ~f:(fun (_, x) -> Float.is_finite (exp x) || Float.equal x 0.)
      xs
  in
  (* let xs = Core.List.map ~f:(fun (a, x) -> (a, exp x)) xs in *)
  let normalise xs =
    let open Core in
    let norm = List.sum (module Float) ~f:snd xs in
    if Float.(norm = 0.) then
      let p' = 1. /. float_of_int (List.length xs) in
      List.map ~f:(fun (v, _) -> (v, p')) xs
    else List.map ~f:(fun (v, p) -> (v, p /. norm)) xs
  in
  let xs = normalise xs in
  (* Core.List.iter ~f:(fun (_, y) -> Printf.printf "%f," y) xs;
  Printf.printf "\n"; *)
  Primitive (Primitive.categorical xs)

let bernoulli p = categorical [ (true, p); (false, 1. -. p) ]

let choice p x y = Bind (bernoulli p, fun c -> if c then x else y)

let binomial n p = Primitive (Primitive.binomial n p)

(* continuous *)
let normal mu sigma = Primitive (Primitive.normal mu sigma)

let gamma shape scale = Primitive (Primitive.gamma shape scale)

let beta a b = Primitive (Primitive.beta a b)

let continuous_uniform a b = Primitive (Primitive.continuous_uniform a b)

let rec sample : 'a. 'a dist -> 'a = function
  | Return x -> x
  | Bind (d, f) ->
      let y = f (sample d) in
      sample y
  | Primitive d -> Primitive.sample d
  | Conditional (_, _) -> raise Undefined

(* | Independent (d1,d2) -> sample d1, sample d2 *)

let rec sample_n : 'a. int -> 'a dist -> 'a array =
 fun n -> function
  | Return x -> Array.init n (fun _ -> x)
  | Bind (d, f) ->
      let y = Array.map f (sample_n n d) in
      Array.map sample y
  | Primitive d -> Array.init n (fun _ -> Primitive.sample d)
  | Conditional (_, _) -> raise Undefined

(* same as sample (prior_with_score d) *)
let rec sample_with_score : 'a. 'a dist -> 'a * likelihood = function
  | Return x -> (x, one)
  | Bind (d, f) ->
      let y, s = sample_with_score d in
      let a, s1 = sample_with_score (f y) in
      (a, s *. s1)
  | Primitive d ->
      let x = Primitive.sample d in
      (x, of_float @@ Primitive.pdf d x)
  (* this instead?? *)
  (* this is how its done in prior - is it right??? *)
  (* | Primitive d -> let x  = Primitive.sample d in (x, 1.)  *)
  | Conditional (lik, d) ->
      let x, s = sample_with_score d in
      (x, s *. lik x)

let rec prior' : 'a. 'a dist -> 'a dist = function
  | Conditional (_, d) -> prior' d
  | Bind (d, f) -> Bind (prior' d, f)
  | d -> d

let rec prior : 'a. 'a dist -> ('a * prob) dist = function
  | Conditional (c, d) ->
      let* x, s = prior d in
      return (x, s *. c x)
  | Bind (d, f) ->
      let* x, s = prior d in
      let* y = f x in
      return (y, s)
  | d ->
      let* x = d in
      return (x, one)

(* same as sample_with_score *)
let rec prior_with_score : 'a. 'a dist -> ('a * prob) dist = function
  | Conditional (c, d) ->
      let* x, s = prior_with_score d in
      return (x, s *. c x)
  | Bind (d, f) ->
      let* x, s = prior_with_score d in
      let* y, s1 = prior_with_score (f x) in
      return (y, s *. s1)
  | Primitive d ->
      let* x = Primitive d in
      return (x, of_float @@ Primitive.pdf d x)
  | Return x -> return (x, one)

(* | Independent(d1,d2) -> 
   let* x,s1 = prior_with_score d1 
   and* y,s2 = prior_with_score d2 in
   ((x,y),s1*.s2) *)

let dist_of_n_samples n (d : 'a dist) : 'a list dist =
  sequence @@ Core.List.init n ~f:(fun _ -> d)

let rec support : 'a. 'a dist -> 'a list = function
  | Bind (d, f) ->
      let supp = support d in
      Core.List.concat_map ~f:(fun x -> support (f x)) supp
  | Conditional (_, d) -> support d
  | Primitive d -> (
      match Primitive.support d with
      | DiscreteFinite xs -> xs
      | _ -> raise Undefined )
  | Return x -> [ x ]

module PplOps = struct
  let ( +~ ) = liftM2 Base.Int.( + )

  let ( -~ ) = liftM2 ( - )

  let ( *~ ) = liftM2 ( * )

  let ( /~ ) = liftM2 ( / )

  let ( +.~ ) = liftM2 Base.Float.( + )

  let ( -.~ ) = liftM2 Base.Float.( - )

  let ( *.~ ) = liftM2 Base.Float.( * )

  let ( /.~ ) = liftM2 Base.Float.( / )

  let ( &~ ) = liftM2 ( && )

  let ( |~ ) = liftM2 ( || )

  let not = liftM not

  let ( ^~ ) = liftM2 ( ^ )
end

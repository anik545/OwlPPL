module type Monad = sig
  type 'a t
  val return: 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type EMonad = sig
  type 'a t
  include Monad with type 'a t := 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (let*): 'a t -> ('a -> 'b t) -> 'b t

  val liftM: ('a -> 'b) -> 'a t -> 'b t
  val liftM2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val mapM: ('a -> 'b t) -> 'a list -> 'b list t
  val sequence: 'a t list -> 'a list t
end


module Make_Extended(M: Monad): EMonad with type 'a t := 'a M.t = struct

  let bind = M.bind
  let return = M.return

  let (>>=) = bind 
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
    in Core.List.fold_right mlist ~f:mcons ~init:(return [])

  let rec mapM f xs =
    match xs with
    | [] -> return []
    | x::xs ->
      let* x' = f x in
      let* xs' = mapM f xs in
      return (x'::xs')
end


module Dist = struct
  type 'a dist = 'a Old_dist.dist
  module DistMonad: (Monad with type 'a t = 'a dist) = struct
    type 'a t = 'a dist
    let return x = Old_dist.Return x
    let bind d f = Old_dist.Bind (d,f)
  end
  include Make_Extended(DistMonad)

  open Old_dist
  let uniform xs = Primitive (Uniform xs)
  let categorical xs = Primitive (Categorical xs)
  let bernoulli p = categorical [(true, p); (false, 1. -. p)]
  let choice p x y = Bind ((bernoulli p), (fun c -> if c then x else y))
  let binomial n p = Binomial (n,p)

  (* continuous *)
  let normal mu sigma = Primitive (Normal (mu, sigma))
  let beta a b = Primitive (Beta (a,b))
  let c_uniform a b = Primitive (C_Uniform(a,b))

  let rec sample: 'a. 'a dist -> 'a = function
      Return x -> x
    | Bind (d,f) -> let y = f (sample d) in sample y
    | Primitive d -> sample_primitive d
    | Conditional (_,_) -> raise Undefined


end

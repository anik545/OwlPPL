module type ExactDist = sig
  type 'a t

  val pdf : 'a t -> float -> float

  val cdf : 'a t -> float -> float

  val logpdf : 'a t -> float -> float

  val logcdf : 'a t -> float -> float
end

(* A distribution that can have inference applied *)
module type ApproximateDist = sig
  type 'a t

  val pdf : 'a t -> float -> float

  val cdf : 'a t -> float -> float
end

(* A distribution that can be directly sampled from (i.e. result of inference, or a primitive) *)
module type SampleDist = sig
  type 'a t

  val sample : 'a t -> 'a

  (* generate a sampler that can be called *)
  val sampler : 'a t -> unit -> 'a
end

module type Primitive_Distributions = sig
  type 'a primitive

  val binomial : int -> float -> int primitive
  (** Create a binomial distribution, the output is the number of successes from n independent trials with probability of success p  *)

  val normal : float -> float -> float primitive

  val categorical : ('a * float) list -> 'a primitive

  val discrete_uniform : 'a list -> 'a primitive

  val beta : float -> float -> float primitive

  val gamma : float -> float -> float primitive

  val continuous_uniform : float -> float -> float primitive
end

(* How would an user add a new distribution? *)
module type Primitives = sig
  type 'a t

  include Primitive_Distributions with type 'a primitive := 'a t

  type 'a support =
    | DiscreteFinite of 'a list
    | DiscreteInfinite
    | ContinuousFinite of ('a * 'a) list (* set of endpoints *)
    | ContinuousInfinite
    | Merged of 'a support * 'a support

  val sample : 'a t -> 'a

  val pdf : 'a t -> 'a -> float

  val cdf : 'a t -> 'a -> float

  val logpdf : 'a t -> 'a -> float

  val support : 'a t -> 'a support
end

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

module type Prob = sig
  type t

  (* type t =  float *)

  val to_float : t -> float

  val of_float : float -> t

  val ( *. ) : t -> t -> t

  val ( /. ) : t -> t -> t

  val ( + ) : t -> t -> t

  val one : t

  val zero : t
end

module LogProb : Prob = struct
  type t = float

  let to_float p = exp p

  let of_float p = log p

  let ( *. ) = ( +. )

  let ( /. ) = ( -. )

  let ( + ) x y =
    if x = -.Float.infinity && y = -.Float.infinity then -.Float.infinity
    else
      (* https://en.wikipedia.org/wiki/Log_probability *)
      let x' = Float.max x y in
      let y' = Float.min x y in
      (* log1p = log(1+x) *)
      x' +. log1p (exp (y' -. x'))

  let one = 0. (* log 1 *)

  let zero = -.Float.infinity (* log 0 *)
end

module FloatProb : Prob = struct
  type t = float

  let to_float p = p

  let of_float p = p

  let ( *. ) = ( *. )

  let ( /. ) = ( /. )

  let ( + ) = ( +. )

  let one = 1.

  let zero = 0.
end

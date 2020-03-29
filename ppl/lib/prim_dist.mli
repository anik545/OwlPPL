exception NotImplemented
type 'a support = Discrete of 'a list | Continuous
module type PRIM_DIST =
sig
  type t
  val sample : unit -> t
  val pdf : t -> float
  val cdf : t -> float
  val support : t support
end

type 'a prim_dist = (module PRIM_DIST with type t = 'a)
val binomial : int -> float -> int prim_dist
val categorical : ('a * float) list -> 'a prim_dist
val normal : float -> float -> float prim_dist
val discrete_uniform : 'a list -> 'a prim_dist
val beta : float -> float -> float prim_dist
val gamma : float -> float -> float prim_dist
val continuous_uniform :
  float -> float -> float prim_dist


val pdf : 'a prim_dist -> 'a -> float
val logpdf : 'a prim_dist -> 'a -> float
val cdf : 'a prim_dist -> 'a -> float
val sample : 'a prim_dist -> 'a
val support : 'a prim_dist -> 'a support


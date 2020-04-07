(** The type of supports - the values with a distribution can take *)
type 'a support =
  | DiscreteFinite of 'a list  (** A list of valid values *)
  | DiscreteInfinite  (** discrete dist with infinite support e.g. poisson*)
  | ContinuousFinite of ('a * 'a) list  (** set of endpoints *)
  | ContinuousInfinite  (** continuous dist with an infinite support e.g.  *)
  | Merged of 'a support * 'a support  (** combination of any of the above *)

(* type 'a support = Discrete of 'a list | Continuous *)
(* This allows users to define distributions with more functionality
   which they can use in their programs, but isn't needed here *)

(** The signature for new primitives distributions *)
module type PRIM_DIST = sig
  type t

  val sample : unit -> t

  val pdf : t -> float

  val cdf : t -> float

  val support : t support
end

type 'a primitive = (module PRIM_DIST with type t = 'a)
(** Type of primitive dists wrapping a module *)

(** {2:inbuilt_dists Predefined Distributions} *)

val binomial : int -> float -> int primitive
(**  *)

val categorical : ('a * float) list -> 'a primitive
(**  *)

val normal : float -> float -> float primitive
(**  *)

val discrete_uniform : 'a list -> 'a primitive
(**  *)

val beta : float -> float -> float primitive
(**  *)

val gamma : float -> float -> float primitive
(**  *)

val poisson : float -> int primitive
(**  *)

val continuous_uniform : float -> float -> float primitive
(**  *)

(* {2: Basic Operations} *)

val pdf : 'a primitive -> 'a -> float

val logpdf : 'a primitive -> 'a -> float

val cdf : 'a primitive -> 'a -> float

val sample : 'a primitive -> 'a

val support : 'a primitive -> 'a support

val new_primitive :
  sample:(unit -> 'a) ->
  pdf:('a -> float) ->
  cdf:('a -> float) ->
  support:'a support ->
  (module PRIM_DIST with type t = 'a)

val merge_supports : 'a support * 'a support -> 'a support

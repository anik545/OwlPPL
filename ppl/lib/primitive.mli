(** Module defining a type for primitive distributions


*)

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

(** The signature for new primitive distributions *)
module type PRIM_DIST = sig
  type t

  val sample : unit -> t

  val pdf : t -> float

  val cdf : t -> float

  val ppf : t -> float

  val support : t support
end

type 'a t
(** Type of primitive dists wrapping a module *)

(** {2:new_prim New Distributions} *)

val create_primitive :
  sample:(unit -> 'a) ->
  pdf:('a -> float) ->
  cdf:('a -> float) ->
  support:'a support ->
  ppf:('a -> float) ->
  'a t

(** {2:inbuilt_dists Predefined Distributions} *)

val binomial : int -> float -> int t
(**  *)

val categorical : ('a * float) list -> 'a t
(**  *)

val normal : float -> float -> float t
(**  *)

val discrete_uniform : 'a list -> 'a t
(**  *)

val beta : float -> float -> float t
(**  *)

val gamma : float -> float -> float t
(**  *)

val poisson : float -> int t
(**  *)

val continuous_uniform : float -> float -> float t
(**  *)

(** {2:basic_ops Basic Operations} *)

val pdf : 'a t -> 'a -> float

val logpdf : 'a t -> 'a -> float

val cdf : 'a t -> 'a -> float

val ppf : 'a t -> 'a -> float

val sample : 'a t -> 'a

val support : 'a t -> 'a support

(** {2:prim_other Other} *)

val merge_supports : 'a support * 'a support -> 'a support

(** Module used for defining probabilistic models  

    Contains a type {!type-dist} which is used to represent probabilistic models.
*)

exception Undefined

module Prob : Sigs.Prob
(** The module used to represent probability, can be switched to use log probs *)

type prob = Prob.t
(** A type for which values need to sum to 1 (not an enforced property) *)

type likelihood = Prob.t
(** A type for which values don't need to sum to 1 (not an enforced property) *)

type 'a samples = ('a * prob) list
(** A set of weighted samples, summing to one *)

(** GADT for representing distributions, private to avoid direct manipulation *)
type _ dist = private
  | Return : 'a -> 'a dist  (** distribution with a single value *)
  (* bind is lazy since it contains a function *)
  | Bind : 'a dist * ('a -> 'b dist) -> 'b dist  (** monadic bind *)
  (* | Bind_var: 'a var_dist * ('a -> 'b dist) -> 'b dist *)
  | Primitive : 'a Primitive.t -> 'a dist  (** primitive exact distribution *)
  | Conditional : ('a -> likelihood) * 'a dist -> 'a dist
      (** variant that defines likelihood model *)
  | Independent : 'a dist * 'b dist -> ('a * 'b) dist
      (** for combining two independent distributions *)

(**/**)

val from_primitive : 'a Primitive.t -> 'a dist

(**/**)

(** {2:dist_monad Condition Operators} *)

val condition' : ('a -> float) -> 'a dist -> 'a dist
(** The most general condition operator *)

val condition : bool -> 'a dist -> 'a dist
(** Hard conditioning *)

val score : float -> 'a dist -> 'a dist
(** Soft conditioning, add a constant score to a trace *)

val observe : 'a -> 'a Primitive.t -> 'b dist -> 'b dist
(** Soft conditioning for observations from a known distribution *)

(** {2:dist_monad Monad Functions} 
    Monad functions
*)

include Monad.Monad with type 'a t := 'a dist
(** @inline *)

val ( and* ) : 'a dist -> 'b dist -> ('a * 'b) dist

(** {2:dist_prims Primitives} 
    These functions create {!type-dist} values which correspond to primitive distributions 
    so that they can be used in models.
*)
include
  Sigs.Primitive_Distributions with type 'a primitive := 'a dist
(** @inline *)

val bernoulli : float -> bool dist

(**/**)

val choice : float -> 'a dist -> 'a dist -> 'a dist

(**/**)

(** {2:dist_sample Sampling} *)

val sample : 'a dist -> 'a
(**  *)

val sample_n : int -> 'a dist -> 'a array
(**  *)

val sample_with_score : 'a dist -> 'a * likelihood
(**  *)

(**/**)

val dist_of_n_samples : int -> 'a dist -> 'a list dist
(**  *)

(**/**)

(** {2:prior Prior Distribution} *)

val prior' : 'a dist -> 'a dist

(**/**)

val prior : 'a dist -> ('a * likelihood) dist
(**  *)

(**/**)

val prior_with_score : 'a dist -> ('a * likelihood) dist
(**  *)

val support : 'a dist -> 'a list
(**  *)

module PplOps : Sigs.Ops with type 'a dist := 'a dist
(** Common operators for combining distributions *)

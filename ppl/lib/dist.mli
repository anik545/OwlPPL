(** Module used for defining probabilistic models  

    Contains a type {!type-dist} which is used to represent probabilistic models.
*)

exception Undefined

module Prob : Sigs.Prob

type prob = Prob.t
(** A type for which values need to sum to 1  *)

type likelihood = Prob.t
(** A type for which values don't need to sum to 1  *)

type 'a samples = ('a * prob) list
(** A set of weighted samples, summing to one *)

(** Type for representing distributions *)
type _ dist = private
  | Return : 'a -> 'a dist  (** distribution with a single value *)
  (* bind is lazy since it contains a function *)
  | Bind : 'a dist * ('a -> 'b dist) -> 'b dist  (** monadic bind *)
  (* | Bind_var: 'a var_dist * ('a -> 'b dist) -> 'b dist *)
  | Primitive : 'a Primitive.t -> 'a dist  (** primitive exact distribution *)
  | Conditional : ('a -> likelihood) * 'a dist -> 'a dist
      (** variant that defines likelihood model *)

(* | Conditional: ('a -> float) * 'a var_dist -> 'a dist *)

(** {2:dist_monad Condition Operators} *)

val condition' : ('a -> float) -> 'a dist -> 'a dist

val condition : bool -> 'a dist -> 'a dist

val score : float -> 'a dist -> 'a dist

val observe : 'a -> 'a Primitive.t -> 'b dist -> 'b dist

val from_primitive : 'a Primitive.t -> 'a dist

(** {2:dist_monad Monad Fsunctions} *)

include Monad.Monad with type 'a t := 'a dist

(** {2:dist_prims Primitives} 
    These functions create {!type-dist} values which correspond to primitive distributions 
    so that they can be used in models. Ok
*)
include
  Sigs.Primitive_Distributions with type 'a primitive := 'a dist
(** @inline *)

val bernoulli : float -> bool dist

val choice : float -> 'a dist -> 'a dist -> 'a dist

(** {2:dist_sample Sampling} *)

val sample : 'a dist -> 'a

val sample_n : int -> 'a dist -> 'a array

val sample_with_score : 'a dist -> 'a * likelihood

val dist_of_n_samples : int -> 'a dist -> 'a list dist

(** {2:prior Prior Distribution} *)

val prior' : 'a dist -> 'a dist

val prior : 'a dist -> ('a * likelihood) dist

val prior_with_score : 'a dist -> ('a * likelihood) dist

val support : 'a dist -> 'a list

module PplOps : Sigs.Ops with type 'a dist := 'a dist
(** Operators for distributions *)

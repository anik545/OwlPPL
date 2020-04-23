(** Implementation of inference algorithms 

    Inference algorithms to be called on probabilistic models defined using {!module-Dist}
*)

open Dist

exception Undefined

type 'a samples = ('a * prob) list

(** {2:inf_helpers Helpers} *)

val unduplicate : 'a samples -> 'a samples

val resample : 'a samples -> 'a samples dist

val normalise : 'a samples -> 'a samples

val flatten : ('a samples * prob) list -> 'a samples

(** {2:inf_exact Exact Inference} *)

val enumerate : 'a dist -> float -> 'a samples

val exact_inference : 'a dist -> 'a dist

(** {2:inf_imp Importance Sampling} *)

val importance : int -> 'a dist -> 'a samples dist

val importance' : int -> 'a dist -> 'a dist

(** {2:rej_helpers Rejetion Sampling} *)

type rejection_type = Hard | Soft

val pp_rejection_type : Format.formatter -> rejection_type -> unit

val show_rejection_type : rejection_type -> string

val create' : int -> 'a option dist -> 'a list -> 'a list

val create : int -> 'a option dist -> 'a list

val reject_transform_hard : ?threshold:float -> 'a dist -> ('a * prob) dist

val reject'' : 'a dist -> 'a option dist

val reject_transform_soft : 'a dist -> ('a * prob) dist

val rejection_transform : ?n:int -> rejection_type -> 'a dist -> 'a dist

val rejection_soft : 'a dist -> ('a * prob) option dist

val rejection_hard :
  ?threshold:Core.Float.t -> 'a dist -> ('a * prob) option dist

val rejection : ?n:int -> rejection_type -> 'a dist -> 'a dist

(** {2:inf_smc Sequential Monte Carlo} *)

val smc : int -> 'a dist -> 'a samples dist

val smc' : int -> 'a dist -> 'a dist

val smcStandard : int -> 'a dist -> 'a samples dist

val smcStandard' : int -> 'a dist -> 'a dist

val smcMultiple : int -> int -> 'a dist -> 'a samples dist

val smcMultiple' : int -> int -> 'a dist -> 'a dist

(** {2:inf_mh Metropolis Hastings} *)

val mh' : int -> 'a dist -> 'a dist

val mh'' : int -> 'a dist -> 'a dist

val mh_sampler : int -> 'a dist -> 'a list dist

val mh_transform : burn:int -> 'a dist -> 'a dist

(** {2:inf_pmcmc Particle Independent Metropolis Hastings} *)

val pimh : int -> 'a dist -> 'a samples list dist

val pimh' : int -> int -> 'a dist -> 'a dist

(** {2:inf_pc Particle Cascade} *)

val resamplePC : ('a * float) list -> int -> ('a * prob) list dist

val cascade : int -> 'a dist -> 'a samples dist

val cascade' : int -> 'a dist -> 'a dist

(** {2:com Common} *)

type infer_strat =
  | MH of int
  | SMC of int
  | PC of int
  | PIMH of int
  | Importance of int
  | Rejection of int * rejection_type
  | RejectionTrans of int * rejection_type
  | Prior
  | Enum
  | Forward

val pp_infer_strat : Format.formatter -> infer_strat -> unit

val show_infer_strat : infer_strat -> string

val print_infer_strat : infer_strat -> string

val print_infer_strat_short : infer_strat -> string

val infer : 'a dist -> infer_strat -> 'a dist

val infer_sampler : 'a dist -> infer_strat -> unit -> 'a

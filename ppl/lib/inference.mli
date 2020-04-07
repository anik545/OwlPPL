open Dist.GADT_Dist

exception Undefined

type 'a samples = ('a * float) list

val unduplicate : 'a samples -> 'a samples

val resample : 'a samples -> 'a samples dist

val normalise : 'a samples -> 'a samples

val flatten : ('a samples * float) list -> 'a samples

val enumerate : 'a dist -> float -> 'a samples

val exact_inference : 'a dist -> 'a dist

val importance : int -> 'a dist -> 'a samples dist

val importance' : int -> 'a dist -> 'a dist

type rejection_type = Hard | Soft

val pp_rejection_type : Format.formatter -> rejection_type -> unit

val show_rejection_type : rejection_type -> string

val create' : int -> 'a option dist -> 'a list -> 'a list

val create : int -> 'a option dist -> 'a list

val reject_transform_hard : ?threshold:float -> 'a dist -> ('a * float) dist

val reject'' : 'a dist -> 'a option dist

val reject_transform_soft : 'a dist -> ('a * float) dist

val rejection_transform : ?n:int -> rejection_type -> 'a dist -> 'a dist

val rejection_soft : 'a dist -> ('a * float) option dist

val rejection_hard :
  ?threshold:Core.Float.t -> 'a dist -> ('a * float) option dist

val rejection : ?n:int -> rejection_type -> 'a dist -> 'a dist

val smc : int -> 'a dist -> 'a samples dist

val smc' : int -> 'a dist -> 'a dist

val smcStandard : int -> 'a dist -> 'a samples dist

val smcStandard' : int -> 'a dist -> 'a dist

val smcMultiple : int -> int -> 'a dist -> 'a samples dist

val smcMultiple' : int -> int -> 'a dist -> 'a dist

val mh' : int -> 'a dist -> 'a dist

val mh'' : int -> 'a dist -> 'a dist

val mh_sampler : int -> 'a dist -> 'a list dist

val mh : burn:int -> 'a dist -> unit -> 'a

val mh_transform : burn:int -> 'a dist -> 'a dist

val pimh' : int -> int -> 'a dist -> 'a dist

val resamplePC : 'a samples -> int -> 'a samples dist

val cascade : int -> 'a dist -> 'a samples dist

val cascade' : int -> 'a dist -> 'a dist

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

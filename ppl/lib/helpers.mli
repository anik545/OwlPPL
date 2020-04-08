(** Utilities for working with distributions 

    A set of utilities for generating statistics and printing distributions
*)

open Dist

(** {2:samples Samples} *)

val sample_mean : ?n:int -> float dist -> float
(** *)

val sample_variance : ?n:int -> float dist -> float
(** *)

val take_k_samples : int -> 'a dist -> 'a array
(** *)

val undup : ('a * float) list -> ('a, float) Core.Map.Poly.t
(** *)

val weighted_dist : ?n:int -> 'a dist -> ('a, int) Core.Map.Poly.t
(** *)

(** {2:print Printing} *)

val print_exact_exn :
  (module Base.Stringable.S with type t = 'a) -> 'a dist -> unit
(** *)

val print_exact_bool : bool dist -> unit
(** *)

val print_exact_int : int dist -> unit
(** *)

val print_exact_float : float dist -> unit
(** *)

(** {2:other_helpers Others} *)

val time : (unit -> 'a) -> 'a
(** *)

val memo : ('a -> 'b) -> 'a -> 'b
(** *)

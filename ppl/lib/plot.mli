(** Plotting utilies

    [Plot] provides helper functions that wrap [Owl_plplot] to graph PPL distributions
*)

open Owl_plplot.Plot
open Dist.GADT_Dist
open Prim_dist

(** {2:histograms Histograms}  *)

val hist_dist_continuous :
  ?h:handle -> ?n:int -> ?fname:string -> float dist -> handle
(** *)

val hist_dist_discrete :
  ?h:handle -> ?n:int -> ?fname:string -> float dist -> handle
(** *)

(** {2:other_plots Other Plots}*)

val qq_plot :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  float dist ->
  float primitive ->
  handle
(** *)

val prob_plot :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  float dist ->
  float primitive ->
  handle
(** *)

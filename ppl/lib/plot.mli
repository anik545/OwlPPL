(** Plotting utilies

    [Plot] provides helper functions that wrap [Owl_plplot] to graph PPL distributions
*)

open Owl_plplot.Plot
open Dist

(** {2:histograms Histograms}  *)

val hist_dist_continuous :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?title:string ->
  ?xlabel:string ->
  ?ylabel:string ->
  float dist ->
  handle
(** *)

val hist_dist_discrete :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?title:string ->
  ?xlabel:string ->
  ?ylabel:string ->
  float dist ->
  handle
(** *)

(** {2:other_plots Other Plots}*)

val qq_plot :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?title:string ->
  ?xlabel:string ->
  ?ylabel:string ->
  float dist ->
  float Primitive.t ->
  handle
(** *)

val pp_plot :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?title:string ->
  ?xlabel:string ->
  ?ylabel:string ->
  float dist ->
  float Primitive.t ->
  handle
(** *)

val ecdf_continuous :
  ?h:Owl_plplot.Plot.handle ->
  ?n:int ->
  ?fname:string ->
  ?title:string ->
  ?xlabel:string ->
  ?ylabel:string ->
  float Dist.dist ->
  Owl_plplot.Plot.handle
(** *)

val ecdf_discrete :
  ?h:Owl_plplot.Plot.handle ->
  ?n:int ->
  ?fname:string ->
  ?title:string ->
  ?xlabel:string ->
  ?ylabel:string ->
  float Dist.dist ->
  Owl_plplot.Plot.handle
(** *)

val add_exact_pdf :
  ?scale:float ->
  dist:float Primitive.t ->
  Owl_plplot.Plot.handle ->
  Owl_plplot.Plot.handle
(** *)

val show : handle -> unit
(** *)

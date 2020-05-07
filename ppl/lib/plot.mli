(** Plotting utilies

    [Plot] provides helper functions that wrap [Owl_plplot] to graph PPL distributions
*)

(**/**)

type handle = Owl_plplot.Plot.handle

type 'a dist = 'a Dist.dist

(**/**)

type options = [ `X_label of string | `Y_label of string | `Title of string ]

(** {2:histograms Histograms}  *)

val pdf_continuous :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?options:[ `Title of string | `X_label of string | `Y_label of string ] list ->
  float dist ->
  handle

val hist_dist_continuous :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?options:options list ->
  float dist ->
  handle
(** *)

val hist_dist_discrete :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?options:options list ->
  float dist ->
  handle
(** *)

(** {2:other_plots Other Plots}*)

val qq_plot :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?options:options list ->
  float dist ->
  float Primitive.t ->
  handle
(** *)

val pp_plot :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?options:options list ->
  float dist ->
  float Primitive.t ->
  handle
(** *)

val ecdf_continuous :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?options:options list ->
  float Dist.dist ->
  handle
(** *)

val ecdf_discrete :
  ?h:handle ->
  ?n:int ->
  ?fname:string ->
  ?options:options list ->
  float dist ->
  handle
(** *)

val add_exact_pdf :
  ?scale:float ->
  ?start:float ->
  ?fin:float ->
  dist:float Primitive.t ->
  handle ->
  handle
(** *)

val add_exact_cdf :
  ?scale:float ->
  ?start:float ->
  ?fin:float ->
  dist:float Primitive.t ->
  handle ->
  handle
(** *)

val show : handle -> unit
(** *)

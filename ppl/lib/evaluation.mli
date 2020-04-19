(** A module for evaluating the correctness of models and inference procedures 

    Contains functionality to perform hypothesis tests and KL-divergences for both 
    continuous and discrete distributions
*)

type 'a samples = 'a Empirical.Discrete.t

type 'a dist = 'a Dist.dist

(** {2:kl KL-Divergence} *)

val kl_discrete : ?n:int -> 'a Primitive.t -> 'a dist -> float
(** Find the KL divergence for two discrete distributions *)

val kl_continuous : ?n:int -> float Primitive.t -> float dist -> float
(** Find the KL divergence for two continuous distributions *)

val kl_cum_discrete :
  int array -> bool Primitive.t -> bool dist -> (int * float) array
(**  *)

val kl_cum_continuous :
  int array -> float Primitive.t -> float dist -> (int * float) array
(**  *)

(** {2:hyp_tests Hypothesis Tests} *)

val kolmogorov_smirnov :
  ?n:int ->
  ?alpha:float ->
  float dist ->
  float Primitive.t ->
  Owl_stats.hypothesis
(** Perform kolmogorov smirnov test, returns a [hypothesis] which is true if the null hypothesis is rejected  *)

val chi_sq :
  ?n:int -> ?alpha:float -> 'a dist -> 'a Primitive.t -> Owl_stats.hypothesis
(** Perform chi-squared test, returns a [hypothesis] which is true if the null hypothesis is rejected  *)

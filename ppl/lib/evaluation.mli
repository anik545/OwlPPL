(** A module for evaluating the correctness of models and inference procedures *)


type 'a samples = 'a Empirical.Discrete.t
type 'a primitive = 'a Prim_dist.primitive
type 'a dist = 'a Dist.GADT_Dist.dist

val kl_discrete: 
  ?n:int -> (** number of samples to use *)
  'a primitive -> 
  'a dist -> 
  float
(** Find the KL divergence for two discrete distributions *)

val kl_continuous: 
  ?n:int -> (** number of samples to use *)
  float primitive -> 
  float dist -> 
  float
(** Find the KL divergence for two continuous distributions *)

val kolmogorov_smirnov: 
  ?n:int -> (** number of samples to use *)
  ?alpha:float -> (** significance level *)
  float dist -> 
  float primitive -> Owl_stats.hypothesis
(** Perform kolmogorov smirnov test, returns a [hypothesis] which is true if the null hypothesis is rejected  *)

val chi_sq:
  ?n:int -> 
  ?alpha:float -> 
  'a dist -> 
  'a primitive -> Owl_stats.hypothesis
(** Perform chi-squared test, returns a [hypothesis] which is true if the null hypothesis is rejected  *)

val kl_cum_discrete: int array -> 'a primitive -> 'a dist -> (int * float) array
(**  *)

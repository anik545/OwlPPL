module type Empirical = sig
  type 'a t
  type 'a support = Discrete of 'a array | Continuous

  val from_dist : ?n:int -> 'a dist -> 'a t
  val empty : 'a t
  val add_sample : 'a t -> 'a -> 'a t
  val get_prob : 'a t -> 'a -> float
  val to_pdf : 'a t -> 'a -> float
  val to_cdf : 'a t -> 'a -> float
  val to_list : 'a t -> 'a list
  val support : 'a t -> 'a support
end

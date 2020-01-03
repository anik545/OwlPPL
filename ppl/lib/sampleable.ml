module type Sampleable = sig
  type 'a t
  val sample: 'a t -> 'a
end
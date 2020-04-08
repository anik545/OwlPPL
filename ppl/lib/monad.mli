(** module with standard monad interface *)

(** Basic functions required for definition of a monad *)
module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(** Extended monad function signatures *)
module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val fmap : ('a -> 'b) -> 'a t -> 'b t

  val liftM : ('a -> 'b) -> 'a t -> 'b t

  val liftM2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t

  val sequence : 'a t list -> 'a list t
end

(** Functor to create extended monad functions from a basic definition *)
module Make_Extended (M : S) : Monad with type 'a t := 'a M.t

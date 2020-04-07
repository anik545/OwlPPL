module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type Monad = sig
  type 'a t

  include S with type 'a t := 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val fmap : ('a -> 'b) -> 'a t -> 'b t

  val liftM : ('a -> 'b) -> 'a t -> 'b t

  val liftM2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val mapM : ('a -> 'b t) -> 'a list -> 'b list t

  val sequence : 'a t list -> 'a list t
end

module Make_Extended (M : S) : Monad with type 'a t := 'a M.t = struct
  let bind = M.bind

  let return = M.return

  let ( >>= ) = bind

  let ( let* ) = ( >>= )

  let fmap f xs =
    let* x = xs in
    return (f x)

  let liftM = fmap

  let liftM2 f ma mb =
    let* a = ma in
    let* b = mb in
    return (f a b)

  let sequence mlist =
    let mcons p q =
      let* x = p in
      let* y = q in
      return (x :: y)
    in
    Core.List.fold_right mlist ~f:mcons ~init:(return [])

  let rec mapM f xs =
    match xs with
    | [] -> return []
    | x :: xs ->
        let* x' = f x in
        let* xs' = mapM f xs in
        return (x' :: xs')
end

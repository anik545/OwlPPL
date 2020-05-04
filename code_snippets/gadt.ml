type _ dist =
  | Return : 'a -> 'a dist
  | Bind : 'a dist * ('a -> 'b dist) -> 'b dist
  | Primitive : 'a primitive -> 'a dist
  | Conditional : ('a -> float) * 'a dist -> 'a dist
  | Independent : 'a dist * 'b dist -> ('a * 'b) dist

let return x = Return x
let bind d f = Bind (d, f)
let product d1 d2 = Independent (d1, d2)

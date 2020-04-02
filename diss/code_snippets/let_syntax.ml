open Option
(* new syntax *)
let add_options x y = 
  let* a = x in
  let* b = y in
  x+y
(* transforms to *)
let add_options x y = 
  x >>= (fun a -> 
      y >>= (fun b ->
          a+b))

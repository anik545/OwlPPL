open Option

let add_options x y z = 
  let* a = x in
  and* b = y in
  Some (a+b)

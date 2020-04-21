type 'a dist = ('a, float) Core.Map.Poly.t

let normalize map = (* omitted *)_

let bind d f = 
  let new_map = mapi d ~f:(fun ~key ~data -> data *. f key) in
  normalize new_map

let return x = 
  let m = empty in  
  add m x 1.

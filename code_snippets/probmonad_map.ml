type 'a dist = ('a, float) Map.Poly.t

let normalize map = (* omitted *)_

let bind d f = 
  fold d ~init:empty ~f:(fun ~key ~data sofar -> 
      Map.merge sofar (f key)) 
      ~combine:(fun ~key -> ( *. ))
  |> normalize_map

(* create a map with a single pair, x:1 *)
let return x = add (empty) x 1.
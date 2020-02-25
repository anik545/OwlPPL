open Core

type 'a dist = ('a, float) Map.Poly.t

let bind d f = 


  let return x = [(x,1.)]

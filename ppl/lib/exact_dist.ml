(* open Core


   type 'a dist = ('a * float) list


   let bind d f = 
   let mult_snd p = List.map ~f:(fun (a,x) -> (a,x*.p)) in
   List.concat_map ~f:(fun (x,p) -> mult_snd (f x) p) d

   let return x = [(x,1.)] *)

open Core

type 'a dist = ('a, float) Map.Poly.t

let bind d f = 
  let open Map.Poly in
  let l = of_alist d in
  f l


let return x = [(x,1.)]


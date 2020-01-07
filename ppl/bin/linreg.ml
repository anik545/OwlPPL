open Ppl
open Core

type point_t  = float * float
type param  = float * float

let linreg = 
  let linear = 
    let* a = normal 0. 2. in
    let* b = normal 0. 2. in
    let* c = gamma 1. 1. in
    return (a,b,c)
  in
  let open Float in
  let point d (x,y) = condition (fun (a,b,c) -> Primitives.(pdf @@ normal (a*x+b) c) y) d in

  let points ps d = List.fold ~f:(point) ~init:d ps in

  let obs = List.init 10 ~f:(fun x -> let x = float_of_int x in (x,x*.2.+.5.)) in
  (* let obs = List.init 10 ~f:(fun x -> let x = float_of_int x in (x,x*.2.)) in *)

  let posterior = points obs linear in
  posterior


let (+++) f g x = f (g x) 
let f1 (x,_,_) = x
let f2 (_,y,_) = y


(* let s = sample param_dist *)

(* let m = sample_mean ~n:500 m_dist
   let c = sample_mean ~n:500 c_dist *)

(* let handle = hist_dist ~fname:"linreg.png" ~n:500 m_dist
   let () = Owl_plplot.Plot.output handle *)

let param_dist = (pimh' 100 100 linreg)
let m_dist = fmap (f1) param_dist
let c_dist = fmap (f2) param_dist

let () = Printf.printf "y = %f*x + %f\n" (sample_mean m_dist) (sample_mean c_dist)

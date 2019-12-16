open Ppl
open Core

type point_t  = float * float
type param  = float * float

let linreg = 
  let linear = 
    let* a = normal 0. 1. in
    let* b = normal 0. 1. in 
    return (a,b)
  in

  let point (x,y) d = condition (fun (a,b) -> pdf (Normal (a *. x +. b, 1.)) y) d in

  let points ps d = List.fold ~f:(flip point) ~init:d ps in

  let obs = [(0.,0.);(1.,2.);(2.,4.);(2.,6.)] in
  let posterior = points obs linear in
  posterior

let param_dist = (mh' 67 linreg)

let m_dist = fmap fst param_dist
let c_dist = fmap snd param_dist

let s = sample param_dist

let m = sample_mean ~n:500 m_dist
let c = sample_mean ~n:500 c_dist

(* let () = Printf.printf "%f %f\n" (sample_mean m_dist) (sample_mean c_dist) *)

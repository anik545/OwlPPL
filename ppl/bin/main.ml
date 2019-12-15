open Ppl
open Core

let flip f a b = f b a

let coin_tossing () =
  let weight: prob dist = bernoulli 0.8 >>= (fun isFair -> if isFair then return 0.5 else beta 5. 1.) in 

  let toss (b:bool) (d:prob dist): prob dist = condition (fun w -> if b then w else 1. -. w) d in 

  let tosses (bs: bool list) (d: prob dist): prob dist = List.fold bs ~init:d ~f:(flip toss) in


  let observations = [true; true; false; false; false; false; false] in
  let posterior_weight = tosses observations weight in

  let pr = prior posterior_weight in
  (* let s = fun () -> sample pr in *)
  pr

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

  let obs = [(0.,0.);(1.,1.);(2.,2.);(3.,3.)] in
  let posterior = points obs linear in
  posterior

let x = linreg
let param_dist = (mh' 67 x)
let s = sample param_dist

let () = Printf.printf "%f %f\n" (fst s) (snd s)

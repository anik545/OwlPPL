open Random
open Owl_base_stats
open Core
open Printf

let numParticles = 300

(* put into module? *)
type 'a particle = 'a * float

let mean_var l: (float*float) = (mean l, var l)

let particleMap f ((v,lw):'a particle) : 'a particle = (f(v), lw)

type 'a prob = 'a particle list

let particleMap f ((v,lw): 'a particle) = (f(v), lw)

let probMap f probs: 'b particle list = List.map probs (particleMap f)

let floatSum = List.fold ~init:0. ~f:(+.)

let multinomial_sample (ps: float list) :int=
  let tot = floatSum ps in
  let u: float = uniform_rvs 0. tot in
  let rec loop ps_left remaining curr_idx = 
    match ps_left with 
      x::xs -> if x < remaining then loop xs (remaining -. x) (curr_idx + 1) else curr_idx
    | [] -> curr_idx
  in 
  loop ps u 0


let resample ?(n=300) (prob: 'a prob) : 'a prob =
  let lw = List.map prob (fun (v,lw) -> lw) in
  let mx = match List.reduce lw max with
    | Some x -> x
    | None -> 0. in
  let rw = List.map lw (fun lwi -> exp (lwi -. mx)) in
  (* let () = List.iter ~f:(printf "%f ") rw in *)
  let law = mx +. log (floatSum rw /. float_of_int (List.length rw)) in
  let ind = List.init n ~f:(fun _ -> multinomial_sample rw) in
  let newParticles = List.map ind (fun x -> List.nth_exn prob x) in
  let modifiedParticles = List.map newParticles (fun (v,_) -> (v,law)) in
  modifiedParticles


let probBind (f:'a -> 'b prob) (prob: 'a prob) : 'b prob = 
  prob
  |> (List.map ~f:(fun (v,lw) -> probMap (fun (psiv, psilw) -> (psiv, lw +. psilw)) (f v))) 
  |> List.concat
  |> resample


let unweighted ?(lw=0.) (ts: 'a list) : 'a prob = List.map ts (fun t -> (t,lw))

(* All these methods for a uniform on [a,b] *)

type uniform = float*float

let ll ((a, b): uniform)  (obs:float) = if obs < a || obs > b then 0.0 else log (1. /. (b -. a))

let ll_list ((a,b):uniform) (obss) = floatSum (List.map obss (ll (a,b)))

let fit ((a,b):uniform) (particles: 'a prob) (obs: 'a list) : 'a prob  = List.map particles (fun (v,lw) -> (v, lw +. ll_list (a,b) (obs) ))
let fitQ ((a,b):uniform) (particles: 'a prob) (obs: 'a list): 'a prob = match obs with 
    x::xs -> [(x, ll_list (a,b) xs)]
  | [] -> []

let particles_uniform ?(n=300) (a,b)  = unweighted (List.init n (fun x -> uniform_rvs a b))

let empirical (particles: 'a prob): 'a list = List.map ~f:(fun (v,lw) -> v) (resample particles)

let uniform1: uniform = (0. , 1.)
(* let x = particles_uniform uniform1
   let f = fit uniform1 x [0.1;0.5;0.2;0.2;0.3;0.4;0.2;0.5;0.2;0.12;0.44;0.22;0.32]
   let m = mean_var (Array.of_list (empirical f)) *)

let (>>=) dist fn = probBind fn dist 
let ($) dist fn = probMap fn dist

let obs = [0.8;0.3;0.93;0.56;0.23;0.90;0.78;0.23]

let param_dist = 
  (particles_uniform uniform1)
  >>= (fun a -> 
      (particles_uniform uniform1)
      >>= (fun b -> 
          (fitQ (a,b) (particles_uniform (a,b)) obs)
          $ (fun _ -> (a,b))))

let emp = empirical param_dist

let mean_var_a = (List.map ~f:(fun (aV,bV) -> aV) emp) |> Array.of_list |> mean_var
let mean_var_b = (List.map ~f:(fun (aV,bV) -> bV) emp) |> Array.of_list |> mean_var
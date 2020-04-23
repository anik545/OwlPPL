open Ppl
open Core

type point_t = float * float

type param = float * float

(* observe exactly y=2*x+5 *)
let obs =
  List.init 10 ~f:(fun x ->
      let x = float_of_int x in
      (x, (x *. 2.) +. 4.))

let linreg =
  let linear =
    let* a = normal 0. 2. in
    let* b = normal 0. 2. in
    let* c = gamma 1. 1. in
    return (a, b, c)
  in
  let open Float in
  let point d (x, y) =
    condition' (fun (a, b, c) -> Primitive.(pdf @@ normal ((a * x) + b) c) y) d
  in
  let points ps d = List.fold ~f:point ~init:d ps in
  (* let obs = List.init 10 ~f:(fun x -> let x = float_of_int x in (x,x*.2.)) in *)
  let posterior = points obs linear in
  posterior

let linreg' =
  let open Float in
  let* m = normal 0. 2. in
  let* c = normal 0. 2. in
  List.fold obs
    ~init:(return (m, c))
    ~f:(fun d (x, y) -> observe y Primitive.(normal ((m * x) + c) 1.) d)

let linreg'' =
  let linear =
    let* a = normal 0. 2. in
    let* b = normal 0. 2. in
    return (a, b)
  in
  let open Float in
  let summer l a b =
    List.sum
      ~f:(fun (x, y) -> Primitive.(pdf @@ normal ((a * x) + b) 1.) y)
      (module Float)
      l
  in
  let points d obs = condition' (fun (a, b) -> summer obs a b) d in
  (* let points ps d = List.fold ~f:point ~init:d ps in *)
  (* let obs = List.init 10 ~f:(fun x -> let x = float_of_int x in (x,x*.2.)) in *)
  let posterior = points linear obs in
  posterior

let m' = fmap fst linreg''

let c' = fmap snd linreg''

let d' = infer (fmap fst3 linreg) (MH 2000)

let d'' = infer (fmap snd3 linreg) (MH 10)

(* let d'' = mh' 10000 c' *)

let () =
  Printf.printf "y=%f*x+%f\n" (sample_mean ~n:1000 d') (sample_mean ~n:100 d'')

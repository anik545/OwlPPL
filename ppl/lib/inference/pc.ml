(* open Common *)
open Core
open Dist.GADT_Dist

(* particle cascade *)
(* https://arxiv.org/pdf/1407.2864.pdf *)
let resamplePC ps n =
  let rec iterate n mean ps iters =
    match ps with
    | [] -> return []
    | (x, w) :: ps ->
        let k = float_of_int n in
        let mean' = (k /. (k +. 1.) *. mean) +. (1. /. (k +. 1.) *. w) in
        let r = w /. mean' in
        let flr = Float.round_down r in
        let probLow = flr in
        let clr = Float.round_up r in
        let probHigh = clr in
        let spawn x w =
          if Float.(r < 1.) then choice r (return [ (x, mean') ]) (return [])
          else
            choice (r -. probLow)
              ( return
              @@ List.init (int_of_float flr) ~f:(fun _ -> (x, w /. probLow)) )
              ( return
              @@ List.init (int_of_float clr) ~f:(fun _ -> (x, w /. probHigh))
              )
        in
        let* children = spawn x w in
        if iters = 0 then return children
        else
          let* rest = iterate (n + 1) mean' ps (iters - 1) in
          return (children @ rest)
  in
  iterate 0 0. ps n

let rec cascade : 'a. int -> 'a dist -> 'a samples dist =
 fun n -> function
  | Conditional (c, d) ->
      let* ps = cascade n d in
      let qs = List.map ~f:(fun (x, w) -> (x, c x *. w)) ps in
      resamplePC qs n
  | Bind (d, f) ->
      let* ps = cascade n d in
      let xs, ws = List.unzip ps in
      let* ys = mapM f xs in
      return (List.zip_exn ys ws)
  | d -> sequence @@ List.init n ~f:(fun _ -> fmap (fun x -> (x, 1.)) d)

let cascade' n d = cascade n d >>= categorical

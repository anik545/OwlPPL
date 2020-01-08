(* open Common *)
open Core
open Dist.GADT_Dist

(* TODO: importance sampling *)
let importance n d = sequence @@ List.init n ~f:(fun _ -> prior d)
let importance' n d = (importance n d) >>= categorical

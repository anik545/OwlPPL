open Ppl
open Core


let hmm = 
  (* Observed values O(t) ~ N(X_t, 1) *)
  let values = [0.9;0.8;0.7] in
  let start = uniform [[-1];[0];[1]] in
  (* transition probabilities for hidden states [-1,0,1] *)
  let trans = function
    | -1 -> categorical @@ List.zip_exn [-1;0;1] [0.1; 0.4; 0.5]
    | 0 -> categorical @@ List.zip_exn [-1;0;1] [0.2; 0.6; 0.2]
    | 1 -> categorical @@ List.zip_exn [-1;0;1] [0.15;0.7;0.15]
    | _ -> raise Undefined
  in
  (* emmission probabilities for observed state (normal) *)
  let score y x = pdf (Normal (float_of_int x,1.)) y in
  let expand d y = condition (fun l -> score y (List.hd_exn l)) @@ 
    let* rest = d in 
    let* x = trans (List.hd_exn rest) 
    in return (x::rest)
  in

  let states = List.fold_left ~f:expand ~init:start values in
  liftM List.rev states

let posterior = (mh' 300 hmm)
let nth_of_dist n d = fmap ((fun l->List.nth_exn l n)) d
let s = sample posterior
let m = weighted_dist ~n:500 (nth_of_dist 0 posterior)
let () = Map.Poly.iteri m ~f:(fun ~key:x ~data:y -> Printf.printf "%d -> %f\n" x y);;
(* let l = List.(map ~f:(fun n -> nth_of_dist n posterior) (range 0 16))
*)

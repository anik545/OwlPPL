open Ppl
open Core

let moving_average n arr =
  let l = Array.to_list arr in
  let n = float_of_int n in
  let open Float in
  if float_of_int (List.length l) < n then failwith "List is too small"
  else
    let rec aux2 acc i = function
      | hd :: tl when i < n -> aux2 (acc + hd) (i + 1.) tl
      | _ -> acc / n
    in
    let rec aux acc l =
      match l with
      | [] -> List.rev acc
      | _ :: tl ->
          let avgn = aux2 0. 0. l in
          if float_of_int (List.length tl) < n then List.rev (avgn :: acc)
          else aux (avgn :: acc) tl
    in
    List.to_array @@ aux [] l

let one_col_mat xs = Owl.(Mat.of_array xs (Array.length xs) 1)

let one_row_mat xs = Owl.(Mat.of_array xs 1 (Array.length xs))

let x_vals =
  Array.map ~f:int_of_float
  @@ Owl.Arr.to_array (Owl.Arr.logspace ~base:10. 2. 5. 100)

let xs _ =
  Evaluation.kl_cum_discrete x_vals Sprinkler.grass_model_exact
    (infer Sprinkler.grass_model'' (MH 500))

let n = 20

let xs' = Array.init n ~f:xs

let () =
  Array.iter
    ~f:(fun arr -> Array.iter ~f:(fun (x, y) -> printf "%d,%f\n" x y) arr)
    xs'

let xs'' =
  let res = Array.create ~len:(Array.length xs'.(0)) (1, 0.) in
  for i = 0 to Array.length xs'.(0) - 1 do
    let tot = ref 0. in
    for j = 0 to n - 1 do
      tot := !tot +. snd xs'.(j).(i)
    done;
    res.(i) <- (fst xs'.(0).(i), !tot /. float_of_int n)
  done;
  res

(* take a function generating arrays all of the same length, and produce single array of mean *)
let means f =
  let xs' = Array.init n ~f in
  let res = Array.create ~len:(Array.length xs'.(0)) (1, 0.) in
  for i = 0 to Array.length xs'.(0) - 1 do
    let tot = ref 0. in
    for j = 0 to n - 1 do
      tot := !tot +. snd xs'.(j).(i)
    done;
    res.(i) <- (fst xs'.(0).(i), !tot /. float_of_int n)
  done;
  res

let () = Array.iter ~f:(fun (x, y) -> printf "%d,%f\n" x y) xs''

let () = Array.iter ~f:(fun (x, y) -> printf "%d,%f\n" x y) (xs ())

let ( @@@ ) = Fn.compose

let () =
  let open Owl_plplot in
  let h = Plot.create "b.png" in
  let x =
    one_col_mat
      ( Array.sub ~pos:0 ~len:(Array.length xs'' - (10 - 1))
      @@ Array.map ~f:(float_of_int @@@ fst) xs'' )
  in
  let y =
    one_col_mat (moving_average 10 @@ Array.map ~f:(Float.abs @@@ snd) xs'')
  in
  Plot.loglog ~h ~x y;
  Plot.output h

let p xs =
  let open Owl_plplot in
  let h = Plot.create "b.png" in
  let x = one_col_mat @@ Array.map ~f:(float_of_int @@@ fst) xs in
  let y = one_col_mat (Array.map ~f:(Float.abs @@@ fst @@@ snd) xs) in
  Plot.loglog ~h ~x y;
  Plot.output h

let () = printf "\n"

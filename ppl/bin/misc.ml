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
  @@ Owl.Arr.to_array (Owl.Arr.logspace ~base:10. 2. 4.2 100)

let xs =
  Evaluation.kl_cum_discrete x_vals Sprinkler.grass_model_exact
    (infer Sprinkler.grass_model'' (RejectionTrans (100, Soft)))

let () = Array.iter ~f:(fun (x, y) -> printf "%d,%f\n" x y) xs

let ( @@@ ) = Fn.compose

let mov len xs =
  let xs = Array.map ~f:fst xs in
  let xs' = Array.sub xs ~pos:0 ~len:(Array.length xs - (len - 1)) in
  let ys = Array.map ~f:snd xs in
  let ys' = moving_average len ys in
  (Array.zip_exn xs', ys')

let () =
  let open Owl_plplot in
  let h = Plot.create "b.png" in
  let x =
    one_col_mat
      ( Array.sub ~pos:0 ~len:(Array.length xs - (10 - 1))
      @@ Array.map ~f:(float_of_int @@@ fst) xs )
  in
  let y =
    one_col_mat (moving_average 10 @@ Array.map ~f:(Float.abs @@@ snd) xs)
  in
  Plot.loglog ~h ~x y;
  Plot.output h

let () = printf "\n"

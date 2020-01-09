open Core
open Dist.GADT_Dist

exception Undefined
module type KL_sig = sig
  type 'a samples
  type 'a primitive

  val kl_discrete: ?n:int -> 'a primitive -> 'a dist -> float
  val kl_continuous: ?n:int -> float primitive -> float dist -> float
end
module KL_Div(P: Primitive_dists.Primitives)(D: Samples.Samples): 
  KL_sig with type 'a samples = 'a D.t 
          and type 'a primitive = 'a P.primitive = 
struct

  type 'a samples = 'a D.t
  type 'a primitive = 'a P.primitive


  let kl_discrete ?(n=1000) p q = 
    (* q is samples, p is exact dist*)
    let q = D.from_dist q ~n in
    let pdf_p = P.pdf p in
    let pdf_q = D.to_pdf q in
    let support_q = D.get_vals q in

    let f x = 
      let p_x = pdf_p x in
      match pdf_q x with
      | 0. -> raise Undefined
      | q_x -> p_x *. log (p_x /. q_x)
    in
    List.sum (module Float) ~f support_q

  let kl_continuous ?(n=10000) p q = 
    (* convert to owl histogram, 
       then treat as a discrete with midpoint of each bin as the value *)

    (* type histogram =
       { bins : float array
       ; counts : int array
       ; weighted_counts : float array option
       ; normalised_counts : float array option
       ; density : float array option
       }
    *)
    let samples = Array.init n ~f:(fun _ -> sample q) in
    let open Owl_stats in
    let h = histogram (`N (n/10+10)) (samples) 
            |> normalise
            |> normalise_density
    in
    (* pp_hist Format.std_formatter h;
       printf "\n";
       Array.iter samples ~f:(fun i -> printf "%f " i);
       printf "\n";
       Array.iter h.bins ~f:(fun i -> printf "%f " i);
       printf "\n";
       Array.iter h.counts ~f:(fun i -> printf "%d " i);
       printf "\n"; *)
    (* let () = match h.normalised_counts with 
        Some s ->
        Array.iter s ~f:(fun i -> printf "%f " i);
        printf "\n";
       | None -> () in *)
    let pdf_q = match h.density with Some pdf_q -> pdf_q | None -> [||] in
    let sum = ref 0. in 
    for i = 0 to Array.length pdf_q - 1 do
      let mp = (h.bins.(i + 1) +. h.bins.(i)) /. 2. in
      let f_p = P.pdf p mp in
      let f_q = pdf_q.(i) in
      (* printf "%d %f %f %f %f %f %f \n" i h.bins.(i + 1) h.bins.(i) f_p f_q mp (f_p *. log (f_p /. f_q)); *)
      if  Float.(f_q = 0.) then () else sum := !sum +. (f_p *. log (f_p /. f_q))
    done;
    !sum /. float_of_int n

  (* let kl_continuous ?(n=1000) p q = 
     (* q is samples of dist, p is exact dist*)
     let samples = Array.init n ~f:(fun _ -> sample q) in
     let q = Array.fold samples ~init:D.empty ~f:D.add_sample in
     kl_discrete p q *)

end

module KL = KL_Div(Primitive_dists.Primitive_Dists)(Samples.DiscreteSamples)
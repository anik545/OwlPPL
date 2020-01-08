open Core


exception Undefined
module KL_Div(P: Primitive_dists.Primitives)(D: Samples.Samples) = struct

  let kl_discrete p q = 
    (* q is samples, p is exact dist*)
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

  let kl_discrete_from_dist ?(n=1000) p q = 
    (* q is samples of dist, p is exact dist*)
    let q = D.from_dist q ~n in
    kl_discrete p q

  let kl_continuous p q = 
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
    let h = Owl_stats.histogram (`N 100) (Array.of_list (D.get_vals p)) in
    h.
end

module KL = KL_Div(Primitive_dists.Primitive_Dists)(Samples.DiscreteSamples)
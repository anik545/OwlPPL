open Core

exception Undefined
module KL_Div(P: Primitive_dists.Primitives)(D: Discrete_samples.D_Samples) = struct

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

end

module KL = KL_Div(Primitive_dists.Primitive_Dists)(Discrete_samples.Samples)
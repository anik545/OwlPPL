let poisson l = 
  (module struct 
    type t = int
    let sample () = 
      let l' = ref (exp (-.l)) in
      let k = ref 0 in
      let p = ref 1. in
      while Float.(!p > !(l')) do
        k := !k + 1;
        let u = Owl_stats_dist.uniform_rvs ~a:0. ~b:1. in
        p := !p *. u
      done;
      !k - 1

    let pdf k = 
      let open Float in
      (l ** (float_of_int k)*exp(-l))/ fact k
    let cdf k = gammainc (float_of_int (k+1)) l /. fact k

  end: PRIM_DIST with type t=int)

open Ppl

(* Using kl discrete for continuous distributions will diverge with number of samples *)
(* converge to 0 *)
let diff1 = KL.kl_continuous ~n:10 Primitives.(normal 0. 1.) (normal 0. 1.)
let diff2 = KL.kl_continuous ~n:100 Primitives.(normal 0. 1.) (normal 0. 1.)
let diff3 = KL.kl_continuous ~n:1000 Primitives.(normal 0. 1.) (normal 0. 1.)
let diff4 = KL.kl_continuous ~n:10000 Primitives.(normal 0. 1.) (normal 0. 1.)
let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4

(* diverges to ?? *)
let diff1 = KL.kl_continuous ~n:10 Primitives.(continuous_uniform 0. 1.) (beta 10. 2.)
let diff2 = KL.kl_continuous ~n:100 Primitives.(continuous_uniform 0. 1.) (beta 10. 2.)
let diff3 = KL.kl_continuous ~n:1000 Primitives.(continuous_uniform 0. 1.) (beta 10. 2.)
let diff4 = KL.kl_continuous ~n:10000 Primitives.(continuous_uniform 0. 1.) (beta 10. 2.)
let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4

(* converge to 0 *)
let diff1 = KL.kl_discrete ~n:10 Primitives.(binomial 10 0.5) (binomial_ 10 0.5)
let diff2 = KL.kl_discrete ~n:100 Primitives.(binomial 10 0.5) (binomial_ 10 0.5)
let diff3 = KL.kl_discrete ~n:1000 Primitives.(binomial 10 0.5) (binomial_ 10 0.5)
let diff4 = KL.kl_discrete ~n:10000 Primitives.(binomial 10 0.5) (binomial_ 10 0.5)
let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4

(* converge to 0.5  *)
let diff1 = KL.kl_discrete ~n:10 Primitives.(discrete_uniform [0;1]) (binomial_ 1 0.9)
let diff2 = KL.kl_discrete ~n:100 Primitives.(discrete_uniform [0;1]) (binomial_ 1 0.9)
let diff3 = KL.kl_discrete ~n:1000 Primitives.(discrete_uniform [0;1]) (binomial_ 1 0.9)
let diff4 = KL.kl_discrete ~n:10000 Primitives.(discrete_uniform [0;1]) (binomial_ 1 0.9)
let () = Printf.printf "%f %f %f %f\n" diff1 diff2 diff3 diff4

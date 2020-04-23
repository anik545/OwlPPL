(* open Ppl *)

(*   open Owl_plplot

   (* normal given x>0  (half normal) *)
   let d =
   condition' (fun x -> if Stdlib.( > ) x 0. then 0. else 1.) (normal 0. 1.)

   let p = prior d

   let x = mh' 500 d

   let m = sample_mean ~n:1000 x (* ~ 0.79 (sqrt(2)/sqrt(pi)) => giving -0.79???*)

   (* standard normal *)
   let d' = condition' (fun _ -> 1.) (normal 0. 1.)

   let p' = prior d'

   let x' = mh' 500 d'

   let m' = sample_mean ~n:1000 x' (* ~ 0 *)

   (* let h = Owl_stats.(histogram (`N 10) l)
   let h' = Owl_stats.(histogram (`N 10) l') *)

   let () =
   let n = 10000 in
   let pl = Plot.create ~m:2 ~n:1 "fig.jpg" in
   Plot.subplot pl 0 0;
   let pl = Ppl.Plot.hist_dist_continuous ~h:pl ~n x in
   Plot.subplot pl 1 0;
   let pl = Ppl.Plot.hist_dist_continuous ~h:pl ~n x' in
   Plot.output pl

   let model =
   let* a = discrete_uniform [ 0; 1 ] in
   let* b = discrete_uniform [ 0; 1 ] in
   let* c = discrete_uniform [ 0; 1 ] in
   let d = return (a, b, c) in
   let d =
    condition'
      (fun (a, b, c) -> if a + b + c = 3 then 1. else -.Float.infinity)
      d
   in
   let* a, _, _ = d in
   return a

   let model1_a =
   let ( let+ ) cond dist = Conditional (cond, dist ()) in
   let ( let* ) = ( let* ) in
   let* a = discrete_uniform [ 0; 1 ] in
   let* b = discrete_uniform [ 0; 1 ] in
   let* c = discrete_uniform [ 0; 1 ] in
   (* an instance of the >> operator in haskell (sequence) in do notation - used in monad-bayes *)
   (* let c = Conditional((fun _ -> (if a+b+c = 3 then 0. else -.infinity)), return a) in
     c *)
   let+ _ = fun _ -> if a + b + c = 3 then 0. else -.infinity in
   return a

   let model1_b =
   let* a = discrete_uniform [ 0; 1 ] in
   let* b = discrete_uniform [ 0; 1 ] in
   let* c = discrete_uniform [ 0; 1 ] in
   condition (a + b + c = 3) (return a)

   let model1_b1 =
   (* LISP style *)
   bind
    (discrete_uniform [ 0; 1 ])
    (fun a ->
      bind
        (discrete_uniform [ 0; 1 ])
        (fun b ->
          bind
            (discrete_uniform [ 0; 1 ])
            (fun c -> condition (a + b + c = 3) (return a))))

   (* TODO: are both models below the same? *)
   let model_desugar_cond_end : int dist =
   Bind
    ( discrete_uniform [ 0; 1 ],
      fun a ->
        Bind
          ( discrete_uniform [ 0; 1 ],
            fun b ->
              Bind
                ( discrete_uniform [ 0; 1 ],
                  fun c ->
                    Conditional
                      ((fun _ -> if a + b + c = 3 then 1. else 0.), return a) )
          ) )
    let arr = sample_n 3 discrete_uniform [0;1]

   let model_desugar_cond_start : (int * int * int) dist =
   Conditional
    ( (fun (a, b, c) -> if a + b + c = 3 then 1. else 0.),
      Bind
        ( discrete_uniform [ 0; 1 ],
          fun a ->
            Bind
              ( discrete_uniform [ 0; 1 ],
                fun b ->
                  Bind (discrete_uniform [ 0; 1 ], fun c -> return (a, b, c)) )
        ) ) *)

(* let x =
  let* a = continuous_uniform 0. 1. in
  let* b = continuous_uniform 0. 1. in
  return a + b *)

(* ->  *)

(* let x =
  Bind
    ( continuous_uniform 0. 1.,
      fun a -> Bind (continuous_uniform 0. 1., fun b -> Return a + b) ) *)

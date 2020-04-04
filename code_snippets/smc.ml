let rec smc: 'a.int -> 'a dist -> 'a samples dist = fun n d ->
  match d with
  (* resample at each piece of evidence/data, *)
  | Conditional(c,d) ->
    let updated = fmap normalise @@
      condition' (List.sum (module Float) ~f:snd) @@
      let* last_particles = smc n d in
      let new_particles =
        List.map
          (* update particles by weight given by condition *)
          ~f:(fun (x,w) -> (x, (c x) *. w))
          last_particles in
      return new_particles
    in
    let ps* = updated in
    resample ps
  (* apply function to each particle, no resampling *)
  | Bind(d,f) ->
    let* particles = smc n d in
    mapM (fun (x,weight) ->
        let* y = f x in
        return (y, weight))
      particles
  (* initialise n particles wih weights from the pdf *)
  | Primitive d ->
    List.init n
      ~f:(fun _ -> (fmap (fun x-> (x, P.pdf d x)) (Primitive d)))
    |> sequence
  (* initialise n particles with the same value and weight *)
  | Return x ->
    List.init n ~f:(fun _ -> return (x,1.))
    |> sequence

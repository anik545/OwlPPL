open Old_dist
open Core


(* INFERENCE *)
let rec prior': 'a.'a dist -> 'a dist = function
    Conditional (_,d) -> prior' d
  | Bind (d,f) -> (prior' d) >>= f
  | d -> d

let rec prior: 'a.'a dist -> ('a*prob) dist = function
    Conditional (c,d) ->
      let* (x,s) = prior d in
      return (x, s *. (c x))
  | Bind (d,f) ->
      let* (x,s) = prior d in
      let* y = f x in
      return (y, s)
  | d ->
      let* x = d in
      return (x,1.)

type 'a samples = ('a * prob) list

let resample (xs: 'a samples): ('a samples) dist =
  let n = List.length xs in
  let old_dist = categorical xs in
  sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) (old_dist)))
 
let flatten xss =
  let mul_likelihood xs p = List.map ~f:(fun (x,q) -> (x, p *.q)) xs in
  (* let rec flat_map xss = match xss with
      (xs, p)::xs' -> (mul_likelihood xs p) @ flatten' xs'
    | [] -> []
  in *)
  List.concat_map xss ~f:(fun (xs,p) -> mul_likelihood xs p)
  (* flat_map xss *)

(* TODO: importance sampling *)

let importance n d = sequence @@ List.init n ~f:(fun _ -> prior d)
let importance' n d = (importance n d) >>= categorical
let normalise xs = 
  let norm = List.sum (module Float) ~f:snd xs in
  List.map ~f:(fun (v,p)->(v,p/.norm)) xs

(* metropolis-hastings *)

let mh n d =
  let proposal = prior d in
  let rec iterate ?(n=n) (x,s) =
    if n = 0 then return [] else
      let* (y,r) = proposal in
      let* accept = bernoulli @@ Float.min 1. (r /. s) in
      let next = if accept then (y,r) else (x,s) in
      let* rest = iterate ~n:(n-1) next in
      return (next::rest)
  in
  fmap (List.map ~f:fst) (proposal >>= iterate)

let mh' n d = fmap (fun x -> List.nth_exn x (n-1)) (mh n d)

(* sequential monte carlo *)

let rec smc: 'a.int -> 'a dist -> 'a samples dist =
  fun n ->
  function
  
  | Conditional(c,d) ->
    let updated = fmap normalise @@ 
      condition (List.sum (module Float) ~f:snd) @@
      let* ps = smc n d in
      let qs = List.map ~f:(fun (x,w) -> (x, (c x) *. w)) ps in
      return qs
    in
      updated >>= resample
  
  | Bind(d,f) ->
      let* ps = smc n d in
      let (xs, ws) = List.unzip ps in
      let* ys = mapM f xs in
      return (List.zip_exn ys ws)

  | d -> sequence @@ List.init n ~f:(fun _ -> (fmap (fun x-> (x, 1.)) d))

let smc' n d = (smc n d) >>= categorical 

let smcStandard n d = prior' (smc n d)
let smcStandard' n d = prior' (smc' n d)

(* TODO: fix importance sampling first *)
(* let smcMultiple k n d = (fmap flatten (importance k (smc n d)))
let smcMultiple' k n d = (importance' k (smc' n d)) *)

(* particle independent metropolis hastings *)
let pimh n d = mh n (smc n d)
let pimh' k n d = mh' k (smc' n d)


(* particle cascade *)
(* https://arxiv.org/pdf/1407.2864.pdf *)
(* TODO: make lazy?? *)
let resamplePC ps n = 
  let rec iterate n mean ps iters =
  match ps with
  | [] -> raise NotImplemented
  | (x,w)::ps ->
    let k = float_of_int n in
    let mean' = (k /. (k +. 1.)) *. mean +. (1. /. (k +. 1.)) *. w in
    let r = w /. mean' in
    let flr = Float.round_down r in
    let probLow = flr in
    let clr = Float.round_up r  in
    let probHigh = clr in
    let spawn x w =
      if Float.(r < 1.) then
        choice r (return [(x,mean')]) (return [])
      else
        choice (r -. probLow)
        (return @@ List.init (int_of_float flr) ~f:(fun _ -> x, w /. probLow))
        (return @@ List.init (int_of_float clr) ~f:(fun _ -> x, w /. probHigh))  
    in
    let* children = spawn x w in
    if iters = 0 then return children else
    let* rest = iterate (n+1) mean' ps (iters-1) in
    return (children @ rest)
  in
    iterate 0 0. ps n

let rec cascade:'a.int -> 'a dist -> 'a samples dist = fun n -> function
  | Conditional(c,d) -> 
      let* ps = cascade n d in
      let qs = List.map ~f:(fun (x,w) -> (x,(c x) *. w )) ps in
      resamplePC qs n
  | Bind (d,f) ->
      let* ps = cascade n d in
      let (xs,ws) = List.unzip ps in
      let* ys = mapM f xs in
      return (List.zip_exn ys ws)

  | d -> sequence @@ List.init n ~f:(fun _ -> (fmap (fun x -> (x, 1.)) d))

let cascade' n d = (cascade n d) >>= (fun x -> categorical (List.take x n))
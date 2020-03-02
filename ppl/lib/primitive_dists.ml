open Core
open Sigs

(* type 'a prim_record = {sample: unit -> 'a; pdf: 'a -> float} *)
module Primitive_Dists: Primitives = struct
  (* exception Undefined *)
  exception NotImplemented

  type 'a support = Discrete of 'a list | Continuous

  type 'a primitive = {sample: unit -> 'a; pdf: 'a -> float;cdf: 'a -> float; support: 'a support}

  let binomial n p = {
    sample = (fun () -> Owl_stats_dist.binomial_rvs ~n ~p);
    pdf = Owl_stats_dist.binomial_pdf ~n ~p;
    cdf=Owl_stats_dist.binomial_cdf ~n ~p;
    support = Continuous
  }

  let normal mu sigma = {
    sample=(fun() -> Owl_base_stats.gaussian_rvs ~mu:mu ~sigma:sigma);
    pdf= Owl_stats_dist.gaussian_pdf ~mu ~sigma;
    cdf=Owl_stats_dist.gaussian_cdf ~mu ~sigma;
    support = Continuous
  }

  let categorical xs = 
    let ps = Array.of_list_map ~f:snd xs in
    (* Array.iter ps ~f:(fun x -> Printf.printf "%f " x);
       Printf.printf "\n"; *)
    let xs_arr = Array.of_list_map ~f:fst xs in
    {sample=
       (fun()->
          xs_arr.(Owl_stats.categorical_rvs ps)
       );
     pdf=(fun x -> 
         let rec lookup l = function 
           | (a,p)::xs -> if Stdlib.(a = x) then p else lookup l xs
           | [] -> 0. (* not found *)
         in
         lookup x xs);
     cdf=(fun _ -> raise NotImplemented);
     support = Discrete (List.map ~f:fst xs)
    }

  let discrete_uniform xs = {
    sample=(fun()->(Owl_base_stats.sample (Array.of_list xs) 1).(0));
    pdf=(fun _ -> 1. /. (float_of_int (List.length xs)));
    cdf=(fun _ -> raise NotImplemented);
    support = Discrete xs
  }
  let beta a b = {
    sample=(fun()->Owl_stats_dist.beta_rvs ~a ~b);
    pdf=Owl_stats_dist.beta_pdf ~a ~b;
    cdf=Owl_stats_dist.beta_cdf ~a ~b;
    support = Continuous
  }
  let gamma shape scale = {
    sample=(fun()->Owl_stats_dist.gamma_rvs ~shape ~scale);
    pdf=Owl_stats_dist.gamma_pdf ~shape ~scale;
    cdf=Owl_stats_dist.gamma_cdf ~shape ~scale;
    support = Continuous
  }
  let continuous_uniform a b = {
    sample=(fun()->Owl_stats_dist.uniform_rvs ~a ~b);
    pdf=Owl_stats_dist.uniform_pdf ~a ~b;
    cdf=Owl_stats_dist.uniform_cdf ~a ~b;
    support = Continuous
  }

  let sample s = s.sample ()
  let pdf s x = s.pdf x
  let logpdf s x = log (s.pdf x)
  let support s = s.support
  let cdf s x = s.cdf x
end

module Primitive_Dists_GADT: Primitives = struct 
  type prob = float
  exception Undefined
  exception NotImplemented
  type _ primitive = 
      Normal: float * float -> float primitive 
    | Uniform: 'a list -> 'a primitive 
    | Categorical: ('a * prob) list -> 'a primitive
    | Beta: float * float -> float primitive
    | Binomial: int * float -> int primitive
    | Continuous_uniform: float * float -> float primitive
    | Gamma: float * float -> float primitive

  (* type a is a locally abstract type *)
  let sample: type a.a primitive -> a = function
      Normal(mu,sigma) -> Owl_base_stats.gaussian_rvs ~mu:mu ~sigma:sigma
    | Uniform xs -> (Owl_base_stats.sample (Array.of_list xs) 1).(0)
    | Categorical xs ->
      let xs = List.sort xs ~compare:(fun a b ->  if Float.(snd a -. snd b < 0.) then 1 else -1) in
      let r = Owl_base_stats.uniform_rvs ~a:0.0 ~b:1. in
      let rec loop p_left remaining = match p_left with
          (v,p)::xs -> if Float.(remaining < p) || Stdlib.(xs = []) then v else loop xs (remaining -. p)
        | [] -> raise Undefined
      in
      loop xs r
    | Beta (a, b) -> Owl_stats_dist.beta_rvs ~a ~b
    | Binomial (n, p) -> Owl_stats_dist.binomial_rvs ~n ~p
    | Continuous_uniform (a,b) -> Owl_stats_dist.uniform_rvs ~a ~b
    | Gamma(_,_) -> raise NotImplemented

  let pdf: type a.a primitive -> a -> float = function
    (* cont *)
    | Normal(mu,sigma) -> Owl_stats_dist.gaussian_pdf ~mu ~sigma
    | Beta (a, b) -> Owl_stats_dist.beta_pdf ~a ~b
    | Continuous_uniform (a, b) -> Owl_stats_dist.uniform_pdf ~a ~b

    (* discrete *)
    | Uniform us -> fun _ -> 1. /. (float_of_int (List.length us))
    | Categorical cs -> fun x -> 
      let rec lookup l = function 
        | (a,p)::xs -> if Stdlib.(a = x) then p else lookup l xs
        | [] -> 0. (* not found *)
      in
      lookup x cs
    | Binomial (n,p) -> Owl_stats_dist.binomial_pdf ~n ~p
    | Gamma(_,_) -> raise NotImplemented

  let logpdf s x = log (pdf s x)

  let binomial n p  = Binomial(n,p)
  let normal mu sigma = Normal(mu,sigma)
  let categorical xs = Categorical(xs)
  let discrete_uniform xs = Uniform(xs)
  let beta a b = Beta(a,b)
  let gamma shape scale = Gamma(shape,scale)
  let continuous_uniform a b = Continuous_uniform(a,b)

  type 'a support = Discrete of 'a list | Continuous

  let support _ = raise NotImplemented
  let cdf _ =  raise NotImplemented
end


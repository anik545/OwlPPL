# OwlPPL

A universal probabilistic programming language and accompanying dissertation written for my final year project

## Requirements

 - Ocaml 4.08
 - Opam

## Build + Install

```
git clone https://github.com/anik545/OwlPPL
opam install OwlPPL/ppl
```

## Usage

Simple coin example (in `ppl/bin/models/single_coin.ml`) - represents flipping a coin 10 times, observing 9 heads, and working out the posterior distribution over the weight of the coin.

```ocaml
open Ppl

let coin_model = 
  let* coinweight = continuous_uniform 0. 1. in
  observe 9 Primitive.(binomial 10 coinweight)
  (return coinweight)
  
let mean = sample_mean ~n:10000 (infer coin_model (MH(1000))) (*0.833*)
```

## Documentation

Build locally using `dune build @doc` in ppl

https://anik545.github.io/OwlPPL/

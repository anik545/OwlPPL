var n = 1000;
var coin_model = function (method) {
  return function () {
    var sprinkler = function () {
      var cloudy = bernoulli({ p: 0.5 });
      var rain = cloudy ? bernoulli({ p: 0.8 }) : bernoulli({ p: 0.2 });
      var sprinkler = cloudy ? bernoulli({ p: 0.1 }) : bernoulli({ p: 0.5 });
      var wet_grass = rain || sprinkler;
      condition(wet_grass);
      return rain;
    };
    var inferred = Infer(method, sprinkler);
    let samples = repeat(n, function () {
      return sample(inferred);
    });
    return samples;
  };
};

// repeat(10, function() {
//   return timeit(coin_model(10000));
// });
var get_method = function () {
  var method_arg = argv._[2];
  if (method_arg === undefined || method_arg === "mh") {
    return {
      method: "MCMC",
      kernel: "MH",
      samples: n,
      burn: 1000,
      lag: 100,
    };
  } else if (method_arg === "smc") {
    return { method: "SMC", samples: n, particles: 100 };
  } else if (method_arg === "rej") {
    return { method: "rej", samples: n };
  }
};

var method = get_method();
var t = timeit(coin_model(method));
t.runtimeInMilliseconds;

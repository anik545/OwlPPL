var n = 1000;
var coin_model = function (method) {
  return function () {
    var coin = function () {
      var theta = uniform(0.0, 1.0);
      observe(Binomial({ n: 10, p: theta }), 9);
      return theta;
    };
    var inferred = Infer(method, coin);
    let samples = repeat(n, function () {
      return sample(inferred);
    });
    return listMean(samples);
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

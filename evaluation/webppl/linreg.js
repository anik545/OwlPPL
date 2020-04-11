var linreg_model = function (method) {
  return function () {
    var xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    var ys = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18];

    var model = function () {
      var m = gaussian(0, 2);
      var c = gaussian(0, 2);
      var sigma = gamma(1, 1);

      var f = function (x) {
        return m * x + c;
      };

      map2(
        function (x, y) {
          factor(Gaussian({ mu: f(x), sigma: sigma }).score(y));
        },
        xs,
        ys
      );

      return m;
    };
    return Infer(method, model);
  };
};

var n = 1000;
var get_method = function () {
  var method_arg = argv._[2];
  if (method_arg === undefined || method_arg === "mh") {
    return {
      method: "MCMC",
      kernel: "MH",
      samples: n,
      burn: 1000,
      lag: 10,
    };
  } else if (method_arg === "smc") {
    return { method: "SMC", samples: n, particles: 100 };
  } else if (method_arg === "rej") {
    return { method: "rej", samples: n };
  }
};

var method = get_method();
var t = timeit(linreg_model(method));
t.runtimeInMilliseconds;

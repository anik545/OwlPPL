var hmm_model = function(method) {
  return function() {
    ///fold:
    var transition = function(s) {
      return s ? flip(0.7) : flip(0.3);
    };

    var observeState = function(s) {
      return s ? flip(0.9) : flip(0.1);
    };

    var hmm = function(n) {
      var prev = n == 1 ? { states: [true], observations: [] } : hmm(n - 1);
      var newState = transition(prev.states[prev.states.length - 1]);
      var newObs = observeState(newState);
      return {
        states: prev.states.concat([newState]),
        observations: prev.observations.concat([newObs])
      };
    };
    ///

    //some true observations (the data we observe):
    var trueObs = [false, false, false];

    var model = function() {
      var r = hmm(3);
      factor(_.isEqual(r.observations, trueObs) ? 0 : -Infinity);
      return r.states;
    };

    // viz.table(Infer({ model }));
    var inferred = Infer(method, model);
    return inferred;
  };
};

var n = 1000;
var get_method = function() {
  var method_arg = argv._[2];
  if (method_arg === undefined || method_arg === "mh") {
    return {
      method: "MCMC",
      kernel: "MH",
      samples: n,
      burn: 1000,
      lag: 100
    };
  } else if (method_arg === "smc") {
    return { method: "SMC", samples: n, particles: 100 };
  } else if (method_arg === "rej") {
    return { method: "rejection", samples: n };
  }
};

var method = get_method();
var t = timeit(hmm_model(method));

t.runtimeInMilliseconds;

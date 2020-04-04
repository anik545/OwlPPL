var hmm_model = function(method) {
  return function() {
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
    var trueObs = [false, false, false];
    var model = function() {
      var r = hmm(3);
      factor(_.isEqual(r.observations, trueObs) ? 0 : -Infinity);
      return r.states;
    };
    return Infer(method, model);
  };
};

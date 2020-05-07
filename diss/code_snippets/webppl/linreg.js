var linreg_model = function () {
  var xs = [0, 1, 2, 3, 4, 5, 6, 7];
  var ys = [0, 2, 4, 6, 8, 10, 12, 14];
  var model = function () {
    var m = gaussian(0, 2);
    var c = gaussian(0, 2);
    var f = function (x) {
      return m * x + c;
    };
    map2(function (x, y) {
      factor(Gaussian({
            mu: f(x),
            sigma: 1,}).score(y));
      }, xs, ys);
    return m;
  };
};

var webppl = require("/usr/lib/node_modules/webppl/src/main.js");
var args = require("/usr/lib/node_modules/webppl/src/args.js");
args.makeGlobal(__filename, process.argv.slice(2));
var webpplTimeit = require("/home/anik/.webppl/node_modules/webppl-timeit");
var __runner__ = util.trampolineRunners.cli();
function topK(s, x) {
  console.log(x);
};
var main = (function (_globalCurrentAddress) {
    return function (p) {
        return function (runTrampoline) {
            return function (s, k, a) {
                runTrampoline(function () {
                    return p(s, k, a);
                });
            };
        };
    }(function (globalStore, _k0, _address0) {
        var _currentAddress = _address0;
        _addr.save(_globalCurrentAddress, _address0);
        var Bernoulli = dists.makeBernoulli;
        var Beta = dists.makeBeta;
        var Binomial = dists.makeBinomial;
        var Categorical = dists.makeCategorical;
        var Cauchy = dists.makeCauchy;
        var Delta = dists.makeDelta;
        var DiagCovGaussian = dists.makeDiagCovGaussian;
        var Dirichlet = dists.makeDirichlet;
        var Discrete = dists.makeDiscrete;
        var Exponential = dists.makeExponential;
        var Gamma = dists.makeGamma;
        var Gaussian = dists.makeGaussian;
        var ImproperUniform = dists.makeImproperUniform;
        var IspNormal = dists.makeIspNormal;
        var KDE = dists.makeKDE;
        var Laplace = dists.makeLaplace;
        var LogNormal = dists.makeLogNormal;
        var LogisticNormal = dists.makeLogisticNormal;
        var LogitNormal = dists.makeLogitNormal;
        var Marginal = dists.makeMarginal;
        var Mixture = dists.makeMixture;
        var Multinomial = dists.makeMultinomial;
        var MultivariateBernoulli = dists.makeMultivariateBernoulli;
        var MultivariateGaussian = dists.makeMultivariateGaussian;
        var Poisson = dists.makePoisson;
        var RandomInteger = dists.makeRandomInteger;
        var SampleBasedMarginal = dists.makeSampleBasedMarginal;
        var TensorGaussian = dists.makeTensorGaussian;
        var TensorLaplace = dists.makeTensorLaplace;
        var Uniform = dists.makeUniform;
        var uniform = function uniform(globalStore, _k343, _address26, arg0, arg1) {
            var _currentAddress = _address26;
            _addr.save(_globalCurrentAddress, _address26);
            var _k345 = function (globalStore, params) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                return function () {
                    return Uniform(globalStore, function (globalStore, _result344) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        return function () {
                            return sample(globalStore, _k343, _address26.concat('_51'), _result344);
                        };
                    }, _address26.concat('_50'), params);
                };
            };
            return function () {
                return util.isObject(arg0) ? _k345(globalStore, arg0) : _k345(globalStore, {
                    a: arg0,
                    b: arg1
                });
            };
        };
        var constF = function constF(globalStore, _k289, _address47, f) {
            var _currentAddress = _address47;
            _addr.save(_globalCurrentAddress, _address47);
            return function () {
                return _k289(globalStore, function (globalStore, _k290, _address48) {
                    var _currentAddress = _address48;
                    _addr.save(_globalCurrentAddress, _address48);
                    return function () {
                        return _k290(globalStore, f);
                    };
                });
            };
        };
        var reduce = function reduce(globalStore, _k250, _address78, fn, init, ar) {
            var _currentAddress = _address78;
            _addr.save(_globalCurrentAddress, _address78);
            var n = ar.length;
            var helper = function helper(globalStore, _k251, _address79, i) {
                var _currentAddress = _address79;
                _addr.save(_globalCurrentAddress, _address79);
                return function () {
                    return ad.scalar.peq(i, n) ? _k251(globalStore, init) : helper(globalStore, function (globalStore, _result252) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        return function () {
                            return fn(globalStore, _k251, _address79.concat('_88'), ar[i], _result252);
                        };
                    }, _address79.concat('_87'), ad.scalar.add(i, 1));
                };
            };
            return function () {
                return helper(globalStore, _k250, _address78.concat('_89'), 0);
            };
        };
        var sum = function sum(globalStore, _k248, _address80, l) {
            var _currentAddress = _address80;
            _addr.save(_globalCurrentAddress, _address80);
            return function () {
                return reduce(globalStore, _k248, _address80.concat('_90'), function (globalStore, _k249, _address81, a, b) {
                    var _currentAddress = _address81;
                    _addr.save(_globalCurrentAddress, _address81);
                    return function () {
                        return _k249(globalStore, ad.scalar.add(a, b));
                    };
                }, 0, l);
            };
        };
        var listMean = function listMean(globalStore, _k244, _address84, l) {
            var _currentAddress = _address84;
            _addr.save(_globalCurrentAddress, _address84);
            return function () {
                return sum(globalStore, function (globalStore, _result245) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    return function () {
                        return _k244(globalStore, ad.scalar.div(_result245, l.length));
                    };
                }, _address84.concat('_92'), l);
            };
        };
        var repeat = function repeat(globalStore, _k192, _address110, n, fn) {
            var _currentAddress = _address110;
            _addr.save(_globalCurrentAddress, _address110);
            var helper = function helper(globalStore, _k199, _address111, m) {
                var _currentAddress = _address111;
                _addr.save(_globalCurrentAddress, _address111);
                return function () {
                    return ad.scalar.peq(m, 0) ? _k199(globalStore, []) : ad.scalar.peq(m, 1) ? fn(globalStore, function (globalStore, _result200) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        return function () {
                            return _k199(globalStore, [_result200]);
                        };
                    }, _address111.concat('_131')) : function (globalStore, m1) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        var m2 = ad.scalar.sub(m, m1);
                        return function () {
                            return helper(globalStore, function (globalStore, _result201) {
                                _addr.save(_globalCurrentAddress, _currentAddress);
                                return function () {
                                    return helper(globalStore, function (globalStore, _result202) {
                                        _addr.save(_globalCurrentAddress, _currentAddress);
                                        return function () {
                                            return _k199(globalStore, _result201.concat(_result202));
                                        };
                                    }, _address111.concat('_133'), m2);
                                };
                            }, _address111.concat('_132'), m1);
                        };
                    }(globalStore, ad.scalar.ceil(ad.scalar.div(m, 2)));
                };
            };
            var _k196 = function (globalStore, _dummy195) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                var _k194 = function (globalStore, _dummy193) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    return function () {
                        return helper(globalStore, _k192, _address110.concat('_136'), n);
                    };
                };
                return function () {
                    return _.isFunction(fn) ? _k194(globalStore, undefined) : error(globalStore, _k194, _address110.concat('_135'), 'Expected second argument to be a function.');
                };
            };
            var _k198 = function (globalStore, _result197) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                return function () {
                    return _result197 ? error(globalStore, _k196, _address110.concat('_134'), 'Expected first argument to be a non-negative integer.') : _k196(globalStore, undefined);
                };
            };
            return function () {
                return util.isInteger(n) ? _k198(globalStore, ad.scalar.lt(n, 0)) : _k198(globalStore, !util.isInteger(n));
            };
        };
        var error = function error(globalStore, _k162, _address122, msg) {
            var _currentAddress = _address122;
            _addr.save(_globalCurrentAddress, _address122);
            return function () {
                return _k162(globalStore, util.error(msg));
            };
        };
        var SampleGuide = function SampleGuide(globalStore, _k158, _address126, wpplFn, options) {
            var _currentAddress = _address126;
            _addr.save(_globalCurrentAddress, _address126);
            return function () {
                return ForwardSample(globalStore, _k158, _address126.concat('_156'), wpplFn, _.assign({ guide: !0 }, _.omit(options, 'guide')));
            };
        };
        var OptimizeThenSample = function OptimizeThenSample(globalStore, _k156, _address127, wpplFn, options) {
            var _currentAddress = _address127;
            _addr.save(_globalCurrentAddress, _address127);
            return function () {
                return Optimize(globalStore, function (globalStore, _dummy157) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    var opts = _.pick(options, 'samples', 'onlyMAP', 'callbacks', 'verbose');
                    return function () {
                        return SampleGuide(globalStore, _k156, _address127.concat('_158'), wpplFn, opts);
                    };
                }, _address127.concat('_157'), wpplFn, _.omit(options, 'samples', 'onlyMAP', 'callbacks'));
            };
        };
        var AISforInfer = function AISforInfer(globalStore, _k152, _address128, wpplFn, options) {
            var _currentAddress = _address128;
            _addr.save(_globalCurrentAddress, _address128);
            return function () {
                return constF(globalStore, function (globalStore, _result155) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    return function () {
                        return Infer(globalStore, function (globalStore, dummyMarginal) {
                            _addr.save(_globalCurrentAddress, _currentAddress);
                            return function () {
                                return AIS(globalStore, function (globalStore, _result154) {
                                    _addr.save(_globalCurrentAddress, _currentAddress);
                                    var _dummy153 = _.assign(dummyMarginal, { normalizationConstant: _result154 });
                                    return function () {
                                        return _k152(globalStore, dummyMarginal);
                                    };
                                }, _address128.concat('_161'), wpplFn, options);
                            };
                        }, _address128.concat('_160'), _result155);
                    };
                }, _address128.concat('_159'), !0);
            };
        };
        var DefaultInfer = function DefaultInfer(globalStore, _k142, _address129, wpplFn, options) {
            var _currentAddress = _address129;
            _addr.save(_globalCurrentAddress, _address129);
            var _dummy151 = util.mergeDefaults(options, {}, 'Infer');
            var maxEnumTreeSize = 200000;
            var minSampleRate = 250;
            var samples = 1000;
            return function () {
                return Enumerate(globalStore, function (globalStore, enumResult) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    var _k150 = function (globalStore, _dummy149) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        var _dummy148 = console.log('Using "rejection"');
                        return function () {
                            return Rejection(globalStore, function (globalStore, rejResult) {
                                _addr.save(_globalCurrentAddress, _currentAddress);
                                return function () {
                                    return rejResult instanceof Error ? function (globalStore, _dummy147) {
                                        _addr.save(_globalCurrentAddress, _currentAddress);
                                        return function () {
                                            return CheckSampleAfterFactor(globalStore, function (globalStore, hasSampleAfterFactor) {
                                                _addr.save(_globalCurrentAddress, _currentAddress);
                                                var _k145 = function (globalStore, _dummy144) {
                                                    _addr.save(_globalCurrentAddress, _currentAddress);
                                                    var _dummy143 = console.log('Using "MCMC"');
                                                    return function () {
                                                        return MCMC(globalStore, _k142, _address129.concat('_168'), wpplFn, { samples: samples });
                                                    };
                                                };
                                                return function () {
                                                    return hasSampleAfterFactor ? function (globalStore, _dummy146) {
                                                        _addr.save(_globalCurrentAddress, _currentAddress);
                                                        return function () {
                                                            return SMC(globalStore, function (globalStore, smcResult) {
                                                                _addr.save(_globalCurrentAddress, _currentAddress);
                                                                return function () {
                                                                    return dists.isDist(smcResult) ? _k142(globalStore, smcResult) : smcResult instanceof Error ? _k145(globalStore, console.log(ad.scalar.add(smcResult.message, '..quit SMC'))) : error(globalStore, _k145, _address129.concat('_167'), 'Invalid return value from SMC');
                                                                };
                                                            }, _address129.concat('_166'), wpplFn, {
                                                                throwOnError: !1,
                                                                particles: samples
                                                            });
                                                        };
                                                    }(globalStore, console.log('Using "SMC" (interleaving samples and factors detected)')) : _k145(globalStore, undefined);
                                                };
                                            }, _address129.concat('_165'), wpplFn);
                                        };
                                    }(globalStore, console.log(ad.scalar.add(rejResult.message, '..quit rejection'))) : dists.isDist(rejResult) ? _k142(globalStore, rejResult) : error(globalStore, _k142, _address129.concat('_169'), 'Invalid return value from rejection');
                                };
                            }, _address129.concat('_164'), wpplFn, {
                                minSampleRate: minSampleRate,
                                throwOnError: !1,
                                samples: samples
                            });
                        };
                    };
                    return function () {
                        return dists.isDist(enumResult) ? _k142(globalStore, enumResult) : enumResult instanceof Error ? _k150(globalStore, console.log(ad.scalar.add(enumResult.message, '..quit enumerate'))) : error(globalStore, _k150, _address129.concat('_163'), 'Invalid return value from enumerate');
                    };
                }, _address129.concat('_162'), wpplFn, {
                    maxEnumTreeSize: maxEnumTreeSize,
                    maxRuntimeInMS: 5000,
                    throwOnError: !1,
                    strategy: 'depthFirst'
                });
            };
        };
        var Infer = function Infer(globalStore, _k135, _address130, options, maybeFn) {
            var _currentAddress = _address130;
            _addr.save(_globalCurrentAddress, _address130);
            var _k141 = function (globalStore, wpplFn) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                var _k140 = function (globalStore, _dummy139) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    var methodMap = {
                        SMC: SMC,
                        MCMC: MCMC,
                        PMCMC: PMCMC,
                        asyncPF: AsyncPF,
                        rejection: Rejection,
                        enumerate: Enumerate,
                        incrementalMH: IncrementalMH,
                        forward: ForwardSample,
                        optimize: OptimizeThenSample,
                        AIS: AISforInfer,
                        defaultInfer: DefaultInfer
                    };
                    var _k138 = function (globalStore, methodName) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        var _k137 = function (globalStore, _dummy136) {
                            _addr.save(_globalCurrentAddress, _currentAddress);
                            var method = methodMap[methodName];
                            return function () {
                                return method(globalStore, _k135, _address130.concat('_172'), wpplFn, _.omit(options, 'method', 'model'));
                            };
                        };
                        return function () {
                            return _.has(methodMap, methodName) ? _k137(globalStore, undefined) : function (globalStore, methodNames) {
                                _addr.save(_globalCurrentAddress, _currentAddress);
                                var msg = ad.scalar.add(ad.scalar.add(ad.scalar.add(ad.scalar.add('Infer: \'', methodName), '\' is not a valid method. The following methods are available: '), methodNames.join(', ')), '.');
                                return function () {
                                    return error(globalStore, _k137, _address130.concat('_171'), msg);
                                };
                            }(globalStore, _.keys(methodMap));
                        };
                    };
                    return function () {
                        return options.method ? _k138(globalStore, options.method) : _k138(globalStore, 'defaultInfer');
                    };
                };
                return function () {
                    return _.isFunction(wpplFn) ? _k140(globalStore, undefined) : error(globalStore, _k140, _address130.concat('_170'), 'Infer: a model was not specified.');
                };
            };
            return function () {
                return util.isObject(options) ? maybeFn ? _k141(globalStore, maybeFn) : _k141(globalStore, options.model) : _k141(globalStore, options);
            };
        };
        var timeit = function timeit(globalStore, _k12, _address165, thunk) {
            var _currentAddress = _address165;
            _addr.save(_globalCurrentAddress, _address165);
            var t0 = webpplTimeit.now();
            return function () {
                return thunk(globalStore, function (globalStore, value) {
                    _addr.save(_globalCurrentAddress, _currentAddress);
                    var t1 = webpplTimeit.now();
                    return function () {
                        return _k12(globalStore, {
                            value: value,
                            runtimeInMilliseconds: ad.scalar.sub(t1, t0)
                        });
                    };
                }, _address165.concat('_257'));
            };
        };
        var n = 1000;
        var coin_model = function coin_model(globalStore, _k5, _address167, method) {
            var _currentAddress = _address167;
            _addr.save(_globalCurrentAddress, _address167);
            return function () {
                return _k5(globalStore, function (globalStore, _k6, _address168) {
                    var _currentAddress = _address168;
                    _addr.save(_globalCurrentAddress, _address168);
                    var coin = function coin(globalStore, _k8, _address169) {
                        var _currentAddress = _address169;
                        _addr.save(_globalCurrentAddress, _address169);
                        return function () {
                            return uniform(globalStore, function (globalStore, theta) {
                                _addr.save(_globalCurrentAddress, _currentAddress);
                                return function () {
                                    return Binomial(globalStore, function (globalStore, _result10) {
                                        _addr.save(_globalCurrentAddress, _currentAddress);
                                        return function () {
                                            return observe(globalStore, function (globalStore, _dummy9) {
                                                _addr.save(_globalCurrentAddress, _currentAddress);
                                                return function () {
                                                    return _k8(globalStore, theta);
                                                };
                                            }, _address169.concat('_260'), _result10, 9);
                                        };
                                    }, _address169.concat('_259'), {
                                        n: 10,
                                        p: theta
                                    });
                                };
                            }, _address169.concat('_258'), 0, 1);
                        };
                    };
                    return function () {
                        return Infer(globalStore, function (globalStore, inferred) {
                            _addr.save(_globalCurrentAddress, _currentAddress);
                            return function () {
                                return repeat(globalStore, function (globalStore, samples) {
                                    _addr.save(_globalCurrentAddress, _currentAddress);
                                    return function () {
                                        return listMean(globalStore, _k6, _address168.concat('_264'), samples);
                                    };
                                }, _address168.concat('_263'), n, function (globalStore, _k7, _address170) {
                                    var _currentAddress = _address170;
                                    _addr.save(_globalCurrentAddress, _address170);
                                    return function () {
                                        return sample(globalStore, _k7, _address170.concat('_262'), inferred);
                                    };
                                });
                            };
                        }, _address168.concat('_261'), method, coin);
                    };
                });
            };
        };
        var get_method = function get_method(globalStore, _k2, _address171) {
            var _currentAddress = _address171;
            _addr.save(_globalCurrentAddress, _address171);
            var method_arg = argv._[2];
            var _k4 = function (globalStore, _result3) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                return function () {
                    return _result3 ? _k2(globalStore, {
                        method: 'MCMC',
                        kernel: 'MH',
                        samples: n,
                        burn: 1000,
                        lag: 100
                    }) : ad.scalar.peq(method_arg, 'smc') ? _k2(globalStore, {
                        method: 'SMC',
                        samples: n,
                        particles: 100
                    }) : ad.scalar.peq(method_arg, 'rej') ? _k2(globalStore, {
                        method: 'rej',
                        samples: n
                    }) : _k2(globalStore, undefined);
                };
            };
            return function () {
                return ad.scalar.peq(method_arg, undefined) ? _k4(globalStore, ad.scalar.peq(method_arg, undefined)) : _k4(globalStore, ad.scalar.peq(method_arg, 'mh'));
            };
        };
        return function () {
            return get_method(globalStore, function (globalStore, method) {
                _addr.save(_globalCurrentAddress, _currentAddress);
                return function () {
                    return coin_model(globalStore, function (globalStore, _result1) {
                        _addr.save(_globalCurrentAddress, _currentAddress);
                        return function () {
                            return timeit(globalStore, function (globalStore, t) {
                                _addr.save(_globalCurrentAddress, _currentAddress);
                                return function () {
                                    return _k0(globalStore, t.runtimeInMilliseconds);
                                };
                            }, _address0.concat('_267'), _result1);
                        };
                    }, _address0.concat('_266'), method);
                };
            }, _address0.concat('_265'));
        };
    });
});

webppl.runEvaled(main, __runner__, {}, {}, topK, '');
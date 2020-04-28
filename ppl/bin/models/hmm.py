from hmmlearn import hmm
import numpy as np

model = hmm.GaussianHMM(n_components=3, covariance_type="spherical")
obs = [0.9,    0.8,    0.7,    0.,    -0.025,    5.,    2.,    0.1,
       0.,    0.13,    0.45,    6.,    0.2,    0.3,    -1.,    -1., ]
# states are -1,0,1 in that order
model.startprob_ = np.array([0.33, 0.33, 0.34])
model.transmat_ = np.array([[0.1, 0.4, 0.5],
                            [0.2, 0.6, 0.2],
                            [0.15, 0.7, 0.15]])
model.means_ = np.array([[1.], [0.], [-1.]])
model.covars_ = np.array([1., 1., 1.])
p = model.predict_proba(np.array([obs]).transpose())
print(p)
print()
model1 = hmm.MultinomialHMM(n_components=2)
model1.n_features = 2
obs = [True, True, True, False]
obs = list(map(lambda x: 1 if x else 0, obs))
# states are true=1 and false=0
# false, true
model1.startprob_ = np.array([0, 1.])
model1.transmat_ = np.array([[0.3, 0.7],
                             [0.7, 0.3]])
model1.emissionprob_ = np.array([[0.9, 0.1],
                                 [0.1, 0.9]])
p = model1.predict_proba(np.array([obs]).transpose())
print(p)

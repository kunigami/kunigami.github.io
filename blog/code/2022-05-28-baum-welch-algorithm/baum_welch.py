from dataclasses import dataclass
import numpy as np
from numpy.typing import ArrayLike

@dataclass
class HMM:
    hidden_sts: ArrayLike
    visible_sts: ArrayLike
    trans_prob: ArrayLike
    obs_prob: ArrayLike
    ini_prob: ArrayLike

    obs: ArrayLike

    @property
    def N(self):
      return len(self.hidden_sts)

    @property
    def T(self):
      return len(self.obs)

    def states(self):
      return range(self.N)

    def observations(self):
      return range(len(self.visible_sts))

    def __str__(self):
      return f"""Transition Probability:
{self.trans_prob}

Observation Probability:
{self.obs_prob}

Initial Probability:
{self.ini_prob}
"""


class BaumWelch:
  # forward step
  def calc_alpha(self, model):
    alpha = np.empty(shape=(model.N, model.T))
    for i in model.states():
      alpha[i, 0] = model.ini_prob[i] * model.obs_prob[i, model.obs[0]]

    for t in range(1, model.T):
      for i in model.states():
        alpha[i, t] = sum(alpha[:, t - 1] * model.trans_prob[:, i]) * \
          model.obs_prob[i, model.obs[t]]
    return alpha

  # backward step
  def calc_beta(self, model):
    beta = np.empty(shape=(model.N, model.T))
    for i in model.states():
      beta[i, model.T-1] = 1

    for t in range(model.T-2, -1, -1): # T-2 to 0
      for i in model.states():
        beta[i, t] = sum(
          model.trans_prob[i, :] *
          beta[:, t + 1] *
          model.obs_prob[:, model.obs[t + 1]]
        )
    return beta

  # un-normalized
  def calc_gamma(self, model, alpha, beta):
    gamma = np.empty(shape=alpha.shape)
    for i in model.states():
      gamma[i, ] = alpha[i, ] * beta[i, ]
    return gamma

  # un-normalized
  def calc_xi(self, model, alpha, beta):
    xi = np.empty(shape=(model.N, model.N, model.T))
    for t in range(model.T - 1):
      for i in model.states():
        for j in model.states():
          a = model.trans_prob[i, j]
          b = model.obs_prob[j, model.obs[t + 1]]
          xi[i, j, t] = alpha[i, t] * a * b * beta[j, t+1]
    return xi

  def calc_trans_prob(self, model, gamma, xi):
    trans_prob = np.empty(shape=model.trans_prob.shape)
    for i in model.states():
      den = sum(gamma[i, 0:model.T-1])
      for j in model.states():
        trans_prob[i, j] = sum(xi[i, j, 0:model.T-1]) / den
    return trans_prob

  def calc_obs_prob(self, model, gamma, xi):
    obs_prob = np.empty(shape=model.obs_prob.shape)
    for i in model.states():
      for k in model.observations():
        num = sum(
          gamma[i, t] for t in range(model.T) if model.obs[t] == k
        )
        obs_prob[i, k] = num / sum(gamma[i, :])
    return obs_prob

  def calc_ini_prob(self, gamma):
    return gamma[:, 0]

  def get_likelihood(self, model):
    alpha = self.calc_alpha(model)
    return sum(alpha[:, model.T-1])

  def iterate(self, model):
    alpha = self.calc_alpha(model)
    beta = self.calc_beta(model)

    # likelihood is used as denominator for computing all gamma and xi, so we can
    # just compute it once. the likelihood is also our metric to be maximized.
    likelihood = self.get_likelihood(model)

    gamma = self.calc_gamma(model, alpha, beta) / likelihood
    xi = self.calc_xi(model, alpha, beta) / likelihood

    # update model
    return HMM(
      hidden_sts=model.hidden_sts,
      visible_sts=model.visible_sts,
      obs=model.obs,
      trans_prob=self.calc_trans_prob(model, gamma, xi),
      obs_prob=self.calc_obs_prob(model, gamma, xi),
      ini_prob=self.calc_ini_prob(gamma),
    )

  def run(self, model):
    current_likelihood = self.get_likelihood(model)
    while True:
      new_model = self.iterate(model)

      new_likelihood = self.get_likelihood(new_model)
      if new_likelihood <= current_likelihood + 1e-6:
        break
      model = new_model

      current_likelihood = new_likelihood

      print('likelihood', current_likelihood)

    print(model)



model = HMM(
    hidden_sts=["state1", "state2"],
    visible_sts=["no eggs", "eggs"],
    trans_prob=np.array([
      [0.5, 0.5],
      [0.3, 0.7],
    ]),
    obs_prob=np.array([
      [0.3, 0.7],
      [0.8, 0.2],
    ]),
    ini_prob=[0.2, 0.8],
    obs=[0, 0, 0, 0, 0, 1, 1, 0, 0, 0]
)

model = HMM(
    hidden_sts=["state1", "state2"],
    visible_sts=["no eggs", "eggs"],
    trans_prob=np.array([
      [0.5, 0.5],
      [0.3, 0.7],
    ]),
    obs_prob=np.array([
      [0.3, 0.7],
      [0.8, 0.2],
    ]),
    ini_prob=[0.2, 0.8],
    # obs=[0, 0, 0, 0, 0, 1, 1, 0, 0, 0]
    obs=[0, 0, 0, 1, 0, 0]
)

# model = HMM(
#     hidden_sts=["state1", "state2"],
#     visible_sts=["yes", "no"],
#     trans_prob=np.array([
#       [0.4, 0.6],
#       [0.6, 0.4],
#     ]),
#     obs_prob=np.array([
#       [0.6, 0.4],
#       [0.4, 0.6],
#     ]),
#     ini_prob=[1.0, 0],
#     obs=[0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
# )


BaumWelch().run(model)

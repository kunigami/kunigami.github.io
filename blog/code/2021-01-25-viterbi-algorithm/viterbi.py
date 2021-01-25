class HMM:
    def __init__(
        self,
        hidden_sts,
        visible_sts,
        trans_prob,
        obs_prob,
        ini_prob,
    ):
        self.hidden_sts = hidden_sts
        self.visible_sts = visible_sts
        self.trans_prob = trans_prob
        self.obs_prob = obs_prob
        self.ini_prob = ini_prob


def viterbi(model, obs):
    n = len(model.hidden_sts)
    m = len(model.visible_sts)
    t = len(obs)

    memo_prob = [0] * n
    memo_sol = [[None] * n for x in range(t)]

    # Handle base case
    for j in range(n):
        obs_prob = model.obs_prob[j][obs[0]]
        memo_prob[j] = model.ini_prob[j] * obs_prob

    for i in range(1, t):

        new_memo_prob = [0] * n
        for j in range(n):

            max_prob = 0
            best_k = None
            for k in range(n):
                prob = memo_prob[k] * model.trans_prob[k][j]
                if prob > max_prob:
                    max_prob = prob
                    best_k = k

            obs_prob = model.obs_prob[j][obs[i]]
            new_memo_prob[j] = max_prob * obs_prob
            memo_sol[i][j] = best_k

        memo_prob = new_memo_prob

    # Recover solution
    curr_st = 0
    for j in range(1, n):
        if memo_prob[curr_st] < memo_prob[j]:
            curr_st = j

    sol = [curr_st]
    for i in range(t - 1, 0, -1):
        curr_st = memo_sol[i][curr_st]
        sol.append(curr_st)

    # Need to reverse since we backtracked
    sol = sol[::-1]
    return [model.hidden_sts[x] for x in sol]


model = HMM(
    hidden_sts=["healthy", "fever"],
    visible_sts=["normal", "cold", "dizzy"],
    trans_prob=[[0.7, 0.3], [0.4, 0.6]],
    obs_prob=[[0.5, 0.4, 0.1], [0.1, 0.3, 0.6]],
    ini_prob=[0.6, 0.4],
)

obs = [0, 1, 2]

print(viterbi(model, obs))

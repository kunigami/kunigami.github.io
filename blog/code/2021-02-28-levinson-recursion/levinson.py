import numpy as np
import random

def levinson(mat, y):

    n = len(mat)

    t_0 = mat[0][0]
    # forward vector f^n
    f = np.array([1.0/t_0])
    # backward vector b^n
    b = np.array([1.0/t_0])
    # partial solution x^n
    x = np.array([y[0]/t_0])

    for i in range(1, n):
        last_row = mat[i, 0 : i + 1]
        f2 = np.append(f, 0)
        eps_f = np.dot(last_row, f2)

        first_row = mat[0, 0 : i + 1]
        b2 = np.insert(b, 0, 0)
        eps_b = np.dot(first_row, b2)

        # Common denominator to all alphas and betas
        denom = 1. - eps_f * eps_b

        alpha_f = 1. / denom
        beta_f = -eps_f / denom
        f = alpha_f * f2 + beta_f * b2

        alpha_b = -eps_b / denom
        beta_b = 1 / denom
        b = alpha_b * f2 + beta_b * b2

        # Compute x^n from b^n
        x2 = np.append(x, 0)

        eps_x = np.dot(last_row, x2)

        x = x2 + (y[i] - eps_x) * b

    return x

def verify_solution(mat, x, y):
    actual_y = np.dot(mat, x)
    print(actual_y, y)

    for i in range(len(y)):
        assert abs(actual_y[i] - y[i]) < 1e-6


def make_random_toeplitz(n):
    mat = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            if i == 0 or j == 0:
                mat[i][j] = random.randint(0, 100)
            else:
                mat[i][j] = mat[i - 1][j - 1]
    return mat

def make_random_vector(n):
    vec = np.zeros(n)
    for i in range(n):
        vec[i] = random.randint(0, 100)
    return vec

n = 100
mat = make_random_toeplitz(n)
y = make_random_vector(n)

x = levinson(mat, y)
verify_solution(mat, x, y)

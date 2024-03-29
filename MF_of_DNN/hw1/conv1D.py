import numpy as np


class Convolution1d:
    def __init__(self, filt):
        self.__filt = filt
        self.__r = filt.size
        self.T = TransposedConvolution1d(self.__filt)

    def __matmul__(self, vector):
        r, n = self.__r, vector.size

        return np.asarray(
            [np.dot(self.__filt, vector[i : i + r]) for i in range(n - r + 1)]
        )  # IMPLEMENT THIS


class TransposedConvolution1d:
    """
    Transpose of 1-dimensional convolution operator used for the
    transpose-convolution operation A.T@(...)
    """

    def __init__(self, filt):
        self.__filt = filt
        self.__r = filt.size

    def __matmul__(self, vector):
        r = self.__r
        n = vector.size + r - 1
        vectorf = np.zeros(n)
        vectorsum = np.zeros(n)
        for i in range(n - r + 1):
            for j in range(r):
                vectorf[i + j] += self.__filt[j] * vector[i]
        # return vectorf

        return np.sum(
            [
                np.concatenate((np.zeros(i), self.__filt, np.zeros(n - r - i))) * x
                for i, x in enumerate(vector)
            ],
            axis=0,
        )  # IMPLEMENT THIS


def huber_loss(x):
    return np.sum(
        (1 / 2) * (x ** 2) * (np.abs(x) <= 1) + (np.sign(x) * x - 1 / 2) * (np.abs(x) > 1)
    )


def huber_grad(x):
    return x * (np.abs(x) <= 1) + np.sign(x) * (np.abs(x) > 1)


r, n, lam = 3, 20, 0.1

np.random.seed(0)
k = np.random.randn(r)
b = np.random.randn(n - r + 1)
A = Convolution1d(k)
# from scipy.linalg import circulant
# A = circulant(np.concatenate((np.flip(k),np.zeros(n-r))))[2:,:]


x = np.zeros(n)
alpha = 0.01
for _ in range(100):
    x = x - alpha * (A.T @ (huber_grad(A @ x - b)) + lam * x)

print(huber_loss(A @ x - b) + 0.5 * lam * np.linalg.norm(x) ** 2)

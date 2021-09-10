import numpy as np
import matplotlib.pyplot as plt


np.seterr(invalid="ignore", over="ignore")  # suppress warning caused by division by inf


def f(x):
    return 1 / (1 + np.exp(3 * (x - 3))) * 10 * x ** 2 + 1 / (1 + np.exp(-3 * (x - 3))) * (
        0.5 * (x - 10) ** 2 + 50
    )


def fprime(x):
    return (
        1 / (1 + np.exp((-3) * (x - 3))) * (x - 10)
        + 1 / (1 + np.exp(3 * (x - 3))) * 20 * x
        + (3 * np.exp(9))
        / (np.exp(9 - 1.5 * x) + np.exp(1.5 * x)) ** 2
        * ((0.5 * (x - 10) ** 2 + 50) - 10 * x ** 2)
    )


x = np.linspace(-5, 20, 100)
plt.plot(x, f(x), "k")
plt.show()


def gradient_descent(f, df, x0, alpha, eps=1e-6):
    # x: starting point, alpha: learning rate
    x1 = x0 - alpha * df(x0)
    while abs(x0 - x1) >= eps or abs(f(x0) - f(x1)) >= eps:
        x0 = x1
        x1 = x0 - alpha * df(x0)
    if np.isnan(x1):
        return "diverge"
    else:
        return x1


for alpha in [0.01, 0.3, 4]:
    y = []
    for x0 in np.linspace(-5, 20, 100):
        y.append(gradient_descent(f, fprime, x0, alpha))
    plt.title("alpha = %0.2f\n# of diverge = %d" % (alpha, y.count("diverge")))
    plt.hist(y, range=(-1, 11))
    plt.show()

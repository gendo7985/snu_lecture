#############################
#                           #
#     계산과학이론 및 실습1    #
#            HW 5           #
#      2017-11362 박건도     #
#                           #
#############################


# problem_A

from math import pi, exp, log


def main():
    x = float(input("x: ").strip())
    coef = 2 / pi ** 0.5
    if x >= 0:
        print(coef * adp_simp(0, x))
    else:
        print(-coef * adp_simp(x, 0))


def adp_simp(a, b, tol=1e-7):
    assert a <= b, "Range [a, b] should be a <= b."
    if a == b:
        return 0

    f = lambda x: exp(-(x ** 2))
    simp = lambda a, m, b: (b - a) * (f(a) + 4 * f(m) + f(b)) / 6

    m = (a + b) / 2
    S = [(a, m, b)]
    area = [simp(a, m, b)]

    while S:
        interval = S.pop()
        x, y, z = interval
        whole = area.pop()
        left = simp(x, (x + y) / 2, y)
        right = simp(y, (y + z) / 2, z)
        delta = left + right - whole
        if abs(delta) >= 15 * tol:
            S += [(x, (x + y) / 2, y), (y, (y + z) / 2, z)]
            area += [right, left]
        else:
            area = [left + right + delta / 15] + area

    return sum(area)


# problem_B


def integral(coefs, interval, n):
    assert n > 0, "n should be positive integer."
    a = len(coefs)
    if coefs[0] != 0:
        f = lambda x: sum([coefs[a - i - 1] * x ** i for i in range(a - 1, -1, -1)])
        F = lambda x: sum([coefs[a - i] * x ** (i) / i for i in range(a, 0, -1)])
    else:
        assert interval[0] * interval[1] > 0, "0 should not be contained at the interval."
        f = lambda x: sum([coefs[i] / x ** i for i in range(1, a)])
        F = lambda x: coefs[1] * log(x) + sum([-coefs[i + 1] / x ** i / i for i in range(1, a - 1)])

    real = F(interval[1]) - F(interval[0])
    err = lambda e: abs(e - real) / real * 100

    h = (interval[1] - interval[0]) / n

    points = [interval[0] + h * (2 * i + 1) / 2 for i in range(n)]
    midpoint = sum([f(x) for x in points]) * h

    points = [interval[0] + h * i for i in range(1, n)]
    trapezoid = (sum([f(x) for x in points]) + (f(interval[0]) + f(interval[1])) / 2) * h

    print("%10s | %10s | %5s(%%)" % ("method", "integral", "error"))
    print("─" * 34)
    print("%10s | %10.5f | %5.2f" % ("midpoint", midpoint, err(midpoint)))
    print("%10s | %10.5f | %5.2f" % ("trapezoid", trapezoid, err(trapezoid)))

    return real


if __name__ == "__main__":
    # main()
    integral([0, 2, 3], (1, 2), 2)


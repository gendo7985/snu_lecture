#############################
#                           #
#     계산과학이론 및 실습1    #
#            HW 5           #
#      2017-11362 박건도     #
#                           #
#############################


# problem_A

from math import pi, exp


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


if __name__ == "__main__":
    main()

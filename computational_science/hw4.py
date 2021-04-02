#############################
#                           #
#     계산과학이론 및 실습1    #
#            HW 2           #
#      2017-11362 박건도     #
#                           #
#############################


def quadratic(a, b, c, x0, x1, tol):
    f = lambda x: a * x ** 2 + b * x + c
    assert a != 0, "'a' must be nonzero in quadratic form."
    assert f(x0) != f(x1), "Invalid initial values for secant method."

    R = -b / (2 * a)
    if b ** 2 == 4 * a * c:
        print("multiple root:", R)
    else:
        I = (b ** 2 - 4 * a * c) ** 0.5 / (2 * a)
        print("two roots: ", R + I, ", ", R - I, sep="")

    if b ** 2 < 4 * a * c:
        return "imaginary roots"

    print("------------------------------")
    print("        Secant method")
    print("------------------------------")
    print("iter |     xi     |    diff")
    print("------------------------------")

    i = 1
    print("%3d  | %10f |      -" % (0, x0))
    if abs(f(x0)) < tol:
        return x0
    print("%3d  | %10f | %10f" % (i, x1, x1 - x0))
    while abs(x0 - x1) >= tol and abs(f(x1)) >= tol:
        i += 1
        x2 = x1 - f(x1) * (x1 - x0) / (f(x1) - f(x0))
        x0, x1 = x1, x2
        print("%3d  | %10f | %10f" % (i, x1, x1 - x0))
    return x1


if __name__ == "__main__":
    print(quadratic(1, -3, 2, 0, 2, 1e-5))
    print(quadratic(1, -3, 2, 1, 9, 1e-5))
    print(quadratic(1, -100, 2, -2, 5, 1e-5))
    print(quadratic(1, -3, 2, -50, 999, 1e-5))

#############################
#                           #
#     계산과학이론 및 실습1    #
#            HW 2           #
#      2017-11362 박건도     #
#                           #
#############################


# problem A


def problem_A():
    s = input("type string(last char must be int, 0~9): ")

    # if last char is not 0~9, return AssertError
    assert s[-1] in "0123456789"

    # last char = n
    n = int(s[-1])

    if n == 0:
        # if n == 0, print(s)
        print(s)
    else:
        # if n <  len(s), min = n,      print(s[:n])
        # if n == len(s), min = n,      print(s[:n])
        # if n >  len(s), min = len(s), print(s)
        print(s[: min(n, len(s))])

    # if n % 2 == 0, n is even, s[1, 3, 5, ...]
    # if n % 2 == 1. n is odd,  s[0, 2, 4, ...]
    print(s[1 - (n % 2) :: 2])


# problem B


def problem_B():
    # get input and split
    variables = input("type x and h, separated with space: ").split()
    # apply float(.) function to x and h
    x, h = map(float, variables)
    if x == 0 or h <= 0:
        # if x == 0 or h <= 0, do nothing
        return
    else:
        # def functions
        f = lambda t: 1 / t
        df = lambda t: -1 / t ** 2
        df_forward = lambda t: (f(t + h) - f(t)) / h
        df_backward = lambda t: (f(t) - f(t - h)) / h
        df_central = lambda t: (f(t + h) - f(t - h)) / (2 * h)
        # relative_err = ( exp_value - true_value ) / true_value * 100(%)
        err = lambda x, g: (g(x) - df(x)) / df(x) * 100

        print("     methods      │    f'(x)  │ err(%%)")
        print("──────────────────┼───────────┼─────────")
        print("forward  2-points │ %.6f │ %2.2f%%" % (df_forward(x), err(x, df_forward)))
        print("backward 2-points │ %.6f │ %2.2f%%" % (df_backward(x), err(x, df_backward)))
        print("central  3-points │ %.6f │ %2.2f%%" % (df_central(x), err(x, df_central)))


if __name__ == "__main__":
    problem_A()
    # problem_B()

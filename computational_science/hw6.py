#############################
#                           #
#     계산과학이론 및 실습1    #
#            HW 6           #
#      2017-11362 박건도     #
#                           #
#############################


# problem_A

from math import pi, exp, log


def PGE(A, b):  # partial Pivoting Gauss Elimination "in-place"
    assert len(A) == len(b), "# of left side should correspond with # of right side."
    n = len(A)
    for k in range(n):
        # Partial Poviting Part
        lead = [abs(A[i][k]) for i in range(k, n)]
        m = lead.index(max(lead)) + k
        if m != k:
            b[m], b[k] = b[k], b[m]
            for j in range(n):
                A[m][j], A[k][j] = A[k][j], A[m][j]
        # Gauss Elimination, forward
        for i in range(k + 1, n):
            coef = A[i][k] / A[k][k]
            b[i] -= coef * b[k]
            A[i][k] = 0
            for j in range(k + 1, n):
                A[i][j] -= coef * A[k][j]
    # backsolve
    for i in range(n - 1, -1, -1):
        for j in range(i + 1, n):
            b[i] -= A[i][j] * b[j]
        b[i] /= A[i][i]


# problem_B
def expand(s):
    # string -> list
    infix = []
    n = ""
    tmp = ""
    for char in s:
        if char.isdigit() or char == ".":  # digit
            n += char
        else:
            if n != "":
                infix.append(n)
                n = ""

        if char == "*":  # ** or *
            tmp += char
        else:
            if tmp != "":
                infix.append(tmp)
                tmp = ""

        if char == " ":
            continue
        elif char in ["x", "/", "(", ")", "+"]:
            infix.append(char)
        elif char == "-":
            if len(infix) == 0 or infix[-1] == "(":
                infix.append("0")
            infix.append("-")
    if n != "":
        infix.append(n)

    # infix -> postfix notation
    postfix = []
    stack = []
    prec = {"**": 2, "*": 1, "/": 1, "+": 0, "-": 0}
    for c in infix:
        if c not in ["(", ")", "**", "*", "/", "+", "-"]:
            postfix.append(c)
        elif c in prec.keys():
            p = prec[c]
            while len(stack) > 0:
                top = stack[-1]
                if top == "(":
                    break
                elif prec[top] < p:
                    break
                postfix.append(stack.pop())
            stack.append(c)
        elif c == "(":
            stack.append(c)
        elif c == ")":
            while True:
                x = stack.pop()
                if x == "(":
                    break
                postfix.append(x)
    while len(stack) > 0:
        postfix.append(stack.pop())

    # def calc
    def calc(operator, a, b):
        if operator == "**":
            if a[0] == 0:
                return [1]
            elif a[0] == 1:
                return b
            elif a[0] % 2 == 0:
                return calc("**", [a[0] // 2], calc("*", b, b))
            else:
                return calc("*", b, calc("**", [a[0] // 2], calc("*", b, b)))
        elif operator == "+":
            if len(a) >= len(b):
                return [a[i] + b[i] for i in range(len(b))] + a[len(b) :]
            else:
                return [a[i] + b[i] for i in range(len(a))] + b[len(a) :]
        elif operator == "-":
            return calc("+", b, calc("*", [-1], a))
        elif operator == "*":
            return [
                sum([a[k] * b[i - k] for k in range(max(0, i - len(b) + 1), min(i + 1, len(a)))])
                for i in range(len(a) + len(b) - 1)
            ]
        elif operator == "/":
            return calc("*", b, [1 / a[0]])

    # calculation, reuse stack
    for c in postfix:
        if c == "x":
            stack.append([0, 1])
        elif c not in ["**", "*", "/", "+", "-"]:
            stack.append([float(c)])
        else:
            a = stack.pop()
            b = stack.pop()
            stack.append(calc(c, a, b))

    # polynomial -> string
    poly = stack[0]
    ans = ""
    for i in range(len(poly) - 1, -1, -1):
        if poly[i] == 0:
            continue
        if int(poly[i]) == poly[i]:
            num = str(int(poly[i]))
        else:
            num = str(poly[i])
        if num[0] != "-":
            num = "+ " + num
        else:
            num = "- " + num[1:]
        if i == 0:
            ans += num
        else:
            if num == "+ 1":
                num = "+ "
            elif num == "- 1":
                num = "- "
            else:
                num += "*"
            if i == 1:
                ans += num + "x "
            else:
                ans += num + "x**" + str(i) + " "
    if ans[0] == "+":
        return ans[2:]
    else:
        return ans[0] + ans[2:]


if __name__ == "__main__":
    # A = [[9, -2, -1], [-8, -1, -4], [-5, -1, -2]]
    # b = [26, -5, -3]
    # PGE(A, b)
    # print(A, b)
    print(expand("(2*x + 1)*(3*x - 1)"))
    print(expand("(1.5*x + 2)**2 *3*x*x**5"))
    print(expand("(-2*x**2-3) * (2*x**2-3)"))

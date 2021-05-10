#############################
#                           #
#     계산과학이론 및 실습1    #
#            HW 7           #
#      2017-11362 박건도     #
#                           #
#############################


# problem_B
class frac(object):
    def __init__(self, numer, denom):
        if not (isinstance(numer, int) and isinstance(denom, int)):
            raise Exception("Numerator and denomiator should be integer.")
        elif denom <= 0:
            raise Exception("Denomiator should be positive integer.")
        self.a = numer
        self.b = denom  # form: a/b

    def simplify(self):
        if self.a == 0:  # 0 = 0/1
            self.b = 1
        else:
            a1, b1 = self.a, self.b
            while a1 % b1 != 0:  # Euclidean algorithm
                a1, b1 = b1, a1 % b1
            self.a //= b1
            self.b //= b1

    def __add__(self, other):
        if isinstance(other, int):
            other = frac(other, 1)
        ans = frac(self.a * other.b + other.a * self.b, self.b * other.b)
        ans.simplify()
        return ans

    def __mul__(self, other):
        if isinstance(other, int):
            other = frac(other, 1)
        ans = frac(self.a * other.a, self.b * other.b)
        ans.simplify()
        return ans

    def __sub__(self, other):
        return self.__add__(other.__mul__(-1))

    def __truediv__(self, other):
        return self.__mul__(frac(other.b, other.a))

    def __radd__(self, other):
        return self.__add__(other)

    def __rsub__(self, other):
        return self.__sub__(other)

    def __rmul__(self, other):
        return self.__mul__(other)

    def __rtruediv__(self, other):
        return self.__truediv__(other)

    def __lt__(self, other):
        return self.__sub__(other).a < 0

    def __le__(self, other):
        return self.__sub__(other).a <= 0

    def __gt__(self, other):
        return ~self.__le__(other)

    def __ge__(self, other):
        return ~self.__lt__(other)

    def __eq__(self, other):
        return self.__sub__(other).a == 0

    def __ne__(self, other):
        return ~self.__eq__(other)

    def __str__(self):
        return "{}/{}".format(self.a, self.b)

    def __float__(self):
        return self.a / self.b


if __name__ == "__main__":
    a = frac(1, 2)
    b = frac(2, 4)
    c = frac(2, 5)
    print(b - a)

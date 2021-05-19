#############################
#                           #
#     계산과학이론 및 실습1    #
#            HW 8           #
#      2017-11362 박건도     #
#                           #
#############################


# problem_A
class nucleic_acid(object):
    def __init__(self, seq):
        self.seq = seq.upper()
        for i in self.seq:
            if not i in "ACGTU":
                raise Exception("NOT a nucleic acid.")

    def __add__(self, other):
        return nucleic_acid(self.seq + other.seq)

    def __str__(self):
        return self.seq

    def cut(self, ind):
        if len(self.seq) <= ind <= 0:
            raise Exception("Invalid index.")
        return (nucleic_acid(self.seq[:ind]), nucleic_acid(self.seq[ind:]))


class DNA(nucleic_acid):
    def __init__(self, seq):
        nucleic_acid.__init__(self, seq)
        if "U" in self.seq:
            raise Exception("Not a DNA.")

    def __add__(self, other):
        if not isinstance(other, DNA):
            raise Exception("Not a DNA.")
        return nucleic_acid.__add__(self, other)

    def transcribe(self):
        rna = ""
        rule = {"A": "U", "T": "A", "G": "C", "C": "G"}
        for i in self.seq:
            rna += rule[i]
        return RNA(rna)


class RNA(nucleic_acid):
    def __init__(self, seq):
        nucleic_acid.__init__(self, seq)
        if "T" in self.seq:
            raise Exception("Not a RNA.")

    def __add__(self, other):
        if not isinstance(other, RNA):
            raise Exception("Not a RNA.")
        return nucleic_acid.__add__(self, other)


if __name__ == "__main__":
    x = nucleic_acid("AGCTT")
    print(x)
    y = nucleic_acid("GGTUU")
    print(y)
    print(x + y)
    a, b = (x + y).cut(2)
    print(a, b)
    x = DNA("ACGT")
    y = DNA("CCAT")
    z = RNA("ACGU")
    print(x + y)
    print(x.transcribe())
    print(y.transcribe() + z)

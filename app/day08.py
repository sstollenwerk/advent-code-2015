from ast import literal_eval


def exc_size(s):
    return len(s) - len(literal_eval(s))


def adjust(s):
    return '"' + s.replace("\\", "\\\\").replace('"', r"\"") + '"'


def exp_size(s):
    return len(adjust(s)) - len(s)


def main():

    with open(directory + "08.txt", "r") as f:
        lines = [i.strip() for i in f.readlines()]

    print("part1", sum(map(exc_size, lines)))
    print("part2", sum(map(exp_size, lines)))


directory = "E:/important/programming/advent-code-2015/data/"


main()

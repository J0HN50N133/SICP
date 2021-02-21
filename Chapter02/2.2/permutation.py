# fail

def accumulate(op, initial, seq: list) -> list:
    if len(seq) == 0:
        return initial
    else:
        return op(seq[0], accumulate(op, initial, seq.copy()[1:]))


accumulate(lambda x, y: x+y, 0, [1, 2, 3])


def map(proc, seq: list) -> list:
    result = []
    for i in seq:
        elem = proc(i)
        result.append(elem)
    return result


def flatmap(proc, seq: list) -> list:
    return accumulate(lambda x, y: x+y, [], seq)


def remove(x, s) -> list:
    return flatmap(lambda i: [] if i == x else [i], s)


print(remove(3, [1, 2, 3]))


def permutations(seq: list) -> list:
    def remove(x, s):
        return flatmap(lambda i: [] if i == x else [i], s)
    if len(seq) == 0:
        return []
    else:
        return flatmap(lambda x: map(lambda p: [x]+p, remove(x, seq)),
                       seq)


a = [1, 2, 3, 4]

# print(permutations(a))

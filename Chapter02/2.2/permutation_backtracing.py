def permutations(seq: list, position, end):
    if position == end - 1:
        print(seq)
    else:
        for index in range(position, end):
            seq[index], seq[position] = seq[position], seq[index]
            permutations(seq, position+1, end)
            seq[index], seq[position] = seq[position], seq[index]

permutations([1, 2, 3, 4, 5], 0, 5)

def subsets(s):
    if len(s) == 0:
        return []
    restWithoutFirst = subsets(s[1:])
    restWithFirst = [[s[0]] + i for i in restWithoutFirst]\
        if len(restWithoutFirst) > 0 else [[s[0]], []]
    return restWithoutFirst + restWithFirst


print(subsets([1, 2, 3]))

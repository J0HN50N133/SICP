def monitor(procedure):
    calls = 0
    def depatch(*args):
        nonlocal calls
        if len(args) == 0:
            calls += 1
            return procedure()
        elif args[0] == 'how-many-calls?':
            return calls
        else:
            calls += 1
            return procedure(*args)

    return depatch

def square(x):
    return x * x
def add(x, y):
    return x + y

if __name__ == '__main__':
    add = monitor(add)
    print(f'1 + 2 = {add(1, 2)}')
    print(f'2 + 2 = {add(2, 2)}')
    print(f'3 + 2 = {add(3, 2)}')
    print(f'4 + 2 = {add(4, 2)}')
    print(f'1 + 2 = {add(1, 2)}')
    print(f'2 + 2 = {add(2, 2)}')
    print(f'3 + 2 = {add(3, 2)}')
    print(f'4 + 2 = {add(4, 2)}')
    print(add('how-many-calls?'))

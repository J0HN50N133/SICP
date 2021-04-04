import random

random.seed()
circle = 0
total = 10 ** 10

for i in range(total):
    x, y = random.random(), random.random()
    if x ** 2 + y ** 2 < 1:
        circle += 1

print((circle / total) * 4)

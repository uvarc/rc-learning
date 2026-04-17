import random
import timeit

values = [random.randrange(100) for _ in range(100000)]

def dummy(x):
    return x**2+42.

def with_map():
    return list(map(dummy, values))

def with_comp():
    return [dummy(x) for x in values]

def with_loop():
    result = []
    for x in values:
        result.append(dummy(x))
    return result

print(f'Time for map {timeit.timeit(with_map, number=100):.4f}')
print(f'Time for comprehension {timeit.timeit(with_comp, number=100):.4f}')
print(f'Time for loop {timeit.timeit(with_loop, number=100):.4f}')


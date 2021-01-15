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

np_setup="""
import numpy as np
import random
values = [random.randrange(100) for _ in range(100000)]
xvals = np.array(values)
"""

np_code="""
def with_ndarray(x):
    return x**2+42.
with_ndarray(xvals)
"""

print(f'Time for map {timeit.timeit(with_map, number=100):.4f}')
print(f'Time for comprehension {timeit.timeit(with_comp, number=100):.4f}')
print(f'Time for loop {timeit.timeit(with_loop, number=100):.4f}')
print(f'Time for numpy {timeit.timeit(setup=np_setup,stmt=np_code, number=100):.4f}')

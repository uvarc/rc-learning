---
title: Ordering in Multiprocessing
date: 2026-08-21:26:29Z
type: book
weight: 2050
menu:
    parallel_programming:
        parent: Multithreaded Programming
---

Consider the following script:

```python
import os
from multiprocessing import Pool

def f(name):
    print("Greetings from "+str(name))

ncpus=int(os.getenv('NUM_THREADS'))
pool=Pool(processes=ncpus)
pool.map(f,range(ncpus))
pool.close()
pool.join()
```

An example of the output run with 8 cores is
```no-highlight
Greetings from 0
Greetings from 1
Greetings from 2
Greetings from 4
Greetings from 3
Greetings from 5
Greetings from 7
Greetings from 6
```

Like other parallel models we have seen, Multiprocessing is not deterministic.  Due to some differences in its implementation compared to compiled languages, threads may be more likely to be nearly ordered or even correctly ordered, but it is not guaranteed.

If we wish to enforce ordering, we must `return` the result and use `map` to collect the result.

```python
import os
from multiprocessing import Pool

def f(name):
    return "Greetings from "+str(name)

ncpus=int(os.getenv('NUM_THREADS'))
pool=Pool(processes=ncpus)
result=pool.map(f,range(ncpus))
print("\n".join(result))
pool.close()
pool.join()
```

This time we always get
```no-highlight
Greetings from 0
Greetings from 1
Greetings from 2
Greetings from 3
Greetings from 4
Greetings from 5
Greetings from 6
Greetings from 7
```

def collatz(N):
    """Collatz conjecture algorithm."""
    steps = [N]
    while N>1:
        if (N % 2 == 0):
            N = N // 2
        else:
            N = N * 3 + 1
        steps.append(N)
    return len(steps),max(steps)
numbers = [30,50]
for N in numbers:
    print (f"Computing Collatz for N={N}")
    header = f"{'N':>5}|{'stopping time':>15}|{'max value':>10}"
    print (header)
    print ("".join(['-'] * len(header)))
    for n in range(1,N+1):
        stop,max_value = collatz(n)
        print (f"{n:5}|{stop:15d}|{max_value:10d}")


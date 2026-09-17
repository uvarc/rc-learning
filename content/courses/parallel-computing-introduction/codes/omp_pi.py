from omp4py import *
    
@omp
def pi(n):
    w = 1.0 / n
    pi_value = 0.0
    with omp("parallel for reduction(+:pi_value)"):
        for i in range(n):
            local = (i + 0.5) * w
            pi_value += 4.0 / (1.0 + local * local)
    return pi_value * w

print(pi(10000000))  

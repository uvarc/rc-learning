"""
  This program computes and prints a table the sums of the integers from 1 to N
"""

def sumit(N):
    if N<0:
        return None
    sum_it=0
    if N==0:
        return N
    for i in range(1,N+1):
       sum_it+=i
    return sum_it

M=int(float(input("Please enter a nonnegative integer:")))

#Header.  ^ center-justifies. See documentation for more formatting tricks.
print("{:^10s}    {:^10s}".format("Integer","Sum"))
for i in range(1,M+1):
    print("{:^10d}    {:^10d}".format(i, sumit(i)))

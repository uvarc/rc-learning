"""
   Find the 'arithmetic numbers' in the integers 1 to N
   An arithmetic number is an integer such that the average of its divisors is a   itself an integer.
"""
# Set the value of N
# Would generally be read in, but we'll hard-code it.
N=100

arith_nums=[]
for i in range(1,N+1):
    divisors=[]
    for j in range(1,i+1):
        if i%j==0:   # divisor
            divisors.append(j)
    s=sum(divisors)
    avg=float(s)/len(divisors)
    if int(avg)==avg:
        arith_nums.append(i)

for n in arith_nums:
    print(n)

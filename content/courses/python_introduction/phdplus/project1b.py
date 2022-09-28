"""
   Simple program to compute speed after falling distance h
"""
import math

g_m=9.8
g_e=32

print("  Distance in m     Distance in ft   Speed in m/s    Speed in ft/s ")
for h in range(0,101,10):
    h_e=3.281*h
    v_m=math.sqrt(2.*g_m*h)
    v_e=math.sqrt(2.*g_e*h_e)
    print("    ",h,"     ",h_e,"   ",v_m,"      ",v_e)

tol=1.e-12
sign=1.0

x=0.1
my_sine=x

nterms=1
k=1
while abs(my_sine-math.sin(x)) > tol:
    sign=-sign
    k+=2
    my_sine+=sign*x**k/math.factorial(k)
    nterms+=1

print()
print("The number of terms required is ",nterms," my_sine ",my_sine," correct ",math.sin(x))




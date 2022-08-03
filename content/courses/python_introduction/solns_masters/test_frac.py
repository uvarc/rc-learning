from Fractions import Fraction
import copy

f1=Fraction(1,2)
f2=Fraction(6,8)
f3=f1+f2
f4=f1-f2
f5=f1*f2
f6=f1/f2
print(f1)
f2.reduce()
print()
print(f2)
print()
print(f3)
print()
print(f4)
print()
print(f5)
print()
print(f6)
print()

f1=Fraction(1,0)

f7=copy.copy(f1)
print(f7)

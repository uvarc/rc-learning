# -*- coding: utf-8 -*-
"""
Created on Mon May 20 10:30:26 2013
This program converts a sequence of decimal numbers into another base.
Author:     K. Holcomb
Changelog:  2013-05-20  Initial version 
"""

N=32
base=16

header1="Base 10"
header2="Base "+str(base)

print(f"{header1:>8} {header2:>8}")
print(f"------------------")

for i in range(N+1):
    if i==0:
        conversion=str(i)
    else:
        digits=[]
        result=i
        while result>0:
            digit=result%base
            result=result//base
            if digit<=9:
                base_digit=str(digit)
            else:
                alpha_order=digit%10
                base_digit=chr(alpha_order+65)
            digits.insert(0,base_digit)
        conversion="".join(digits)
    print("{:>6}   | {:>5}".format(i,conversion))

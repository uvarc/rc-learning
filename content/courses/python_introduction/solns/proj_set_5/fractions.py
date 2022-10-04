import math

nan=float('NaN')

def _gcd(a,b):
    while b!=0:
       t=b
       b=a%b
       a=t
    return a

class Fraction (object):
    """ 
    This class implements an object to define, manipulate, and print fractions.
    Version with a reduce method.
    Author:    K. Holcomb
    Changelog: 20150323 Initial version
    """

    def __init__(self,num,denom):
        if denom !=0:
            self.num=num
            self.denom=denom
        else:
            self.num=nan
            self.denom=nan

    def __add__(self,f):
        if math.isnan(self.denom) or math.isnan(f.denom):
           f3=Fraction(0,0)
        else:
           denom=self.denom*f.denom
           num  =self.num*f.denom+f.num*self.denom
           f3=Fraction(num,denom)
           f3.reduce()
        return f3

    def __sub__(self,f):
        if math.isnan(self.denom) or math.isnan(f.denom):
           f3=Fraction(0,0)
        else:
           denom=self.denom*f.denom
           num  =self.num*f.denom-f.num*self.denom
           f3=Fraction(num,denom)
           f3.reduce()
        return f3

    def __mul__(self,f):
        if math.isnan(self.denom) or math.isnan(f.denom):
           f3=Fraction(0,0)
        else:
           denom=self.denom*f.denom
           num  =self.num*f.num
           f3=Fraction(num,denom)
           f3.reduce()
        return f3

    def __truediv__(self,f):
        if math.isnan(self.denom) or math.isnan(f.denom) or f.num==0:
           f3=Fraction(0,0)
        else:
           denom=self.denom*f.num
           num  =self.num*f.denom
           f3=Fraction(num,denom)
           f3.reduce()
        return f3

    def reduce(self):
        if not math.isnan(self.denom):
           gcd=_gcd(self.num,self.denom)
           self.num/=gcd
           self.denom/=gcd

    def __copy__(self):
        return Fraction(self.num,self.denom)

    def __str__(self):
        return "%s/%s"%(str(self.num),str(self.denom))


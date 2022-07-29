import numpy as np

def inch_to_m(length):
    return length*0.0254

def pound_to_kg(weight):
    return weight*0.453592

def bmi(wt,ht):
    return wt/ht**2

weight,height=np.loadtxt("bodyfat.csv",delimiter=',',usecols=(2,3),skiprows=1,unpack=True)

wt=pound_to_kg(weight)
ht=inch_to_m(height)

bmi_data=bmi(wt,ht)

print(f"The mean is {bmi_data.mean():.1f} and the std is {bmi_data.std():.1f}")
print(f"The max is {bmi_data.max():.1f} and the min is {bmi_data.min():.1f}")

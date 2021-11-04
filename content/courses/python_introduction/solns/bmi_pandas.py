import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

def inch_to_m(length):
    return length*0.0254

def pound_to_kg(weight):
    return weight*0.453592

def bmi(wt,ht):
    return wt/ht**2

bf_data=pd.read_csv("bodyfat.csv")
bf_data.columns=['bodyfat','age','weight_lbs','height_inch']

wt=pound_to_kg(bf_data.weight_lbs)
ht=inch_to_m(bf_data.height_inch)

bmi_vals=bmi(wt,ht)
bf_data['BMI']=bmi_vals

bf_data.plot.scatter(x='bodyfat',y='BMI')

Q1=bf_data.BMI.quantile(.25)
Q3=bf_data.BMI.quantile(.75)
QIF=Q3-Q1
lower_limit=Q1-1.5*QIF
upper_limit=Q3+1.5*QIF
bf_data.loc[(bf_data['BMI']>upper_limit) | (bf_data['BMI']<lower_limit)]=np.nan

bf_data.plot.scatter(x='bodyfat',y='BMI')
plt.show()


import numpy as np
import matplotlib.pyplot as plt
import seaborn as sn
import pandas as pd

iris=sn.load_dataset('iris')

print(iris.describe())

print(iris.columns)

for i in iris.index.tolist()[0:31]:
    print(iris.loc[i,'species'])

print(iris['petal_length'].mean())

print('The 91st row')
print(iris.iloc[90])

petal_data=iris.loc[:,["petal_length","petal_width","species"]]

print(petal_data.describe())

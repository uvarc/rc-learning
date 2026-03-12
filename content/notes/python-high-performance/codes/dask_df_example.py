import numpy as np
import pandas as pd
import dask
import dask.dataframe as dd

df = dask.datasets.timeseries('2000','2010',freq='2H')
print(df.head())

df2 = df[df.y > 0]
df3 = df2.groupby("name").x.std()
print(df3)
print(df3.compute())
df4=df.groupby("name").aggregate({"x": "sum", "y": "max"})
print(df4.compute())

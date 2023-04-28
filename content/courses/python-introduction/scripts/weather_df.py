import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

dates=["2000-01-01 00:00:00","2000-01-02 00:00:00",
       "2000-01-03 00:00:00","2000-01-04 00:00:00",
       "2000-01-05 00:00:00","2000-01-06 00:00:00"]
weather=pd.DataFrame({"Minimum Temp":[-5.87,-3.82,-4.58,-6.40,-5.50,-3.29],
                      "Maximum Temp":[8.79,4.78,5.10,2.68,6.18,4.50],
                      "Cloud Cover":[3,5,3,2,3,5]},
                      index=dates)

print(weather.columns)
print(weather["2000-01-02":"2000-01-04"])

#Two ways to rename the columns
weather.rename(columns={'Minimum Temp':'Tmin','Maximum Temp':'Tmax'},inplace=True)
#weather.columns=["Tmin","Tmax","Cloud Cover"]

print(weather.Tmin.mean())

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

weather=pd.DataFrame({"Date":["2000-01-01 00:00:00","2000-01-02 00:00:00", 
                              "2000-01-03 00:00:00","2000-01-04 00:00:00",
                              "2000-01-05 00:00:00","2000-01-06 00:00:00"],
                      "Minimum Temp":[-5.87,-3.82,-4.58,-6.40,-5.50,-3.29],
                      "Maximum Temp":[8.79,4.78,5.10,2.68,6.18,4.50]})

print(weather.describe())
print(weather["Minimum Temp"].mean())
print(weather["Maximum Temp"].mean())

tmin_vals=weather["Minimum Temp"].values
tmax_vals=weather["Maximum Temp"].values

weather["Minimum Temp"].plot()
weather["Maximum Temp"].plot()

plt.show()




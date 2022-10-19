import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

rome_weather=pd.read_csv("rome_rain.dat",sep=";")

means=mean_precip=rome_weather.mean(numeric_only=True)
totals=total_precip=rome_weather.sum(numeric_only=True)

print("Mean values")
print(means)
print("Total values")
print(totals)

rome_weather.max(numeric_only=True)
max_precip_mm=rome_weather['Millimeters'].max()
max_loc=rome_weather['Millimeters'].idxmax()

print(rome_weather.min(numeric_only=True))
min_precip_mm=rome_weather['Millimeters'].min()
min_loc=rome_weather['Millimeters'].idxmin()

print(f"The minimum precip is {min_precip_mm} mm in {rome_weather.iloc[min_loc,0]}")
print(f"The maximum precip is {max_precip_mm} mm in {rome_weather.iloc[max_loc,0]}")

xlabels=rome_weather['Month'].tolist()
ax=rome_weather.plot(y=['Millimeters'],title="Annual Precipitation in Rome, Italy",legend=False)
ax.set_ylabel("Precipitation in mm")
ax.set_xticks(rome_weather.index)
ax.set_xticklabels(rome_weather.Month,rotation=45)
plt.show()

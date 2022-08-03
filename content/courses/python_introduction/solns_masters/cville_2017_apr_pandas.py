import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

weather=pd.read_excel("cville_2017_april.xlsx")

xlabels=weather.Date.dt.strftime("%Y-%m-%d")

axs1=weather["Avg Wind (mph)"].plot(title="Average Wind Speed")
axs1.set_xticklabels(rotation=45,labels=xlabels)
plt.xlabel("Date")
plt.ylabel("Speed in MPH")
plt.show()

axs2=weather["Avg Wind (mph)"].plot.bar(title="Average Wind Speed and Gusts")
weather["Wind Gust (mph)"].plot(color="r")
axs2.xaxis.set_major_locator(plt.MaxNLocator(8))
axs2.set_xticklabels(rotation=45,labels=xlabels)
plt.xlabel("Date")
plt.ylabel("Speed in MPH")
axs2.legend()
plt.show()

#How to make a stacked chart if you want the values added
axs3=weather[["Lo Temp","Hi Temp"]].plot.bar(title="Stacked High and Low Temperatures",stacked=True)
axs3.xaxis.set_major_locator(plt.MaxNLocator(8))
axs3.set_xticklabels(rotation=45,labels=xlabels)
plt.xlabel("Date")
plt.ylabel("Temperature (F)")
plt.show()

#What we really want
fig,axs4=plt.subplots(1,1)
lows=weather["Lo Temp"]
plt.bar(weather["Date"],weather["Hi Temp"],color="orange")
plt.bar(weather["Date"],lows)
axs4.xaxis.set_major_locator(plt.MaxNLocator(8))
axs4.set_xticklabels(rotation=45,labels=xlabels)
plt.xlabel("Date")
plt.ylabel("Temperature (F)")
plt.title("Daily High and Low Temperatures")
plt.legend(labels=["High","Low"])
plt.show()

axs5=weather[["Hi Temp","Lo Temp"]].plot.bar(title="Daily High and Low Temperatures")
axs5.xaxis.set_major_locator(plt.MaxNLocator(8))
axs5.set_xticklabels(rotation=45,labels=xlabels)
plt.xlabel("Date")
plt.ylabel("Temperature (F)")
plt.show()

days=weather.groupby("Condition")["Date"].count()

days.plot.bar(title="Days Per Cloud Condition")
plt.ylabel("Count")

plt.show()


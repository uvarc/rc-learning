import numpy as np
import matplotlib.pyplot as plt

date=[]
hi_temp_list=[]
lo_temp_list=[]
avg_wind_list=[]
wind_gust_list=[]
precip_list=[]
condition=[]
with open('cville_2017_april.csv', 'r') as file:
    file.readline()  #skip header
    for line in file:
        row=line.strip("\r\n").split(',')
        date.append(row[0])
        hi_temp_list.append(float(row[1]))
        lo_temp_list.append(float(row[2]))
        avg_wind_list.append(float(row[3]))
        wind_gust_list.append(float(row[4]))
        precip_list.append(float(row[5]))
        condition.append(row[6])

hi_temp=np.array(hi_temp_list)
lo_temp=np.array(lo_temp_list)
avg_wind=np.array(avg_wind_list)
wind_gust=np.array(wind_gust_list)
precip=np.array(precip_list)

xlabels=date

fig,axs1=plt.subplots()
axs1.set_title("Average Wind Speed")
axs1.set_xticklabels(rotation=45,labels=xlabels)
plt.xlabel("Date")
plt.ylabel("Speed in MPH")
plt.plot(avg_wind)
plt.show()

fig,axs2=plt.subplots()
axs2.set_title("Average Wind Speed and Gusts")
plt.plot(wind_gust,color='red')
plt.bar(xlabels,avg_wind)
axs2.xaxis.set_major_locator(plt.MaxNLocator(8))
axs2.set_xticklabels(rotation=45,labels=xlabels)
plt.xlabel("Date")
plt.ylabel("Speed in MPH")
axs2.legend()
plt.show()

#How to make a stacked chart if you want the values added
fig,axs3=plt.subplots()
axs3.set_title("Stacked High and Low Temperatures")
plt.bar(xlabels,lo_temp)
plt.bar(xlabels,hi_temp,bottom=lo_temp)
axs3.xaxis.set_major_locator(plt.MaxNLocator(8))
axs3.set_xticklabels(rotation=45,labels=xlabels)
plt.xlabel("Date")
plt.ylabel("Temperature (F)")
plt.show()

#What we really want
fig,axs4=plt.subplots()
x = np.arange(len(xlabels))  
#Calculate a reasonable width (found on StackOverflow)
width = np.min(np.diff(x))/3.
plt.bar(x-width/2,hi_temp,width=width,label="High",align="center")
plt.bar(x+width/2,lo_temp,width=width,label="Low",align="center")
axs4.xaxis.set_major_locator(plt.MaxNLocator(8))
axs4.set_xticklabels(rotation=45,labels=xlabels)
plt.xlabel("Date")
plt.ylabel("Temperature (F)")
plt.title("Daily High and Low Temperatures")
plt.legend(labels=["High","Low"])
plt.show()

days={}
for i in range(len(condition)):
    if condition[i] not in days:
        days[condition[i]]=1
    else:
        days[condition[i]]+=1
print(days)
   
fig,axs5=plt.subplots()
plt.title("Days Per Cloud Condition")
plt.ylabel("Count")
sky_condition=list(days.keys())
sky_count=days.values()
x=np.arange(len(days))
plt.bar(sky_condition,sky_count)
plt.show()




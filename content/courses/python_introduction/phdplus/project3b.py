import numpy as np
import matplotlib.pyplot as plt

raw_data=np.loadtxt("world_population.csv",delimiter=',',skiprows=1,dtype='str')
countries=raw_data[:,2]
populations=np.asarray(raw_data[:,5:13],dtype='float')

rank=np.asarray(raw_data[:,0],dtype='int')
rank_index=np.argsort(rank)

top_ten_pop=populations[rank_index[:11],:]
#invert
top_ten_pop.sort(axis=1)
top_ten_countries=countries[rank_index[:11]]

years=np.array([2022,2020,2015,2010,2000,1990,1980,1970])
years.sort()

year_interval=years[1:]-years[0:-1]

fig1,ax1=plt.subplots()
ax1.set_title("Ten Most Populous Countries")
for p in top_ten_pop:
    ax1.plot(years,p)
ax1.legend(top_ten_countries,bbox_to_anchor=(1.4,1.))

pop_change=top_ten_pop[:,1:]-top_ten_pop[:,0:-1]
pop_change_rate=pop_change/year_interval

fig2,(ax2,ax3)=plt.subplots(2,1)
ax2.set_title("Change in Population of Top Ten")
for i,p in enumerate(pop_change):
    ax2.plot(years[0:-1],p)
ax2.legend(top_ten_countries,bbox_to_anchor=(1.4,0.6))
fig2.subplots_adjust(hspace=0.7)
ax3.set_title("Rate of Change of Population")
for p in pop_change_rate:
    ax3.plot(years[0:-1],p)

plt.figure()
plt.hist(populations)

plt.show()

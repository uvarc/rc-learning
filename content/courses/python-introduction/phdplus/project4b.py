import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

world_pop_data=pd.read_csv("world_population.csv")
pop_years=world_pop_data.iloc[:,5:13]
pop_years=pop_years.iloc[:,::-1]
country_data=world_pop_data.iloc[:,0:5]

pop_data=pd.concat([country_data,pop_years],axis=1)
pop_data.rename(columns={'1970 Population':'1970','1980 Population':'1980','1990 Population':'1990','2000 Population':'2000','2010 Population':'2010','2015 Population':'2015','2020 Population':'2020','2022 Population':'2022'},inplace=True)

year_list=pop_data.columns[5:].tolist()

years=[]
for y in year_list:
    years.append(int(y.split()[0]))
years=np.array(years)

top_ten=pop_data.sort_values(by="Rank").iloc[0:10]

labels=top_ten.loc[:,'Country'].tolist()

pop_years=top_ten.drop(labels=['Rank','CCA3','Country','Capital','Continent'],axis=1)

pop_years_inv=pop_years.T
pop_years_inv.columns=labels

pop_years_inv.plot(kind='line',title='Population Trend for Top Ten Countries').legend(bbox_to_anchor=(1.4,1.))

populations=pop_years.values

pop_change=populations[:,1:]-populations[:,0:-1]
year_interval=years[1:]-years[0:-1]
pop_rate_change=pop_change/year_interval

pop_changes=pd.DataFrame(data=pop_rate_change.T,columns=labels,index=years[0:-1])

pop_changes.plot(kind='line',title='Rate of Population Change for Top Ten Countries').legend(bbox_to_anchor=(1.4,1.))

plt.show()

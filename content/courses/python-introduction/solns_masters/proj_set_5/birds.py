import sys
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

class Birddata:

    def __init__(self,species,years,data):
        self.species=species
        self.years=years
        self.obs=np.array(data)

    def stats(self):
        mean=np.mean(self.obs)
        std=np.std(self.obs)
        median=np.median(self.obs)
        return mean,std,median

    def minmax(self):
        min_val=np.min(self.obs)
        max_val=np.max(self.obs)
        min_year=self.years[np.argmin(self.obs)]
        max_year=self.years[np.argmax(self.obs)]
        return min_val,max_val,min_year,max_year

if (len(sys.argv)<2):
    print("Please include the input file on the command line.")
    sys.exit()
else:
    infile=sys.argv[1]

df=pd.read_csv(infile)
years_strings=df.columns[1:].values
years=np.array([int(float(years_strings[i])) for i in range(len(years_strings))])

birds=[]
for i in range(len(df)):
    species=df.iloc[i,0]
    obs_list=df.iloc[i,1:].tolist()
    birds.append(Birddata(species,years,np.array(obs_list)))

bird_name=input("Please enter the common name of a bird, without spaces:")

ind=-1
for bird in range(len(birds)):
    common_name=birds[bird].species.lower()
    if bird_name.lower()==common_name:
        ind=bird
        break

if ind==-1:
    print("Requested species not found in this dataset.")
    sys.exit()

mean,std,median=birds[ind].stats()
min_val,max_val,min_year,max_year=birds[ind].minmax()
obs=birds[ind].obs

print("Summary of Observational Data for {:}".format(bird_name))
print("Mean: {:.2f}, median: {:.2f}, standard deviation: {:.2f}".format(mean,median,std))
print("Min obs: {:.2f} in year {:}, max obs: {:.2f} in year {:}".format(min_val,min_year,max_val,max_year))

fig,ax=plt.subplots()

title_string="Observations for "+bird_name
ax.set_xlabel("Year")
ax.set_ylabel("Number of Observations")
ax.set_title(title_string)
plt.plot(years,obs)
plt.show()

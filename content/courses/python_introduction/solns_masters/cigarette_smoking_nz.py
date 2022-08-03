import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

smokers=pd.read_csv("cigarette-smoking-behaviour-2018-census.csv") 

smokers.columns=["Code","Behavior","Count"]

labels=smokers.Behavior.iloc[:5].tolist()
counts=smokers.Count.iloc[:5].tolist()
countbc=pd.DataFrame(counts,index=labels)
countbc.plot.bar(legend=False,rot=70,title="Smoking in New Zealand as of 2018")

smokers["Ratio"]=smokers.Count/smokers.at[6,'Count']
ratios=smokers["Ratio"]

percents=[100.*x for x in ratios.tolist()[:5]]
countbc=pd.DataFrame(percents,index=labels)
countbc.plot.bar(legend=False,rot=70,title="Smoking in New Zealand as of 2018")
plt.ylabel("Percent of Total Responses")
plt.show()

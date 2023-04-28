import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

randns=pd.Series(np.random.randn(1000))
print(randns.head())
print(randns.tail())
print(randns.describe())
randns.plot()
plt.show()

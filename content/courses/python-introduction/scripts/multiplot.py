import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

dates = pd.date_range("1/1/2000", periods=1000)

df = pd.DataFrame(np.random.randn(1000, 4), index=dates, columns=list("ABCD"))
df = df.cumsum()
df.plot();

plt.show()

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

df = pd.DataFrame(abs(np.random.randn(10, 4)), columns=list("ABCD"))

df.plot.bar();
plt.show()

import numpy as np
import pandas as pd
import seaborn as sns

import xarray as xr

np.random.seed(123)

xr.set_options(display_style="html")

times = pd.date_range("2000-01-01", "2001-12-31", name="time")
annual_cycle = np.sin(2 * np.pi * (times.dayofyear.values / 365.25 - 0.28))

base = 10 + 15 * annual_cycle
tmin = base + np.random.randn(annual_cycle.size)
tmax = base + 10 +  np.random.randn(annual_cycle.size)

print("date,","tmin,","tmax")
for i in range(annual_cycle.size):
     print("{},{:.2f},{:.2f}".format(times[i],tmin[i],tmax[i]))

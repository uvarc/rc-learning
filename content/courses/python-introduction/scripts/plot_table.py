import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

def rain_condition(v):
    if v < 1.75:
        return "Dry"
    elif v < 2.75:
        return "Rain"
    return "Heavy Rain"

def make_pretty(styler):
    styler.set_caption("Weather Conditions")
    styler.format(rain_condition)
#    The following line is only valid for Pandas >= 1.4
#    styler.format_index(lambda v: v.strftime("%A"))
    styler.background_gradient(axis=None, vmin=1, vmax=5, cmap="YlGnBu")
    return styler


weather_df = pd.DataFrame(np.random.rand(10,2)*5,
                          index=pd.date_range(start="2021-01-01", periods=10),
                          columns=["Tokyo", "Beijing"])
print(weather_df)

#HTML for Jupyter
weather_df.loc["2021-01-04":"2021-01-08"].style.pipe(make_pretty)

#Plot
ax = plt.subplot(111, frame_on=False) # no visible frame
ax.xaxis.set_visible(False)  # hide the x axis
ax.yaxis.set_visible(False)  # hide the y axis
tabla = pd.plotting.table(ax, weather_df, loc='upper right', colWidths=[0.17]*len(weather_df.columns)) 
tabla.scale(1.5,1.5)
plt.show()





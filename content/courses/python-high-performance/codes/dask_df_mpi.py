import os
import dask
import dask_mpi as dm
import dask.distributed as dd
import dask.dataframe as ddf
from dask.distributed import Client

# Initialize Dask cluster and store worker files in current work directory
dm.initialize(local_directory=os.getcwd())

#Connect clients
client = Client()

df=dask.datasets.timeseries()
df2=df[df.y>0]
df3=df2.groupby('name').x.std()
df3.compute()
print("Finished")
client.close()

import dask
import dask.array as da

x = da.random.random((100, 100), chunks=(10, 100))
y=x.mean()
# visualize the low level Dask graph after optimizations
y.visualize(filename="da_mean.svg", optimize_graph=True)

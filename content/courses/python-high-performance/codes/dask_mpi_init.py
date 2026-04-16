from dask_mpi import initialize
initialize()

from dask.distributed import Client
client = Client()  # Connect this local process to remote workers

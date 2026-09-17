import mpi4py
provided=mpi4py.rc.thread_level
#Reference:https://mpi4py.readthedocs.io/en/stable/mpi4py.html#mpi4py.mpi4py.rc

from mpi4py import MPI

thread_levels=["single", "funneled", "serialized", "multiple"]
mpi_levels=[MPI.THREAD_SINGLE,MPI.THREAD_FUNNELED,MPI.THREAD_SERIALIZED,MPI.THREAD_MULTIPLE] 
for t in range(len(thread_levels)):
    print(t,mpi_levels[t],thread_levels[t])

print(f"Requested:{MPI.THREAD_MULTIPLE} Provided:{provided}")

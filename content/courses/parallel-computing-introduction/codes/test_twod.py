import numpy as np
from mpi4py import MPI

comm = MPI.COMM_WORLD
world_size = comm.Get_size()
rank = comm.Get_rank()

# Size of my send and receive matrix
height = 3
width  = 3

# Variables used to define the struct
count = 3
blocklength = 1
stride = 3

# Int seemingly used to define how many of the structs are being sent?
sending_int = 1

# Here I define the struct with Create_vector:
column_type = MPI.DOUBLE.Create_vector(count = count,blocklength = blocklength,stride = stride)
column_type.Commit()


if rank == 0:
    send_array = np.arange(width*height,dtype='float').reshape(height,width)
    send_array += 1

    comm.Send([send_array,sending_int, column_type], dest = 1, tag = 0) 

    print(send_array)

if rank == 1:
    rec_array = np.zeros(width*height,dtype='float').reshape(height, width)

    column_end_type = MPI.DOUBLE.Create_subarray([3,3],[3,1],[0,2])
    column_end_type.Commit()
    comm.Recv([rec_array,sending_int,column_end_type], source = 0, tag = 0)

    print(rec_array)

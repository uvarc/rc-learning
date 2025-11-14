from mpi4py import MPI

comm = MPI.COMM_WORLD
rank = comm.Get_rank()
size = comm.Get_size()

# Define the file name
filename = "output.txt"

# Open the file in append mode and write a message from each processor
with open(filename, "a") as file:
    file.write(f"Hello from processor {rank} of {size}\n")

# Synchronize all processors before reading the file
comm.Barrier()

# Now let only one processor (e.g., rank 0) read and display the file content
if rank == 0:
    # Open the file in read mode
     with open(filename, "r") as file:
        # Read and display the file content
        print("File contents:")
        print(file.read())


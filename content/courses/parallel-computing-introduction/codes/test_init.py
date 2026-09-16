from mpi4py import MPI
import sys

def main():
    # Request the highest level of threading support
    required = MPI.THREAD_MULTIPLE

    # Initialize MPI with threading support
    try:
        provided = MPI.Init_thread(required)
    except Exception as e:
        print(f"Error initializing MPI with threading: {e}")
        sys.exit(1)

    # Get communicator and rank
    comm = MPI.COMM_WORLD
    rank = comm.Get_rank()
    size = comm.Get_size()

    # Print threading support info
    if rank == 0:
        print(f"Requested threading level: {thread_level_name(required)}")
        print(f"Provided threading level:  {thread_level_name(provided)}")

    # Example parallel work
    print(f"Hello from rank {rank} of {size}")

    # Finalize MPI
    MPI.Finalize()

def thread_level_name(level):
    """Return a human-readable name for MPI thread support level."""
    mapping = {
        MPI.THREAD_SINGLE: "THREAD_SINGLE",
        MPI.THREAD_FUNNELED: "THREAD_FUNNELED",
        MPI.THREAD_SERIALIZED: "THREAD_SERIALIZED",
        MPI.THREAD_MULTIPLE: "THREAD_MULTIPLE"
    }
    return mapping.get(level, f"Unknown({level})")

if __name__ == "__main__":
    main()


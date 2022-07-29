import os
#CWD
with os.scandir() as it:
    for entry in it:
        if entry.is_file():
            print("{} is a file".format(entry.name))
        elif entry.is_dir():
            print("{} is a directory".format(entry.name))
        else:
            print("{} is not a file or a folder".format(entry.name))
  

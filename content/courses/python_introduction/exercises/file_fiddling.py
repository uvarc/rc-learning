import os
import shutil

home_dir=os.path.expanduser('~')
doc_dir="Documents"
doc_path=os.path.join(home_dir,doc_dir)

os.chdir(doc_path)

for file in os.listdir():
    print(file)
    if file.endswith("txt"):
        print("File {} is a text file.".format(file))

new_folder="MyTestFolder"
if not os.path.isdir(new_folder):
    os.mkdir(new_folder)

new_file="new_file.txt"
f=open(new_file,"w")

f.write("Here are some words for this file.\n")
f.close()

shutil.move(new_file,new_folder)


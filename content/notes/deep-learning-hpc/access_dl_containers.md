---
title: Accessing Deep Learning Containers
date: 2024-06-28T01:51:43Z
type: docs 
weight: 100
menu: 
    deep-learning-hpc:
toc: true
---

Software on HPC is accessed via environment modules or containers.

* __Software Module:__
  * Examples: R, Rstudio, JupyterLab, TensorFlow, PyTorch
  * Full list of software available on HPC: [https://www.rc.virginia.edu/userinfo/hpc/software/complete-list/](https://www.rc.virginia.edu/userinfo/hpc/software/complete-list/)
* __Container:__
  * Containers bundle an application, the libraries and other executables it may need, and even the data used with the application into portable, self-contained files called images.
  * Containers simplify installation and management of software with complex dependencies and can also be used to package workflows.

Visit our [documentation](https://www.rc.virginia.edu/userinfo/rivanna/software/overview/) on HPC software modules and containers for more information.

## Access through Open OnDemand

{{< figure src=/notes/deep-learning-hpc/img/OOD_jypnb.png width=70% height=70% >}}

* Click on the kernel to open a Jupyter Notebook.
* Packages from the selected kernel will be available for use in the notebook.


## Run DL Script on the Command Line

__Use the PyTorch container:__
```bash
module load apptainer pytorch 
apptainer run --nv $CONTAINERDIR/pytorch-2.0.1.sif file_name.py
```
__Use the TensorFlow/Keras container:__
```bash
module load apptainer tensorflow 
apptainer run --nv $CONTAINERDIR/tensorflow-2.13.0.sif file_name.py
```   

* The `--nv` flag tells the command to use the GPU
* The default command defined in each container is _python_ so using `run` basically executes `python file_name.py`


## Check Your Knowledge
1. Log in to Rivanna using the Interactive partition using the following parameters.
  * 2 hours
  * 8 cores
  * Allocation: hpc_training
  * GPU: yes, 1
2. Copy the folder `project/hpc_training/dl_with_hpc` to your home or scratch account using one of the following:
```bash
cp -r /project/hpc_training/dl_with_hpc ~/<...>
# OR
cp -r /project/hpc_training/dl_with_hpc /scratch/<ID>/<...>
```
3. Convert __example1.ipynb__ file to __example1.py__ file (make sure you are in the `dl_with_hpc` folder)
```bash
jupyter nbconvert --to python example1.ipynb
```
4. Run `example1.py` using the Pytorch container.


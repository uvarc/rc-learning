---
title: Effective Use of HPC for Deep Learning
date: "2024-06-06T00:00:00"
type: docs 
weight: 4550
menu: 
    deep-learning-distributed:
        name: Effective Use of HPC for Deep Learning
---


* Optimize single-node/single-GPU performance?
  * Use performance analysis tools
  * Tune and optimize data pipeline
  * Make effective use of the hardware (e.g. mixed precision)
* Before running on multiple nodes, make sure the job can scale well to 8 GPUs on a single node. Never do one GPU per node for multinode jobs.
* Use multiple CPU cores for data loading.
* For hyperparameter tuning consider using a job array. This will allow you to run multiple jobs with one `sbatch` command.


> You will likely want the code loading data to look like this:
>```python
>train_dataset = datasets['train'].map(preprocess_data, num_parallel_calls=tf.data.AUTOTUNE)
>    .cache()
>    .shuffle(SHUFFLE_SIZE)
>    .batch(BATCH_SIZE)
>    .prefetch(tf.data.AUTOTUNE)
>```


## Questions or Need more help?

__Office Hours via Zoom:__

* Tuesdays: 3 pm - 5 pm
* Thursdays: 10 am - noon

Zoom Links are available at https://www.rc.virginia.edu/support/#office-hours
* Website: https://rc.virginia.edu

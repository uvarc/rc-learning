---
title: Pipeline Debugging Tips
date: "2025-02-23T00:00:00"
type: docs 
weight: 2000
menu: 
    llms-hpc:
        parent: Inference
---


Use the CPU. Error messages for code running on the CPU tend to be more helpful than those for code running on the GPU.

Run the pipeline tokenizer and model separately to see where the error is being generated.

Check out the data you are feeding to the pipeline that is causing the error.  Is it somehow different than other pieces of data?

If you get stuck, please [submit a ticket to RC](https://www.rc.virginia.edu/form/support-request/).  We can help!


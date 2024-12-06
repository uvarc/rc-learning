---
title: Tackling data-intensive problems on desktops and clusters
date: 2024-11-16-20:28:40Z
type: docs 
weight: 250
menu: 
    matlab-parallel-programming:
---


## Extend Big Data Capabilities in MATLAB with Parallel Computing

{{< figure src=/notes/matlab-parallel-programming/img/matlab-parallel-diagram.png >}}

---

MATLAB provides a single, high-performance environment for working with big data. You can:

- Access data that does not fit in memory from standard big data sources and use data from a Hadoop Distributed File System (HDFS) in MATLAB.
- Create repositories of large amounts of images, spreadsheets, and custom files using a datastore.
- Utilize capabilities tailored for both beginners and power users of big data applications.
- Use tall arrays for working with columnar data containing millions or billions of rows.
- Partition data too large for a single computer across multiple computers in a cluster using distributed arrays.
- Take advantage of hundreds of MATLAB and toolbox functions supported with tall, distributed, and sparse arrays.
- Create your own big data algorithms using MATLAB MapReduce or the MATLAB API for Spark.
- Program once and scale to many execution environments, including desktop machines, compute clusters, and Spark clusters.
- Easily access data, regardless of how it is stored.
- Prototype algorithms quickly using small datasets.
- Scale up to big datasets running on large clusters, all while using the same intuitive MATLAB syntax you're accustomed to.

The Parallel Computing Toolbox extends the capabilities of tall arrays and MapReduce built into MATLAB, allowing you to run on local workers for improved performance. You can then scale tall arrays and MapReduce up to additional resources with MATLAB Parallel Server, on traditional clusters or Apache Spark™ and Hadoop® clusters.

---

## Overcoming Single Machine Memory Limitations Distributed Arrays

{{< figure src=/notes/matlab-parallel-programming/img/distributed-arrays.png >}}

---

- **Distributed Data**: MATLAB offers a tool for working with large datasets through **distributed arrays**. These arrays enable you to manipulate large matrices using the combined memory of a cluster. On your desktop, distributed arrays appear just like normal MATLAB variables, but their data is distributed across MATLAB workers on the cluster. When you perform an operation on a distributed array, the work happens out in the cluster, but the workflow remains unchanged from your normal MATLAB experience. You can prototype distributed arrays on your desktop and then scale up to additional resources with **MATLAB Parallel Server**.

- **Common Actions**:
  - **Matrix Manipulation**
  - **Linear Algebra and Signal Processing**

A large number of **standard MATLAB functions** work with distributed arrays just as they do for normal variables. This means you can program a **distributed-array algorithm** in the same way as you would program a normal in-memory algorithm. 

MATLAB also provides **overloaded functions** that work transparently with variables stored across the memory of multiple physical computers. These functions allow you to write one application that can work with both local data and distributed data, without needing to be an expert in message passing. The goal is to make complex tasks easier, allowing you to focus on your algorithms and research, rather than on the underlying details of parallel computing.

## Tall Arrays

{{< figure src="/notes/matlab-parallel-programming/img/tall-data.png" height="200" >}}

- **Applicable when**:
  - Data is **columnar** with **many** rows.
  - The overall data size is **too big to fit into memory**.
  - Operations are mathematical or statistical in nature.

- **Statistical and Machine Learning Applications**:
  - Hundreds of functions are supported in MATLAB and the **Statistics and Machine Learning Toolbox**.

**Tall arrays** are designed to handle large datasets that do not fit into memory by automatically breaking the data into small "chunks" that fit into memory. These arrays process the data one "chunk" at a time, allowing you to work with data that would otherwise be too large for your system's memory.

The processing code for tall arrays is the same as it would be for ordinary arrays, making it easy to integrate tall arrays into your existing MATLAB workflows. Tall arrays wrap around a datastore and treat the entire dataset as a single, continuous table or array. When you need to perform calculations, the datastore allows you to work through the array one piece at a time, without requiring you to manage the data chunks manually.

For example, when working with CSV files containing tabular data, the resulting tall array is actually a **tall table**. You can then use standard table functions, such as `summary` or dot references to access columns, and apply operations like `max`, `min`, `plus`, and `minus` just as you would with a non-tall table.

{{< figure src="/notes/matlab-parallel-programming/img/tall-arrays-diagram.png" height="300px" >}}

---

## Demo: Predicting Cost of Taxi Ride in NYC Working with tall arrays in MATLAB

* **Objective:** Create a model to predict the cost of a taxi ride in New York City
* **Inputs:**
  * Monthly taxi ride log files
  * The local data set contains > 2 million rows
* **Approach:**
  * Preprocess and explore data
  * Work with subset of data for prototyping
  * Fit linear model
  * Predict fare and validate model

{{< figure src=/notes/matlab-parallel-programming/img/taxi-example.png >}}


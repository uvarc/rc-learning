---
title: Reading and Writing Data in Pandas
toc: true
type: docs
draft: false
weight: 135

menu:
    python-introduction:
        parent: Pandas
        weight: 135
---

Pandas provides multiple functions to read files in several formats.

## CSV 
Pandas easily reads files in CSV (comma separated values) format.  The separator does not have to be a comma, but anything else must be specified through the `sep` keyword argument.

Suppose we have a file `weather.txt` containing weather data over a year for one site.  It is in comma-separated form with exactly one line of column headers.
```
wdata=pd.read_csv('weather.txt')
```

The `read_csv` function stores the column headers as the column names and the rest of the data as the columns in a dataframe object.  To see the data we would type 
```
wdata 
```

The `read_csv` method has many optional arguments.  Some important ones are

* `sep=`   
     Column separator character.  Default is comma
* `header=`   
     Number of header lines.  Default is 1. 
     Specify header=None if no header 
* `names=`  
     List of column names if no header. 
     Should be provided if header=None.  If there is a header, can be used to 
     rename the columns, but then header=0 should be given.
* `skiprows=`
     Number of rows to skip before importing the data.
* `usecols=`   
     List of columns to import, if not all are to be read.

There are many other options; see the [documentation](https://pandas.pydata.org/docs/reference/api/pandas.read_csv.html).

## Excel

The [`read_excel`](https://pandas.pydata.org/docs/reference/api/pandas.read_excel.html) method can read files stored in Excel format (.xls, .xlsx, and similar).  
```
my_data=pd.read_excel('weather.xlsx')
```

It has fewer options because an Excel file includes more information about formatting.  Commonly-used ones include

* `header=`   
     Number of header lines.  Default is 1. 
     Specify header=None if no header 
* `names=`  
     List of column names if no header. 
     Should be provided if header=None.  If there is a header, can be used to 
     rename the columns, but then header=0 should be given.
* `usecols=`   
     List of columns to import, if not all are to be read
* `sheet_name=`  
     Can specify a string for a sheet name, an integer for the sheet number, counting from 0. A lists of strings or integers will request multiple sheets. Specify None for all available worksheets.  Default is 0 (first sheet only).

### Writing CSV and Excel Files

A dataframe can be written to a CSV file with `to_csv`
```python
outfile='new_data.csv'
my_new_df.to_csv(outfile)
```

If the Dataframe is to be only one Excel worksheet, a similar method `to_excel` can be used.
```python
my_new_df.to_excel("newfile.xlsx")
```
However, if more than one worksheet is needed, an ExcelWriter object must be created.  The following example is from the [documentation](https://pandas.pydata.org/docs/reference/api/pandas.ExcelWriter.html).
```python
df1 = pd.DataFrame([["AAA", "BBB"]], columns=["Spam", "Egg"])  
df2 = pd.DataFrame([["ABC", "XYZ"]], columns=["Foo", "Bar"])  
with pd.ExcelWriter("path_to_file.xlsx") as writer:
    df1.to_excel(writer, sheet_name="Sheet1")  
    df2.to_excel(writer, sheet_name="Sheet2")  
```

## HDF5 and NetCDF

HDF (Hierarchical Data Format) and NetCDF are self-describing, cross-platform, binary data formats that are widely used in a number of scientific disciplines, particularly earth sciences.
Pandas can also directly import HDF5 files, using [Pytables](https://www.pytables.org/).
```python
my_data=pd.read_hdf("weather.hdf")
```

Pandas dataframes are strictly two-dimensional objexts.  The [Xarray](https://docs.xarray.dev/en/stable/) package provides an extension of the Pandas dataframe to more than two dimensions. 
Since Pandas 0.20, Xarray is the recommended package to manage higher-dimensional data, replacing the Pandas `Panel` data structure.

Xarray can read NetCDF files directly if the [netCDF4-Python](https://github.com/Unidata/netcdf4-python) is installed.  It can also read HDF5 files via [h5netcdf](https://github.com/h5netcdf/h5netcdf).  

```python
import xarray as xr
data=xr.open_dataset("mydata.hdf", engine="h5netcdf")
ncdata=xr.open_dataset("myfile.nc")
```
HDF and NetCDF data is often in multiple files.  Xarray can merge them.
```python
alldata=xr.open_mfdataset("climate/*nc",parallel=True)
```

To write a NetCDF file:
```python
ds.to_netcdf("newdata.nc")
```

**Exercise**
Download the [weather.csv](/data/weather.csv) file.
Read the data into a dataframe.  Summarize the data.  Look at the first 20 lines.  Print the columns.  Change the names of "Data.Temperature.Avg Temp",
"Data.Temperature.Max Temp" and "Data.Temperature.Min Temp" to "Data.AvgTemp", "Data.MaxTemp", and "Data.MinTemp" respectively.  Print the mean of the average tempature.

{{< spoiler text="Example Solution, zipped Jupyter notebook" >}}
[pandas_weather_ex1.zip](/courses/python-introduction/exercises/pandas_weather_ex1.zip)
{{< /spoiler >}}


---
title: Dictionaries
toc: true
type: docs
draft: false
weight: 61

menu:
    python-introduction:
        parent: Dictionaries and Sets
---

Dictionaries are _mappings_.  Elements are accessed by a _key_ which may be of any immutable type. Tuples may used as a key, but in that case no elements of the tuple may be mutable.  Keys must be _unique_ (no duplication)

The key corresponds to a _value_.  The value may be of any type, including mutable types such as lists. The dictionary consists of all key-value pairs.  Dictionaries themselves are _mutable_ and may be of any length up to the limits of the system.  Dictionaries can be nested, i.e. the value may itself be a dictionary.

Dictionaries are denoted by curly braces `{}`.  A key-value pair is separated by a colon `:`.

## Creating Dictionaries

* Declaring an empty dictionary
  * D={}
  * Note the similarity to an empty list, but we use curly braces.
* Enumerating the key-value pairs
  * D={\"Alice\":\"2341\", \"Beth\":\"9102\", \"Cecil\":\"3258\"}
  * Another option to create a dictionary with initial entries
    * D=dict([(\"Alice\",\"2341\"),(\"Beth\",\"9102\"),(\"Cecil\",\"3258\")])
* Adding an entry
  * D[\"Dan\"]=\"5837\"
  * If the key \"Dan\" is not present, it will be created and the specified value assigned.  If it is present already in the dictionary, the value will be replaced.

### Dictionary Operations

* Length of the Dictionary
  * len(D)
  * The length is the number of key-value pairs, i.e. the number of keys.
* Delete the key-value entry
  * del D[k]
* Delete all entries
  * D.clear()
* Generate an iterator of keys 
  * keys=list(D.keys())
  * Omit the list constructor if all you need is an iterator.
    * for k in D.keys():
    * In a `for` loop the keys() can be omitted
      * for k in D:
* Generate an iterator of values
  * D.values()
* Analogous to `enumerate` for lists, we may use `items` to loop through a dictionary and obtain both the key and value
  * for k,v in D.items():
* Copy a dictionary
  * D2=D1.copy()
    * Equating D2=D1 will **not** copy the dictionary, but as for other mutable types, will just make an alias.

Quiz:

What is the difference between

```python
data=[]
data[0]=12
data[1]=4
```

and 

```python
data={}
data[0]=12
data[1]=4
```

Are both correct? Is either correct?

### More Key Handling Methods

D[key] alone results in an error if the key is not in the dictionary.  If you must attempt a lookup and do not know whether the key is present, use `get` instead.  

```python
D.get(key)
```

This returns `None` by default if the key is not found.  It can be set to return a value with an optional argument.

```python
D.get(key,0)
```

The `in` operator may also be used 

```python
if key in D
```

or

```python
if key not in D
```

depending on what you want to do.

**Exercises**

Type into Spyder or JupyterLab and run

{{< code-download file="/courses/python-introduction/exercises/dictionary_demo.py" lang="python" >}}

Use the following lists to create a dictionary `teams` where the key is taken from the first list with the value from the second list.  Use your dictionary to print the team located in Chicago.  Note that there are two teams in Los Angeles so you must make the values a list, most of which will have only one element. Hint: for neat printing, use the `join` function to create a string from a list.

{{< code-snippet  >}}
cities=["Boston","Brooklyn","New York","Philadelphia","Toronto",
        "San Francisco","Los Angeles","Los Angeles","Phoenix",
        "Sacramento","Chicago","Cleveland","Detroit","Indiana",
        "Milwaukee","Dallas","Houston","Memphis","New Orleans",
        "San Antonio","Atlanta","Charlotte","Miami","Orlando",
        "Washington","Denver","Minnesota","Oklahoma City","Portland" 
        "Salt Lake City"]
mascots=["Celtics","Nets","Knicks","76ers","Raptors","Golden State Warriors",
         "Clippers","Lakers","Suns","Kings","Bulls","Cavaliers","Pistons",
         "Pacers","Bucks","Mavericks","Rockets","Grizzlies","Hornets","Spurs",
         "Hawks","Bobcats","Heat","Magic","Wizards","Nuggets","Timberwolves",
         "Thunder","Trail Blazers","Jazz"]
{{< /code-snippet >}}

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python-introduction/exercises/create_dictionary.py" lang="python" >}}
{{< /spoiler >}}

### Resources

Documentation is [here](https://docs.python.org/3/tutorial/datastructures.html#dictionaries).


---
title: Dictionaries
toc: true
type: docs
draft: false
weight: 6
menu:
  python_introduction:
    parent: Introduction to Programming in Python
    weight: 6
---

## Dictionaries

So far the compound types we have studied have been _ordered_.  We access elements by integer _indices_ numbered from 0 to N-1, where N is the total number of elements.  Dictionaries, in contrast, are _unordered_.  Elements are accessed by a _key_ which may be of any immutable type. If a tuple is used as a key, no elements of the tuple may be mutable.  Keys must be _unique_ (no duplication)

The key corresponds to a _value_ which may be of any type, including mutable types such as lists. The dictionary consists of all key-value pairs.  Dictionaries themselves are _mutable_ and may be of any length up to the limits of the system.  Dictionaries can be nested, i.e. the value may itself be a dictionary.

Dictionaries are denoted by curly braces (){}).  A key-value pair is separated by a colon (:).

### Creating Dictionaries

* Declaring an empty dictionary
  * D={}
  * Note the similarity to an empty list, but we use curly braces.
* Enumerating the key-value pairs
  * D={'Alice':'2341', 'Beth':'9102', 'Cecil':'3258'}
  * Another option to create a dictionary with initial entries
    * D=dict([('Alice','2341'),('Beth','9102'),('Cecil','3258')])
* Adding an entry
  * D['Dan']='5837'
  * If the key 'Dan' is not present, it will be created and the specified value assigned.  If it is present already in the dictionary, the value will be replaced.

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
  * Omit the list constructor if all you need is an interator.
    * for k in D.keys():
    * In a `for` loop the keys() can be omitted:
      * for k in D:
* Generate an iterator of values
  * D.values()
* Analogous to `enumerate` for lists, we may use `items` to loop through a dictionary and obtain both the key and value
  * for k,v in D.items():

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

#### More Key Handling Methods

D[key] alone results in an error if the key is not in the dictionary.  If you must attempt a lookup and do not know whether the key is present, use `get` instead.  

```python
D.get(key)
```

This returns None by default if the key is not found.  It can be set to return a value with an optional argument.

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

<details>
<summary>Exercise 12</summary>

Type into Spyder or Jupyterlab and run

```python
capitals={"Alabama":"Montgomery"}
capitals\["Alaska"\]="Juneau"
capitals\["Arizona"\]="Little Rock"
print(capitals.keys())
print("Virginia" in capitals)
print("Arkansas" in capitals)
newstate="Connecticut"
newcapital="Hartford"
if newstate not in capitals:
    capitals[newstate]=newcapital
for key in capitals:
    print("The capital of",key,"is",capitals[key])
```

</details>

## Sets

Sets are another _unordered_ type.  No element of a set may be duplicated.  The set is mutable but all elements must be immutable.

Create the set with the set() function.  Empty parentheses will not work because those are used for an empty tuple (which will thus remain empty).

A frequent application of sets is to eliminate duplicates.

```python
L=[0,0,1,4,8,8,10]
M=list(set(L))
print(M)
```

Since sets are unordered, the order of `L` is not guaranteed to be preserved.

### Set Operations

* Add an element to set s
  * s.add(item)
* Extend with a sequence 
  * s.update(t)
* Remove an item (will fail silently if the item isn't present)
  * s.discard(item)
* Remove with an exception if the item isn't is_present 
  * s.remove(item)

The `in` operator works even though, strictly speaking, sets are not sequences.

```python
item in s
```

Sets attempt to reproduce most of the properties of mathematical sets.  

* Test for subset (a set is a subset of itself)
  * s2.issubset(s1) or s2&lt;=s1
* Test for superset (similarly, a set is its own superset)
  * s1.issuperset(s2) or s2>=s1
* Intersection
  * s1.intersection(s2) or s1&s2
* Union -- the | symbol is a pipe 
  * s1.union(s2) or s1|s2
* Symmetric difference (elements in one or the other but not both)
  * s1.symmetric_difference(s2) or s1^s2
* Set difference (elements in s1 but not in s2)
  * s1.difference(s2) or s1-s2

<details>
<summary>Exercise 13</summary>

Type at the interpeter 

```python
s=set()
s.update("California")
print(s)
```

What happened?  Lesson: be careful with strings since they are sequences.

```python
states={"Alabama","Arkansas","California","California"}
```

Initialization with curly braces has been valid since Python 2.6.  Since there are no key-value pairs it will not be construed as a dictionary.

```python
print(states)
states2=set()
states2.add("California")
states2.add("Colorado")
states2.add("Oregon")
states-states2
states&states2
states^states2
states|states2
```

</details>


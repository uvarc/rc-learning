---
title: Project Set 5
toc: true
type: docs
draft: false
weight: 220

menu:
    python_introduction:
        weight: 222
---

Classes.

## Project 19

Download the file [vabirds.csv](data/vabirds.csv). 

a) Use Pandas to read the file and capture the header information. Use the header to create of list of years.  Convert to a numpy array of floats.

b) Write a class Bird that contains the species name as a string and a numpy array for the observations.  Write a constructor that loads the values into an instance.  

c) Create an empty list.  Go through the dataframe and create a Bird instance from each row.  Append to your list of instances.

d) Add one or two methods to your Bird class that compute the maximum, minimum, mean, and median numbers of birds observed.  For the maximum and minimum also obtain the year.  (Hint: look up argmax and argmin for numpy).  You may wish to add an attribute years as well.  

e) Read the name of a bird from the command line or from user input (your choice).  Print a summary of the statistics to the console and produce a plot of observations versus years.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/proj_set_5/birds.py" lang="python" >}}
{{< /spoiler >}}

## Project 20
Write a program that reads a file with the following format:each line consists of an actor’s name followed by a semicolon, followed by a (partial) list of movies in which the actor has appeared.  Movie titles are separated by commas.  

- You can handle this file by reading each line, then splitting on the semicolon so that you carve off the performer’s name.  Append that to an actorslist.  The rest of the line is a comma-separated string.  Don’t forget to strip the end-of-line marker.   Take the movies string and split on commas to create a list. 

- Append (not extend) this list to a movies list.  This gives you a two-dimensional list (each element is itself a list).  Use your two lists to print the information in a nicer format.  
Each line should be printed as<Actor> has appeared in the following movies: <movies>  You should use your two lists to construct the above string.  Use join to rejoin the movies list into a string.  
Use either a format string or concatenation to create the message string.
- Read the name of the input file from the command line.  Use [movies.txt](/data/movies.txt) as your example input file.
   - Modify your code to define a class Actor whose attributes are
     1. name
     2. filmography 
- Write a constructor that stores these attributes as members of the instance.  The filmography will just be the movie list for this project.
- Write a `printme` method that uses code you wrote previously to print an instance in the format specified.  
That is, it will use self.name and self.filmography in the formatting. 
- Keep your Actor class definition in its own file.  (The example solution is all one file for convenience in downloading.  Split off the class.)
Modify your code so that instead of storing actor and movielist separately, you will create instances of your Actor class.  Specifically, your actors list will now be a list of instances of your Actor class.  
- As you read the file you will create a new instance using a line like
      `actors.append(Actor(something,something))`
- After creating your list of instances, use your `printme` method to reproduce the output.
- In addition to a constructor and a `printme` method, your Actor class should contain a method to return the actor name (a “getter”) and another to return the filmography as a string.  You can use these in your `printme` method.  
- Write a separate program with a main() that reads the movies.txtfile and constructs the list of Actor instances.  
- Use the list of Actor instances to create a dictionary in which the movie titles are the keys and the value corresponding to each key is a set of the cast member names.  

You will need to process through your actors list, checking whether each movie title is already in the dictionary.  
If it is not, first create an empty set, then immediately update the set with the actor name (use the “getter”).  If the movie title is already a key, update the set of the castlist.

- Write code to request from the user a movie title.  Use that movie title to print the castlist of the movie.   You can convert a set to a list with `list(movie_dict[key])` and then use `join` to print the castlist neatly.

Be sure to use appropriate functions rather than monolithic code throughout this project.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/proj_set_5/movies.py" lang="python" >}}
{{< /spoiler >}}

## Project 21

1. Write a Fraction class that implements a representation of a fraction, where each instance consists of a numerator and a denominator. Overload addition, subtraction, and multiplication for this class.  Write a dunder to format each fraction in the form
```no-highlight
5/7
```
For your first attempt it is not necessary to reduce the fraction, i.e. it is acceptable to have fractions like 6/8. Be sure to check for division by zero in your `__truediv__` method.

2. Add a reduce method that finds the [least common multiple](https://en.wikipedia.org/wiki/Least_common_multiple) to obtain the lowest common denominatorand reduce the fraction.

3. Use `NaN` to represent division by zero and `isnan` to check for it.

{{< spoiler text="Example solution" >}}
{{< code-download file="/courses/python_introduction/solns/proj_set_5/fractions.py" lang="python" >}}
{{< /spoiler >}}


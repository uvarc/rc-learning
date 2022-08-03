---
title: Object-Oriented Programming
toc: false
type: docs
draft: false
weight: 200

menu:
    python_introduction:
        weight: 200
---

Object-oriented programming (OOP) is an approach to structuring a program.  The code is organized around coherent structures that contain both attributes (variables) and behaviors (procedures).  For example, consider a program for a human resources department.  They are concerned with _employees_.  An employee has several attributes, including such things as a name, an employee ID number, a salary, and possibly other information such as home address and start date.  If we did not have something like an object, we would have to represent a group of employees with a list or similar structure.  Each element of the list would have to contain all the information about one employee, so would have to be a list itself.  If we wanted to work with a particular employee's record, we would have to determine the index, then make sure to use it consistently across any other lists or arrays we might be using.  With the information about each employee bundled into an object, we can create a variable representing an employee, and load all the pertinent data into it.  

Moreover, employees have behaviors (or behaviors can be imposed on them).  They can be given salary raises (or perhaps cuts).  They can change their status.  They can be hired, fired, or they can quit.  We could write procedures to manage those behaviors, and include them in the object so that each employee would carry out the behaviors and the relevant attributes would be properly updated for the correct employee.  We could easily introduce new attributes or behaviors without needing to rewrite code to shuffle list elements.  We can more easily check our code dealing with an "employee" since everything relevant to the employee is _encapsulated_ in the corresponding object.

Objects are another software paradigm that can be used to implement separation of concerns.

A longer discussion of OOP in Python is available [here](https://www.python-course.eu/python3_object_oriented_programming.php).

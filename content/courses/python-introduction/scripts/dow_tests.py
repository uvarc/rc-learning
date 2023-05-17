# -*- coding: utf-8 -*-
"""
This program computes the day of the week given a date in the Gregorian 
calendar.  The user inputs the day, month, and year as integers.
Author:    K. Holcomb
Changelog: Initial version 2013-05-20
Changelog:  Bugs introduced to help with teaching how to debug 2016-07-22
"""

#Tables for lookups
months=[0,3,3,6,1,4,6,2,5,0,3,5]
month_names=["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
days=["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

def DoW(day, month, year):
    
    D=day
    M=months[month-1]
    century = 100*(year//100)  #integer division
    Y=year-century
    L = Y//4  #integer division
     
    century_leap_year= century%400==0
    if century_leap_year:
        L+=1

    leap_year = (century_leap_year) or (year%4==0 and year%100 > 0)
    if leap_year and month<3:
        L -= 1
   
    if century==1400 or century==1800 or century==2200:
        C=2
    elif century==1500 or century==1900 or century==2300:
        C=0
    elif century==1600 or century==2000 or century==2400:
        C=5
    elif century==1700 or century==2100 or century==2500:
        C=4
    else:
        print("This algorithm doesn't cover the century requested")
        C=-1
    W=(C+Y+L+M+D)%7
    return days[W]
 
#Tests

print("Testing a list of dates")
testdays=[(30,5,2016),(14,2,2000),(14,2,1900),(4,7,1971),(4,7,1776)]
answerdays=[1,1,3,0,4]

for doW,date in enumerate(testdays):
    day,month,year=date
    answer=answerdays[doW]
    print("DoW {:} -> Answer {:} ".format(DoW(day,month,year),days[answer]))

print("\n\nTesting first of each month")
day = 1
month = 1
year=2016
answerdays=[5,1,2,5,0,3,5,1,4,6,2,4]
counter=0
while month < 13:
    print("DoW {:} -> Answer {:}".format(DoW(day,month,year),days[answerdays[counter]]))
    month += 1
    counter+=1

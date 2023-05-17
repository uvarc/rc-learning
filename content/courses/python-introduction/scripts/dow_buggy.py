# -*- coding: utf-8 -*-
"""
This program computes the day of the week given a date in the Gregorian 
calendar.  The user inputs the day, month, and year as integers.
Author:    K. Holcomb
Changelog: Initial version 2013-05-20
Changelog:  Bugs introduced to help with teaching how to debug 2016-07-22
"""

#Tables for lookups
months=[0,3,3,6,1,4,6,2,6,0,3,5]
month_names=["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
days=["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

def DoW(day, month, year):
    
    D=day
    M=months[month]
    century = 100*(year//100)  #integer division
    Y=year-century
    L = Y//4  #integer division
     
    century_leap_year= century%400==0
    if century_leap_year:
        L+=1
     
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
 
day  =30
month=5
year =2016
day_of_week=DoW(day,month,year)

print("The day of the week is", day_of_week)

"""inflation.py
   This program reads Consumer Price Index data and computes an approximation
   to the inflation rate
   @author: K. Holcomb
   @change: Modifications for Introduction to Python 20220202
   @change: Initial code 20130210
"""
import numpy as np
import matplotlib.pyplot as plt
import csv
import sys
import math

def prices(years,cpi,year1,year2):
    y1=math.nan
    y2=math.nan
    for y in range(len(years)):
        if years[y]==year1: y1=y
        if years[y]==year2: y2=y
    if math.isnan(y1) or math.isnan(y2):
        return None
    else:
       return cpi[y2]/cpi[y1]

def inflate(years,cpi):
    inflation=np.zeros(nyears-1)
    for n in range(nyears-1):
        inflation[n]=(cpi[n+1]-cpi[n])/12.
    return inflation

if len(sys.argv) < 2:
   sys.exit("No data file specified")


compare=int(float(input("Do you want to compare prices, 1 yes, 0 no:")))
if ( compare ):
    year1=int(float(input("Please enter the reference year:")))
    year2=int(float(input("Please enter the second year:")))
    amount=int(float(input("Please enter the amount to compare:")))

try:
   infile=sys.argv[1]
except (ValueError,TypeError):
   sys.exit("Invalid file name")

years_list=[]
cpi_list=[]

with open(infile,"r") as fin:
   #Skip header.
   fin.readline()
   for line in fin:
       (yr,factor)=line.rstrip("\r\n").split(",")
       years_list.append(float(yr))
       cpi_list.append(float(factor))

years =np.array(years_list)
cpi   =np.array(cpi_list)

nyears=np.size(years)

if ( compare ):
   ratio=prices(years,cpi,year1,year2)
   if ratio is None:
       print("One or more year specified is outside the data range")
       sys.exit()
   else:
       print("An item costing {:.2f} in {:} is about {:.2f} in {:}".format(amount,year1,ratio*amount,year2))

inflation=inflate(years,cpi)

fig, axs = plt.subplots(2, 1)
axs[0].plot(years, cpi)
axs[0].set_ylabel('CPI')

axs[1].plot(years[:-1], inflation)
axs[1].set_xlabel('year')
axs[1].set_ylabel('Inflation')

plt.show()



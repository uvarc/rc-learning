"""This program reads body-fat data, computes BMI (with appropriate units
   conversions), cleans the data, and plots the data before and after cleaning.
   Author:    K. Holcomb
   Changelog: Initial version 20130225
"""
import sys
import numpy as np
import matplotlib.pylab as plt
from scipy import stats

#Global constants
in2m =0.0254
oz2kg=0.0283495

def imp_ht_to_metric(ft,inch):
   """Converts imperial length units to meters"""
   return (ft*12.0+inch)*in2m

def imp_wt_to_metric(lb,oz):
   """Converts pounds and ounces to kg"""
   return (lb*16.0+oz)*oz2kg

def BMI(ht,wt):
   """Returns BMI from ht in meters and wt in kg"""
   return wt/ht**2

def reject_outliers(array):
   """Returns a mask of values of array below the outlier value."""
   """Kind of a dumb method.  Should use Chauvenet criterion at least."""
   cutoff=np.percentile(array,99)
   return array<cutoff

def main():
   #Get name of file from the command line
   if len(sys.argv) < 2:
      print("No file specified")
      sys.exit()

   infile=sys.argv[1]
   try:
       (bodyfat,weight,height)=np.loadtxt(infile,usecols=(0,2,3), \
                                           unpack=True,skiprows=1,delimiter=',')
   except IOError:
      print("Cannot read file")
      sys.exit()

   bmi=BMI(imp_ht_to_metric(0.,height),(imp_wt_to_metric(weight,0.)))

   plt.figure()
   plt.scatter(bodyfat,bmi)

   valid=reject_outliers(bmi)
   bodyfat=bodyfat[valid]
   bmi=bmi[valid]
   
   slope, intercept, r_value, p_value, std_err = stats.linregress(bodyfat,bmi)
   linear_fit=bodyfat*slope+intercept

   print("Mean BMI is %.2f, median is %.2f, and standard deviation is %.2f" % \
          (bmi.mean(),np.median(bmi),bmi.std()))

   print("The least-squares regression gives a r-value of %.2f" % r_value)

   plt.figure()
   plt.scatter(bodyfat,bmi)
   plt.plot(bodyfat,linear_fit)

   plt.figure()
   #Histogram using WHO BMI categories with somewhat arbitrary upper 
   #and lower bounds.
   bin_bounds=[5.,16.,18.5,25.,30.,35.,40.,60.]
   plt.hist(bmi,bins=bin_bounds)

   plt.show()

if __name__=='__main__':
   main()

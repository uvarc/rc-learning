---
date : "2019-06-23T08:37:46-05:00"
title : "Statistical Methods with MATLAB"
type: article
toc : true
---

# Overview

MATLAB is an integrated technical computing environment from the MathWorks that combines array-based numeric computation, advanced graphics and visualization, and a high-level programming language. Separately licensed toolboxes provide additional domain-specific functionality.

**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats" target="_blank"><font size="3">Matlab Academy: Statistical Methods with Matlab<font/></a>**

**<a href="https://www.mathworks.com/help/stats/index.html" target="_blank"><font size="3">Documentation: Statistics and Machine Learning Toolbox (help page)<font/></a>**

**<a href="https://www.mathworks.com/products/statistics.html" target="_blank"><font size="3">Documentation: Statistics and Machine Learning Toolbox (product page)<font/></a>**
<br/>
<br/>


## Course Overview
<p>This video from the MATLAB academy provides ample introductory information to follow along on this tutorial.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=1&lesson=1&section=1" target="_blank"><font size="3">Video: Statistical Methods with Matlab<font/></a>**

## Exploring Data
<p>How do we know what our data looks like? This section aims to show you how to explore your data and get to know what kind of information you are dealing with.<p>
### Visualizing Data Sets
<p>The following section displays appropriate uses of histograms, boxplots, and scatter plots as a way to quantitatively assess your data before you continue your analysis in MATLAB.</p>
{{< figure src="exploreData1.png"  >}}

{{< figure src="exploreData2.png"  >}}

**<a href="https://www.mathworks.com/help/matlab/ref/matlab.graphics.chart.primitive.histogram.html" target="_blank"><font size="3">Documentation: &nbsp; <u>histogram</u> <font/></a>**  &nbsp; &nbsp; **<a href="https://www.mathworks.com/help/stats/boxplot.html" target="_blank"> <strong>  <font size="3">  <u>boxplot</u> </strong><font/> </a>**   &nbsp; &nbsp;   **<a href="https://www.mathworks.com/help/matlab/ref/scatter.html" target="_blank"> <strong>  <font size="3"><u>scatter</u> </strong><font/> </a>**

<br/>
### Measures of Centrality and Spread
<p>This section explores statistical measures such as mean, median, mode, variance, and interquartile range to summarize data.<p>
{{< figure src="centrality1.png"  >}}
{{< figure src="centrality2.png"  >}}
{{< figure src="centrality3.png"  >}}

**<a href="https://www.mathworks.com/help/matlab/ref/mean.html" target="_blank"> <strong>  <font size="3">Documentation: &nbsp; <u>mean</u> </strong> <font/></a>** &nbsp; &nbsp; **<a href="https://www.mathworks.com/help/matlab/ref/median.html" target="_blank"> <strong>  <font size="3"><u>median</u> </strong> <font/></a>** &nbsp; &nbsp;
**<a href="https://www.mathworks.com/help/matlab/ref/mode.html" target="_blank"><font size="3"> <u>mode</u><font/> </a>** &nbsp; &nbsp; **<a href="https://www.mathworks.com/help/stats/trimmean.html" target="_blank"> <strong>  <font size="3"><u>trimmean</u> </strong> <font/></a>**

<br/>

{{< figure src="spread1.png"  >}}
{{< figure src="spread2.png"  >}}
{{< figure src="spread3.png"  >}}

**<a href="https://www.mathworks.com/help/matlab/ref/std.html" target="_blank">  <font size="3">Documentation: &nbsp; <u>std</u> <font/> </a>** &nbsp; &nbsp;  **<a href="https://www.mathworks.com/help/stats/prob.normaldistribution.iqr.html" target="_blank"> <font size="3"> <u>iqr</u> <font/></a>**
**<a href="https://www.mathworks.com/help/stats/range.html" target="_blank"><font size="3"> &nbsp; &nbsp; <u>range</u><font/> </a>** &nbsp; &nbsp; **<a href="https://www.mathworks.com/help/matlab/ref/var.html" target="_blank"> <font size="3"><strong> <u>var</u> </strong> <font/></a>**


<br/>



### Distributions
<p>This section covers different types probability distributions and how to visualize and analyze them in MATLAB.<p>
{{< figure src="distribution3.png"  >}}
{{< figure src="spread4.png"  >}}

<br/>
{{< figure src="distribution1.png"  >}}
{{< figure src="distribution2.png"  >}}

**<a href="https://www.mathworks.com/help/stats/normpdf.html" target="_blank"><font size="3">Documentation: &nbsp; <u>normpdf</u> <font/> </a>** &nbsp; &nbsp; **<a href="https://www.mathworks.com/help/stats/unifpdf.html" target="_blank"> <strong><font size="3"> <u>unifpdf</u><font/> </strong> </a>** &nbsp; &nbsp; **<a href="https://www.mathworks.com/help/matlab/ref/randn.html" target="_blank"><font size="3"> <u>randn</u> <font/></a>** &nbsp; &nbsp; **<a href="https://www.mathworks.com/help/matlab/ref/rand.html" target="_blank"> <strong> <font size="3"><u>rand</u><font/> </strong> </a>**

### Summary
<p>These figures summarize the key concepts and visualizations discussed in the previous sections.<p>
{{< figure src="summary1.png"  >}}


<br/>
{{< figure src="summary2.png"  >}}


<br/>

{{< figure src="summary3.png"  >}}



<br/>

## Fitting a Curve to Data

### Linear Regression
<p>This section demonstrates how to fit linear models to data and interpret the results.<p>
{{< figure src="regression1.png"  >}}
{{< figure src="regression2.png"  >}}
{{< figure src="regression4.png"  >}}
{{< figure src="regression3.png"  >}}

**<a href="https://www.mathworks.com/help/curvefit/fit.html" target="_blank"> <font size="3"> Documentation: &nbsp; <u>fit</u> <font/></a>**

 <br/>

### Evaluating Goodness of Fit
<p>Here we evaluate how well linear models fit the data using various statistical metrics, including residual analysis.<p>
{{< figure src="regression5.png"  >}}
{{< figure src="regression6.png"  >}}
{{< figure src="regression7.png"  >}}

<br/>

### Nonlinear Regression
<p>This section shows how to fit nonlinear models to data and compare them to linear models.<p>
{{< figure src="regression8.png"  >}}
{{< figure src="regression9.png"  >}}
{{< figure src="regression10.png"  >}}

### Summary
<p>These figures summarize the results and insights gained from regression analyses and how to fit a curve to data.<p>
{{< figure src="summary4.png"  >}}
{{< figure src="summary5.png"  >}}
{{< figure src="summary6.png"  >}}

## Interpolating Data
<p>This section disucusses how we can create new data as an estimate based off our current data, and implement it in MATLAB<p>
### Linear Interpolation
<p>Linear interpolation techniques are used to estimate values between known data points.<p>
{{< figure src="interp1.png"  >}}
{{< figure src="interp2.png"  >}}
{{< figure src="interp3.png"  >}}
{{< figure src="interp4.png"  >}}
{{< figure src="interp5.png"  >}}
{{< figure src="interp6.png"  >}}

### Nonlinear Interpolation
<p>This section demonstrates nonlinear interpolation methods for more complex datasets.<p>
{{< figure src="interp7.png"  >}}
{{< figure src="interp8.png"  >}}
{{< figure src="interp9.png"  >}}

**<a href="https://www.mathworks.com/help/matlab/ref/interp1.html" target="_blank"> <font size="3"> Documentation: &nbsp; <u>interp1</u> <font/> </a>**

 <br/>

### Summary
<p>Figures here summarize the interpolation methods and results and how they can be applied using MATLAB.<p>
{{< figure src="summary7.png"  >}}
{{< figure src="summary8.png"  >}}

## Additional Resources
<p>This section provides links to additional MATLAB tools, tutorials, and support resources.<p>
{{< figure src="additionalRes1.png"  >}}

<br/>
**<a href="https://www.mathworks.com/matlabcentral/" target="_blank"><u>MATLAB Central</u></a>** &nbsp; &nbsp; **<a href="https://www.mathworks.com/support.html" target="_blank"> <strong><font size="3"> <u>MathWorks Support</u><font/> </strong> </a>** 

<br/>

## Exercises
<p>The resource computing team has kindly accumulated exercises to practice on, based on your MATLAB needs. All exercises are provided through the MATLAB Help Center <p>
### Visualizing Data sets
<p>Practice exercises to reinforce techniques for visualizing height and weight data.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=2&lesson=1&section=2" target="_blank">Exercise: Visualize Height and Weight Data</a>**

### Measure of Centrality and Spread
<p>Exercises to calculate and interpret mean, median, standard deviation, and other centrality measures.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=2&lesson=2&section=2" target="_blank">Exercise: Find the Mean and Median</a>**

**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=2&lesson=2&section=4" target="_blank">Exercise: Find the Standard Deviation and IQR</a>**

### Distributions
<p>Exercises to explore probability distributions and generate data using MATLAB functions.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=2&lesson=3&section=3" target="_blank">Exercise: Fit and Plot a Normal Distribution</a>**

**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=2&lesson=3&section=4" target="_blank">Exercise: Generating Random Numbers</a>**

### Review: Exploring Data
<p>Exercises reviewing data visualization and analysis skills from previous sections.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=2&lesson=4&section=2" target="_blank">Exercise: Earthquakes</a>**

### Linear Regression
<p>Exercises to practice fitting lines and polynomials to datasets.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=3&lesson=1&section=3" target="_blank">Exercise: Fit a Line to Data</a>**

**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=3&lesson=1&section=5" target="_blank">Exercise: Fit a Polynomial to Data</a>**

### Evaluating the Goodness of Fit
<p>Exercises to evaluate and improve the fit of models to your data.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=3&lesson=2&section=2" target="_blank">Exercise: Evaluate and Improve the Fit</a>**


<br/>
<br/>
### Nonlinear Regression
<p>Exercises focused on fitting nonlinear models to data and interpreting results.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=3&lesson=3&section=2" target="_blank">Exercise: Fit a Nonlinear Model</a>**

### Review: Fitting a Curve to Data
<p>Exercises reviewing linear and nonlinear regression techniques applied to sample datasets.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=3&lesson=4&section=2" target="_blank">Exercise: Temperature Fluctuations</a>**

### Linear Interpolation
<p>Exercises to practice estimating values using linear interpolation.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=4&lesson=1&section=2" target="_blank">Exercise: Fill in Missing Data</a>**

**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=4&lesson=1&section=3" target="_blank">Exercise: Resample Data</a>**

### Nonlinear Interpolation
<p>Exercises applying nonlinear interpolation methods to datasets.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=4&lesson=2&section=2" target="_blank">Exercise: Resample Data with Different Interpolation Methods</a>**

### Review: Interpolation
<p>Exercises reviewing both linear and nonlinear interpolation techniques.<p>
**<a href="https://matlabacademy.mathworks.com/R2019b/portal.html?course=stats#chapter=4&lesson=3&section=2" target="_blank">Exercise: Stock Prices</a>**

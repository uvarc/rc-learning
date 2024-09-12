---
title: 'Introduction to R'
summary: "This short course introduces the basics of the R statistical computing language and the RStudio environment."
type: article
toc: true
tags: ["Programming","R"]
categories: ["Programming","R"]
date: "2020-11-17T00:00:00Z"
---

<p class="lead"> We will introduce the R statistical computing environment as well as RStudio. We will cover very basic functionality, including variables, functions, importing data, and the fundamentals of inspecting data frame objects. The course assumes little to no experience with statistical computing or R.</p>

```{r, echo=FALSE, message=FALSE, eval=TRUE, warning=FALSE}
library(knitr)
opts_chunk$set(message=FALSE, warning=FALSE, eval=TRUE, echo=TRUE, results="show", cache=FALSE)
opts_knit$set(root.dir = "../../static/data/")
options(digits=3)
options(max.print=200)
.ex <- 1 # Track ex numbers w/ hidden var. Increment each ex: `r .ex``r .ex=.ex+1`
```

## RStudio

Let's start by learning about RStudio. **R** is the underlying statistical computing environment, but using R alone is no fun. **RStudio** is a graphical integrated development environment that makes using R much easier.

- Panes in RStudio. There are four panes, and their orientation is configurable under "Tools -- Global Options." You don't have to do it this way, but I usually set up my window to have:
    - Editor in the top left
    - Console top right
    - Environment/history on the bottom left
    - Plots/help on the bottom right.  
- Projects: first, start a new project in a new folder somewhere easy to remember. When we start reading in data it'll be important that the _code and the data are in the same place._ Creating a project creates a Rproj file that opens R running _in that folder_. This way, when you want to read in dataset _whatever.txt_, you just tell it the filename rather than a full path. This is critical for reproducibility, and we'll talk about that more later.
- Code that you type into the console is code that R executes. From here forward we will use the editor window to write a script that we can save to a file and run it again whenever we want to. We usually give it a `.R` extension, but it's just a plain text file. If you want to send commands from your editor to the console, use `CMD`+`Enter` (`Ctrl`+`Enter` on Windows).
- Anything after a `#` sign is a comment. Use them liberally to *comment your code*.

## Basic operations

R can be used as a glorified calculator. Try typing this in directly into the console. Make sure you're typing into the editor, not the console, and save your script. Use the run button, or press `CMD`+`Enter` (`Ctrl`+`Enter` on Windows).

```{r calculator}
2+2
5*4
2^3
6/7
```

R Knows order of operations and scientific notation.

```{r calculator2}
2+3*4/(5+3)*15/2^2+3*4^2
5e4
```

However, to do useful and interesting things, we need to assign _values_ to _objects_. To create objects, we need to give it a name followed by the assignment operator `<-` and the value we want to give it:

```{r assignment}
weight_kg <- 55
```

`<-` is the assignment operator. It assigns values on the right to objects on the left, it is like an arrow that points from the value to the object. Mostly similar to `=` but not always. Learn to use `<-` as it is good programming practice. Using `=` in place of `<-` can lead to issues down the line. The keyboard shortcut for inserting the `<-` operator is `Alt-dash`.

Objects can be given any name such as `x`, `current_temperature`, or `subject_id`. You want your object names to be explicit and not too long. They cannot start with a number (`2x` is not valid but `x2` is). R is case sensitive (e.g., `weight_kg` is different from `Weight_kg`). There are some names that cannot be used because they represent the names of fundamental functions in R (e.g., `if`, `else`, `for`, see [here](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Reserved.html) for a complete list). In general, even if it's allowed, it's best to not use other function names, which we'll get into shortly (e.g., `c`, `T`, `mean`, `data`, `df`, `weights`). In doubt check the help to see if the name is already in use. It's also best to avoid dots (`.`) within a variable name as in `my.dataset`. It is also recommended to use nouns for variable names, and verbs for function names.

When assigning a value to an object, R does not print anything. You can force to print the value by typing the name:

```{r printAssignment}
weight_kg
```

Now that R has `weight_kg` in memory, we can do arithmetic with it. For instance, we may want to convert this weight in pounds (weight in pounds is 2.2 times the weight in kg).

```{r modAssignment}
2.2 * weight_kg
```

We can also change a variable's value by assigning it a new one:

```{r newAssignment}
weight_kg <- 57.5
2.2 * weight_kg
```

This means that assigning a value to one variable does not change the values of other variables. For example, let's store the animal's weight in pounds in a variable.

```{r calculationWithVar}
weight_lb <- 2.2 * weight_kg
```

and then change `weight_kg` to 100.

```{r modAssignment2}
weight_kg <- 100
```

What do you think is the current content of the object `weight_lb`? 126.5 or 220?

You can see what objects (variables) are stored by viewing the Environment tab in Rstudio. You can also use the `ls()` function. You can remove objects (variables) with the `rm()` function. You can do this one at a time or remove several objects at once. You can also use the little broom button in your environment pane to remove everything from your environment.

```{r rm, eval=FALSE}
ls()
rm(weight_lb, weight_kg)
ls()
weight_lb # oops! you should get an error because weight_lb no longer exists!
```

----

**EXERCISE `r .ex``r .ex=.ex+1`**

What are the values after each statement in the following?

```{r ex1}
mass <- 50              # mass?
age  <- 30              # age?
mass <- mass * 2        # mass?
age  <- age - 10        # age?
mass_index <- mass/age  # massIndex?
```

----

## Functions

R has built-in functions.

```{r fns}
# Notice that this is a comment.
# Anything behind a # is "commented out" and is not run.
sqrt(144)
log(1000)
```

Get help by typing a question mark in front of the function's name, or `help(functionname)`:

```
help(log)
?log
```

Note syntax highlighting when typing this into the editor. Also note how we pass *arguments* to functions. The `base=` part inside the parentheses is called an argument, and most functions use arguments. Arguments modify the behavior of the function. Functions some input (e.g., some data, an object) and other options to change what the function will return, or how to treat the data provided. Finally, see how you can *nest* one function inside another (here taking the square root of the log-base-10 of 1000).

```{r log}
log(1000)
log(1000, base=10)
log(1000, 10)
sqrt(log(1000, base=10))
```

----

**EXERCISE `r .ex``r .ex=.ex+1`**

See `?abs` and calculate the square root of the log-base-10 of the absolute value of `-4*(2550-50)`. Answer should be `2`.

----

## Data Frames

There are _lots_ of different basic data structures in R. If you take any kind of longer introduction to R you'll probably learn about arrays, lists, matrices, etc. We are going to skip straight to the data structure you'll probably use most -- the **data frame**. We use data frames to store heterogeneous tabular data in R: tabular, meaning that individuals or observations are typically represented in rows, while variables or features are represented as columns; heterogeneous, meaning that columns/features/variables can be different classes (on variable, e.g. age, can be numeric, while another, e.g., cause of death, can be text). 

**Recommended reading:** Review the [_Introduction_ (10.1)](http://r4ds.had.co.nz/tibbles.html#introduction-4) and [_Tibbles vs. data.frame_ (10.3)](http://r4ds.had.co.nz/tibbles.html#tibbles-vs.data.frame) sections of the [**_R for Data Science_ book**](http://r4ds.had.co.nz/tibbles.html). We will initially be using the `read_*` functions from the [**readr** package](http://readr.tidyverse.org/). These functions load data into a _tibble_ instead of R's traditional data.frame. Tibbles are data frames, but they tweak some older behaviors to make life a little easier. These sections explain the few key small differences between traditional data.frames and tibbles. 

## Our data

The data we're going to look at is cleaned up version of a gene expression dataset from [Brauer et al. Coordination of Growth Rate, Cell Cycle, Stress Response, and Metabolic Activity in Yeast (2008) _Mol Biol Cell_ 19:352-367](http://www.ncbi.nlm.nih.gov/pubmed/17959824). This data is from a gene expression microarray, and in this paper the authors are examining the relationship between growth rate and gene expression in yeast cultures limited by one of six different nutrients (glucose, leucine, ammonium, sulfate, phosphate, uracil). If you give yeast a rich media loaded with nutrients except restrict the supply of a _single_ nutrient, you can control the growth rate to any rate you choose. By starving yeast of specific nutrients you can find genes that: 

1. **Raise or lower their expression in response to growth rate**. Growth-rate dependent expression patterns can tell us a lot about cell cycle control, and how the cell responds to stress. The authors found that expression of >25% of all yeast genes is linearly correlated with growth rate, independent of the limiting nutrient. They also found that the subset of negatively growth-correlated genes is enriched for peroxisomal functions, and positively correlated genes mainly encode ribosomal functions. 
2. **Respond differently when different nutrients are being limited**. If you see particular genes that respond very differently when a nutrient is sharply restricted, these genes might be involved in the transport or metabolism of that specific nutrient.

You can download the cleaned up version of the data at [the link above](data.html). The file is called [**brauer2007_tidy.csv**](data/brauer2007_tidy.csv). Later on we'll actually start with the original raw data (minimally processed) and manipulate it so that we can make it more amenable for analysis. 

## Reading in data

### dplyr and readr

There are some built-in functions for reading in data in text files. These functions are _read-dot-something_ -- for example, `read.csv()` reads in comma-delimited text data; `read.delim()` reads in tab-delimited text, etc. We're going to read in data a little differently here using the [readr](http://readr.tidyverse.org/) package. When you load the readr package, you'll have access to very similar looking functions, named _read-underscore-something_ -- e.g., `read_csv()`. You have to have the readr package installed to access these functions. Compared to the base functions, they're _much_ faster, they're good at guessing the types of data in the columns, they don't do some of the other silly things that the base functions do. We're going to use another package later on called [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), and if you have the dplyr package loaded as well, and you read in the data with readr, the data will display nicely. 

First let's load those packages.

```{r loadpkgs}
library(readr)
library(dplyr)
```

If you see a warning that looks like this: `Error in library(packageName) : there is no package called 'packageName'`, then you don't have the package installed correctly.

### `read_csv()`

Now, let's actually load the data. You can get help for the import function with `?read_csv`. When we load data we assign it to a variable just like any other, and we can choose a name for that data. Since we're going to be referring to this data a lot, let's give it a short easy name to type. I'm going to call it `ydat`. Once we've loaded it we can type the name of the object itself (`ydat`) to see it printed to the screen. 

```{r loaddata}
ydat <- read_csv(file="brauer2007_tidy.csv")
ydat
```

Take a look at that output. The nice thing about loading dplyr and reading in data with readr is that data frames are displayed in a much more friendly way. This dataset has nearly 200,000 rows and 7 columns. When you import data this way and try to display the object in the console, instead of trying to display all 200,000 rows, you'll only see about 10 by default. Also, if you have so many columns that the data would wrap off the edge of your screen, those columns will not be displayed, but you'll see at the bottom of the output which, if any, columns were hidden from view. If you want to see the whole dataset, there are two ways to do this. First, you can click on the name of the data.frame in the **Environment** panel in RStudio. Or you could use the `View()` function (_with a capital V_).

```{r view, eval=FALSE}
View(ydat)
```

## Inspecting data.frame objects

### Built-in functions

There are several built-in functions that are useful for working with data frames.

* Content:
    * `head()`: shows the first few rows
    * `tail()`: shows the last few rows
* Size:
    * `dim()`: returns a 2-element vector with the number of rows in the first element, and the number of columns as the second element (the dimensions of the object)
    * `nrow()`: returns the number of rows
    * `ncol()`: returns the number of columns
* Summary:
    * `colnames()` (or just `names()`): returns the column names
    * `str()`: structure of the object and information about the class, length and content of each column
    * `summary()`: works differently depending on what kind of object you pass to it. Passing a data frame to the `summary()` function prints out useful summary statistics about numeric column (min, max, median, mean, etc.)

```{r data_frame_functions}
head(ydat)
tail(ydat)
dim(ydat)
names(ydat)
str(ydat)
summary(ydat)
```

### Other packages

The `glimpse()` function is available once you load the **dplyr** library, and it's like `str()` but its display is a little better.

```{r}
glimpse(ydat)
```

The **skimr** package has a nice function, skim, that provides summary statistics the user can skim quickly to understand your data. You can install it with `install.packages("skimr")` if you don't have it already.

```{r, eval = FALSE}
library(skimr)
skim(ydat)
```


## Accessing variables & subsetting data frames

We can access individual variables within a data frame using the `$` operator, e.g., `mydataframe$specificVariable`. Let's print out all the gene names in the data. Then let's calculate the average expression across all conditions, all genes (using the built-in `mean()` function).

```{r}
# display all gene symbols
ydat$symbol

#mean expression
mean(ydat$expression)
```

Now that's not too interesting. This is the average gene expression across all genes, across all conditions. The data is actually scaled/centered around zero:

```{r histogram_expression_values, echo=F}
library(ggplot2)
ggplot(ydat, aes(expression)) + 
  geom_histogram(bins=100)+ 
  xlab("Expression") + 
  ggtitle("Histogram of expression values") +
  theme_bw()
```

We might be interested in the average expression of genes with a particular biological function, and how that changes over different growth rates restricted by particular nutrients. This is the kind of thing we're going to do in the next section.

----

**EXERCISE `r .ex``r .ex=.ex+1`**

1. What's the standard deviation expression (hint: get help on the `sd` function with `?sd`).
1. What's the range of rate represented in the data? (hint: `range()`).

----

## BONUS: Preview to advanced manipulation

What if we wanted to show the mean expression, standard deviation, and correlation between growth rate and expression, separately for each limiting nutrient, separately for each gene, for all genes involved in the leucine biosynthesis pathway?

```{r, results='hide'}
ydat %>% 
  filter(bp=="leucine biosynthesis") %>% 
  group_by(nutrient, symbol) %>% 
  summarize(mean=mean(expression), sd=sd(expression), r=cor(rate, expression))
```

```{r, echo=FALSE}
ydat %>% 
  filter(bp=="leucine biosynthesis") %>% 
  group_by(nutrient, symbol) %>% 
  summarize(mean=mean(expression), sd=sd(expression), r=cor(rate, expression)) %>% 
  mutate_each(funs(round(., 2)), mean:r)
```

```{r makedata, include=F, eval=F}
# Here's how I made the data
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

# read original data
(ydat <- read_delim("data/brauer2007_orig.txt", delim="\t"))
# change variable WS||WS to ::
(ydat <- ydat %>% mutate(NAME=gsub("\\s*\\|\\|\\s*", "::", NAME, perl=TRUE)))
# separate on :: into gene symbol, go-bp, go-mf, syssymbol, and somenumber
(ydat <- ydat %>% separate(NAME, c("symbol", "bp", "mf", "systematic_symbol", "somenumber"), sep = "::"))
# replace missing with NA
(ydat[ydat==""] <- NA)
# remove things where syssymbol is missing
(ydat <- ydat %>% filter(!is.na(systematic_symbol) & systematic_symbol!=""))
# write out go terms to join to later
ydat %>% select(systematic_symbol, bp, mf) %>% distinct %>% write_csv("data/brauer2007_syssymbol2go.csv")
# unite the symbol column again
(ydat <- ydat %>% unite(NAME, symbol, systematic_symbol, somenumber, sep="::") %>% select(-bp, -mf))
# write out the messy data that you'll use for data cleaning class
ydat %>% write_csv("data/brauer2007_messy.csv")
rm(ydat)

(ydat <- read_csv("data/brauer2007_messy.csv"))
(syssymbol2go <- read_csv("data/brauer2007_syssymbol2go.csv"))

ydat <- ydat %>% 
  separate(NAME, c("symbol", "systematic_symbol", "somenumber"), sep = "::") %>%
  select(-somenumber, -GID, -YORF, -GWEIGHT) %>%
  gather(sample, expression, G0.05:U0.3) %>%
  separate(sample, c("nutrient", "rate"), sep = 1) %>%
  filter(!is.na(expression), systematic_symbol != "") %>% 
  inner_join(syssymbol2go, by="systematic_symbol") %>% 
  mutate(nutrient = plyr::mapvalues(nutrient, 
                                    from=c("G", "L", "P", "S", "N", "U"), 
                                    to=c("Glucose", "Leucine", "Phosphate", "Sulfate", "Ammonium", "Uracil")))

ydat %>% write_csv("data/brauer2007_tidy.csv")
```


```{r, include=FALSE, eval = TRUE}
# find packages attached during for this Rmd session and their versions
package <- installed.packages()[names(sessionInfo()$otherPkgs), "Package"]
version <- installed.packages()[names(sessionInfo()$otherPkgs), "Version"]

thesepkgs <- data.frame(package, version)

# if no non-base packages installed skip
if(nrow(thesepkgs) == 0) invisible()

# find package csv file
fp <- "../../packages.csv"

# if it exists find read in contents ... combine with attached pkgs and dedupe
if(file.exists(fp)) {
  
  pkgs <- read.csv(fp)
  
  pkgs <- 
    rbind(pkgs, thesepkgs)
  
  pkgs <- pkgs[!duplicated(pkgs),]
  
} else{
  
  pkgs <- thesepkgs
  
}

# write out new package.csv file
write.table(pkgs, 
          file = fp,
          sep = ",",
          row.names = FALSE)
```

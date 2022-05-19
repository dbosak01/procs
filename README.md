<!-- badges: start -->

[![procs version](https://www.r-pkg.org/badges/version/procs)](https://cran.r-project.org/package=procs)
[![procs lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://cran.r-project.org/package=procs)
[![procs downloads](https://cranlogs.r-pkg.org/badges/procs)](https://cran.r-project.org/package=procs)
[![procs total downloads](https://cranlogs.r-pkg.org/badges/grand-total/procs)](https://cran.r-project.org/package=procs)
[![R-CMD-check](https://github.com/dbosak01/procs/workflows/R-CMD-check/badge.svg)](https://github.com/dbosak01/procs/actions)
[![Codecov test coverage](https://codecov.io/gh/dbosak01/procs/branch/master/graph/badge.svg)](https://app.codecov.io/gh/dbosak01/procs?branch=master)

<!-- badges: end -->

# Introduction to **procs**
<!-- <img src='man/images/procs.png' align="left" height="138" style="margin-right:10px"/>-->

There are already several logging packages for R.  Why create another one? 

Because the other logging packages all have something in common: they were built
for *R package developers*.  

What is different about the **procs** package is
that it is built for *normal R users*: statisticians, analysts, researchers, 
students, teachers, business people, etc.

The **procs** package is for those people who just need a written record of their
program execution.  It is designed to be as simple as possible, yet still
produce a useful and complete log.  

There are only three steps to creating a **procs** log:

1. Open the log
2. Print to the log
3. Close the log

Now this a logging system that anyone can use!  


### Installation

The easiest way to install the **procs** package is to run the following 
command from your R console:

    install.packages("procs")


Then put the following line at the top of your script:

    library(procs)
    
For examples and usage 
information, please visit the **procs** documentation site 
[here](https://procs.r-sassy.org/articles/procs.html)

### Getting Help

If you need help, the first place 
to turn to is the [procs](https://procs.r-sassy.org) web site.  

If you want to look at the code for the **procs** package, visit the
github page [here](https://github.com/dbosak01/procs).

If you encounter a bug or have a feature request, please submit an issue 
[here](https://github.com/dbosak01/procs/issues).


### See Also

The **procs** package is part of the **sassy** meta-package. 
The **sassy** meta-package includes several packages that help make R
easier for SAS® programmers.  You can read more about the **sassy** package
[here](https://sassy.r-sassy.org).

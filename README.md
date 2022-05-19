<!-- badges: start -->

[![procs version](https://www.r-pkg.org/badges/version/procs)](https://cran.r-project.org/package=procs)
[![procs lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://cran.r-project.org/package=procs)
[![procs downloads](https://cranlogs.r-pkg.org/badges/procs)](https://cran.r-project.org/package=procs)
[![procs total downloads](https://cranlogs.r-pkg.org/badges/grand-total/procs)](https://cran.r-project.org/package=procs)
<!--[![R-CMD-check](https://github.com/dbosak01/procs/workflows/R-CMD-check/badge.svg)](https://github.com/dbosak01/procs/actions)-->
[![Codecov test coverage](https://codecov.io/gh/dbosak01/procs/branch/master/graph/badge.svg)](https://app.codecov.io/gh/dbosak01/procs?branch=master)

<!-- badges: end -->

# Introduction to **procs**
<!-- <img src='man/images/procs.png' align="left" height="138" style="margin-right:10px"/>-->

R provides a wide range of statistical packages and functions.  However,
these statistical functions frequently return a fragmented output.  The user
is often left to compile R statistical results into a readable report themselves.

SAS software, on the other hand, provides the same statistical results, 
but with rich outputs.  These outputs include multiple datasets
and complete statistical reports.  

The purpose of the **procs** package is to recreate some SAS statistical
procedures in R.  The first version of the package will attempt to recreate
SAS 'proc freq'.  Subsequent versions of the package will add additional,
commonly used procedures like 'proc means' and 'proc tabulate'.

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

# procs 1.0.5

* Fixed bug on `proc_means()` when ordered factor used as class.
* Updates to documentation.
* Fixed bug on `proc_transpose()` when factor used on by parameter.
* Added "dist" option to `proc_ttest()`.

# procs 1.0.4

* Added "as.character" parameter to `proc_sort()` to cast any factors in the
by variables to character.  This parameter is the counter-point to
the "as.factor" parameter on the `value()` function in the **fmtr** package.
* Added an "options" parameter to `proc_transpose()`.  For now accepts only
one option "noname" which will drop the NAME variable from the output dataset.
* Added `proc_ttest()` function.

# procs 1.0.3

* Fixed 'class' parameter on 'proc_freq()'.
* Fixed sparse output when variables are defined as factors.
* Put interactive() condition on viewer functions so as not to produce 
unnecessary temp files in batch sessions.
* Added 'dupkey' option to `proc_sort()`.
* Added 'completetypes' option to proc means.
* Released to CRAN.

# procs 0.0.9008

* Added alpha = option for confidence limits.
* Differentiated between one-side and two-sided confidence limits.
* Added codecov.
* Various fixes.
* Added validation document.
* Allowed ordering of frequencies using factors.

# procs 0.0.9007

* Documentation updates.

# procs 0.0.9006

* Another big revision on return datasets.

# procs 0.0.9005

* Added more options to `out()` function.
* Wrote documentation.

# procs 0.0.9004

* Big revision on return datasets.
* Added attributes to reporter.
* Added `proc_print()`.

# procs 0.0.9003

* Means and Transpose documentation.
* More options on transpose.
* Added `proc_sort()`.
* Stub in proc compare.

# procs 0.0.9002

* Readme complete
* Freq documentation complete.
* `proc_means()` basically working.
* `proc_transpose()` basically working.

# procs 0.0.9001

* Create pkgdown site.
* Setup GitHub Actions.
* Add vignette stubs.

# procs 0.0.9000

A package to recreate some common SAS statistical procedures. Development 
Version 0.0.9000 will attempt to provide some of the functionality 
of SAS 'proc freq'.




# TTest Procedure ---------------------------------------------------------



#' @title Calculates T-Test Statistics
#' @encoding UTF-8
#' @description The \code{proc_ttest} function generates T-Test statistics
#' for selected variables on the input dataset.
#' The variables are identified on the \code{var} parameter or the \code{paired}
#' parameter. The function will calculate a standard set of T-Test statistics.
#' Results are displayed in
#' the viewer interactively and returned from the function.
#' @details
#' The \code{proc_ttest} function is for performing hypothesis testing.
#' Data is passed in on the \code{data}
#' parameter. The function can segregate data into
#' groups using the \code{by} parameter. There are also
#' options to determine whether and what results are returned.
#'
#' The \code{proc_ttest} function allows for three types of analysis:
#' \itemize{
#' \item{\strong{One Sample}: The one sample test allows you to perform
#' significance testing of a single variable against a known baseline
#' value or null hypothesis.  To perform this test, pass the variable name
#' on the \code{var} parameter and the baseline on the \code{h0=} option.
#' }
#' \item{\strong{Paired Comparison}: The paired comparison is for tests of
#' two variables with a natural pairing and the same number of observations
#' for both measures.
#' For instance, if you are checking for a change in blood pressure for
#' the same group of patients at different time points. To perform a paired
#' comparison, use the \code{paired} parameter with the two variables
#' separated by a star (*).
#' }
#' \item{\strong{Two Independant Samples}: The analysis of two independent
#' samples is used when there is no natural pairing, and there may be a different
#' number of observations in each group. This method is used, for example,
#' if you are comparing the effectiveness of a treatment between two different
#' groups of patients. The function assumes that there is
#' a single variable that contains the analysis values for both groups, and
#' another variable to identify the groups.  To perform this analysis,
#' pass the target variable name on the \code{var} parameter, and the
#' grouping variable on the \code{class} parameter.
#' }
#' }
#'
#' @section Interactive Output:
#' By default, \code{proc_ttest} results will
#' be sent to the viewer as an HTML report.  This functionality
#' makes it easy to get a quick analysis of your data. To turn off the
#' interactive report, pass the "noprint" keyword
#' to the \code{options} parameter.
#'
#' The \code{titles} parameter allows you to set one or more titles for your
#' report.  Pass these titles as a vector of strings.
#'
#' The exact datasets used for the interactive report can be returned as a list.
#' To return these datasets, pass
#' the "report" keyword on the \code{output} parameter. This list may in
#' turn be passed to \code{\link{proc_print}} to write the report to a file.
#'
#' @section Dataset Output:
#' Dataset results are also returned from the function by default.
#' \code{proc_ttest} typically returns multiple datasets in a list. Each
#' dataset will be named according to the category of statistical
#' results.  There are three standard categories: "Statistics",
#' "ConfLimits", and "TTests". For the class style analysis, the function
#' also returns a dataset called "Equality" that shows the Folded F analysis.
#'
#' The output datasets generated are optimized for data manipulation.
#' The column names have been standardized, and additional variables may
#' be present to help with data manipulation.  For example, the by variable
#' will always be named "BY".  In addition, data values in the
#' output datasets are intentionally not rounded or formatted
#' to give you the most accurate numeric results.
#'
#' @section Options:
#' The \code{proc_ttest} function recognizes the following options.  Options may
#' be passed as a quoted vector of strings, or an unquoted vector using the
#' \code{v()} function.
#' \itemize{
#' \item{\strong{alpha = }: The "alpha = " option will set the alpha
#' value for confidence limit statistics.  Set the alpha as a decimal value
#' between 0 and 1.  For example, you can set a 90% confidence limit as
#' \code{alpha = 0.1}.
#' }
#' \item{\strong{h0}: The "h0 =" option is used to set the baseline mean value
#' for testing a single variable.  Pass the option as a name/value pair,
#' such as \code{h0 = 95}.
#' }
#' \item{\strong{noprint}: Whether to print the interactive report to the
#' viewer.  By default, the report is printed to the viewer. The "noprint"
#' option will inhibit printing.  You may inhibit printing globally by
#' setting the package print option to false:
#' \code{options("procs.print" = FALSE)}.
#' }
#' }
#' @section Data Shaping:
#' The output datasets produced by the function can be shaped
#' in different ways. These shaping options allow you to decide whether the
#' data should be returned long and skinny, or short and wide. The shaping
#' options can reduce the amount of data manipulation necessary to get the
#' data into the desired form. The
#' shaping options are as follows:
#' \itemize{
#' \item{\strong{long}: Transposes the output datasets
#' so that statistics are in rows and variables are in columns.
#' }
#' \item{\strong{stacked}: Requests that output datasets
#' be returned in "stacked" form, such that both statistics and
#' variables are in rows.
#' }
#' \item{\strong{wide}: Requests that output datasets
#' be returned in "wide" form, such that statistics are across the top in
#' columns, and variables are in rows. This shaping option is the default.
#' }
#' }
#' These shaping options are passed on the \code{output} parameter.  For example,
#' to return the data in "long" form, use \code{output = "long"}.
# @section Data Constraints:
# Explain limits of data for each stat option.  Number of non-missing
# values, etc.
#'
#' @param data The input data frame for which to calculate summary statistics.
#' This parameter is required.
#' @param var The variable or variables to be used for hypothesis testing.
#' Pass the variable names in a quoted vector,
#' or an unquoted vector using the
#' \code{v()} function. If there is only one variable, it may be passed
#' unquoted.
#' If the \code{class}
#' variable is specified, the function will compare the two groups identified
#' in the class variable.  If the \code{class} variable is not specified,
#' enter the baseline hypothesis value on the "h0" option.  Default "h0" value
#' is zero (0).
#' @param paired A vector of paired variables to perform a paired T-Test on.
#' Variables should
#' be separated by a star (*).  The entire string should be quoted, for example,
#' \code{paired = "var1 * var2"}.  To test multiple pairs, place the pairs in a
#' quoted vector
#' : \code{paired = c("var1 * var2", "var3 * var4")}.  The parameter does not
#' accept parenthesis, hyphens, or any other shortcut syntax.
#' @param output Whether or not to return datasets from the function. Valid
#' values are "out", "none", and "report".  Default is "out", and will
#' produce dataset output specifically designed for programmatic use. The "none"
#' option will return a NULL instead of a dataset or list of datasets.
#' The "report" keyword returns the datasets from the interactive report, which
#' may be different from the standard output. The output parameter also accepts
#' data shaping keywords "long, "stacked", and "wide".
#' These shaping keywords control the structure of the output data. See the
#' \strong{Data Shaping} section for additional details. Note that
#' multiple output keywords may be passed on a
#' character vector. For example,
#' to produce both a report dataset and a "long" output dataset,
#' use the parameter \code{output = c("report", "out", "long")}.
#' @param by An optional by group. If you specify a by group, the input
#' data will be subset on the by variable(s) prior to performing any
#' statistics.
#' @param class The \code{class} parameter is used to perform a unpaired T-Test
#' between two different groups of the same variable.  For example, if you
#' want to test for a significant difference between a control group and a test
#' group, where the control and test groups are in rows identified by a
#' variable "Group".  Note that
#' there can only be two different values on the class variable.  Also, the
#' analysis is restricted to only one class variable.
# @param weight An optional weight parameter.
#' @param options A vector of optional keywords. Valid values are: "alpha =",
#' "h0 =", and "noprint". The "alpha = " option will set the alpha
#' value for confidence limit statistics.  The default is 95% (alpha = 0.05).
#' The "h0 = " option sets the baseline hypothesis value for single-variable
#' hypothesis testing.  The "noprint" option turns off the interactive report.
#' @param titles A vector of one or more titles to use for the report output.
#' @return Normally, the requested T-Test statistics are shown interactively
#' in the viewer, and output results are returned as a list of data frames.
#' You may then access individual datasets from the list using dollar sign ($)
#' syntax.
#' The interactive report can be turned off using the "noprint" option, and
#' the output datasets can be turned off using the "none" keyword on the
#' \code{output} parameter.
#' @import fmtr
#' @import tibble
#' @export
#' @examples
#' # Turn off printing for CRAN checks
#' options("procs.print" = FALSE)
#'
#' # Prepare sample data
#' dat1 <- subset(sleep, group == 1, c("ID", "extra"))
#' dat2 <- subset(sleep, group == 2, c("ID", "extra"))
#' dat <- data.frame(ID = dat1$ID, group1 = dat1$extra, group2 = dat2$extra)
#'
#' # View sample data
#' dat
#' #    ID group1 group2
#' # 1   1    0.7    1.9
#' # 2   2   -1.6    0.8
#' # 3   3   -0.2    1.1
#' # 4   4   -1.2    0.1
#' # 5   5   -0.1   -0.1
#' # 6   6    3.4    4.4
#' # 7   7    3.7    5.5
#' # 8   8    0.8    1.6
#' # 9   9    0.0    4.6
#' # 10 10    2.0    3.4
#'
#' # Example 1:  T-Test using h0 option
#' res1 <- proc_ttest(dat, var = "group1", options = c("h0" = 0))
#'
#' # View results
#' res1
#' # $Statistics
#' #      VAR  N MEAN     STD    STDERR  MIN MAX
#' # 1 group1 10 0.75 1.78901 0.5657345 -1.6 3.7
#' #
#' # $ConfLimits
#' #      VAR MEAN       LCLM    UCLM     STD
#' # 1 group1 0.75 -0.5297804 2.02978 1.78901
#' #
#' # $TTests
#' #      VAR DF       T     PROBT
#' # 1 group1  9 1.32571 0.2175978
#'
#' # Example 2: T-Test using paired parameter
#' res2 <- proc_ttest(dat, paired = "group2 * group1")
#'
#' # View results
#' res2
#' # $Statistics
#' #     VAR1   VAR2          DIFF  N MEAN      STD    STDERR MIN MAX
#' # 1 group2 group1 group2-group1 10 1.58 1.229995 0.3889587   0 4.6
#' #
#' # $ConfLimits
#' #     VAR1   VAR2          DIFF MEAN      LCLM     UCLM      STD   LCLMSTD  UCLMSTD
#' # 1 group2 group1 group2-group1 1.58 0.7001142 2.459886 1.229995 0.8460342 2.245492
#' #
#' # $TTests
#' #     VAR1   VAR2          DIFF DF        T      PROBT
#' # 1 group2 group1 group2-group1  9 4.062128 0.00283289
#'
#' # Example 3: T-Test using class parameter
#' res3 <- proc_ttest(sleep, var = "extra", class = "group")
#'
#' # View results
#' res3
#' # $Statistics
#' #     VAR      CLASS        METHOD  N  MEAN      STD    STDERR  MIN MAX
#' # 1 extra          1          <NA> 10  0.75 1.789010 0.5657345 -1.6 3.7
#' # 2 extra          2          <NA> 10  2.33 2.002249 0.6331666 -0.1 5.5
#' # 3 extra Diff (1-2)        Pooled NA -1.58       NA 0.8490910   NA  NA
#' # 4 extra Diff (1-2) Satterthwaite NA -1.58       NA 0.8490910   NA  NA
#' #
#' # $ConfLimits
#' #     VAR      CLASS        METHOD  MEAN       LCLM      UCLM      STD  LCLMSTD  UCLMSTD
#' # 1 extra          1          <NA>  0.75 -0.5297804 2.0297804 1.789010 1.230544 3.266034
#' # 2 extra          2          <NA>  2.33  0.8976775 3.7623225 2.002249 1.377217 3.655326
#' # 3 extra Diff (1-2)        Pooled -1.58 -3.3638740 0.2038740       NA       NA       NA
#' # 4 extra Diff (1-2) Satterthwaite -1.58 -3.3654832 0.2054832       NA       NA       NA
#' #
#' # $TTests
#' #     VAR        METHOD VARIANCES       DF         T      PROBT
#' # 1 extra        Pooled     Equal 18.00000 -1.860813 0.07918671
#' # 2 extra Satterthwaite   Unequal 17.77647 -1.860813 0.07939414
#' #
#' # $Equality
#' #    VAR   METHOD NDF DDF      FVAL     PROBF
#' # 1 extra Folded F   9   9 1.252595 0.7427199
#'
#' # Example 4: T-Test using alpha option and by variable
#' res4 <- proc_ttest(sleep, var = "extra", by = "group", options = c(alpha = 0.1))
#'
#' # View results
#' res4
#' # $Statistics
#' #   BY   VAR  N MEAN      STD    STDERR  MIN MAX
#' # 1  1 extra 10 0.75 1.789010 0.5657345 -1.6 3.7
#' # 2  2 extra 10 2.33 2.002249 0.6331666 -0.1 5.5
#' #
#' # $ConfLimits
#' # BY   VAR MEAN       LCLM     UCLM      STD  LCLMSTD  UCLMSTD
#' # 1  1 extra 0.75 -0.2870553 1.787055 1.789010 1.304809 2.943274
#' # 2  2 extra 2.33  1.1693340 3.490666 2.002249 1.460334 3.294095
#' #
#' # $TTests
#' #   BY   VAR DF        T       PROBT
#' # 1  1 extra  9 1.325710 0.217597780
#' # 2  2 extra  9 3.679916 0.005076133
#'
#' # Example 5: Single variable T-Test using "long" shaping option
#' res5 <- proc_ttest(sleep, var = "extra", output = "long")
#'
#' # View results
#' res5
#' # $Statistics
#' #     STAT      extra
#' # 1      N 20.0000000
#' # 2   MEAN  1.5400000
#' # 3    STD  2.0179197
#' # 4 STDERR  0.4512206
#' # 5    MIN -1.6000000
#' # 6    MAX  5.5000000
#' #
#' # $ConfLimits
#' #      STAT     extra
#' # 1    MEAN 1.5400000
#' # 2    LCLM 0.5955845
#' # 3    UCLM 2.4844155
#' # 4     STD 2.0179197
#' # 5 LCLMSTD 1.5346086
#' # 6 UCLMSTD 2.9473163
#' #
#' # $TTests
#' #    STAT       extra
#' # 1    DF 19.00000000
#' # 2     T  3.41296500
#' # 3 PROBT  0.00291762
#'
proc_ttest <- function(data,
                       var = NULL,
                       paired = NULL,
                       output = NULL,
                       by = NULL,
                       class = NULL,
                       # freq = NULL, ?
                       # weight = NULL, ?
                       options = NULL,
                       titles = NULL
) {

  # SAS seems to always ignore these
  # Not sure why R has an option to keep them
  missing <- FALSE


  # Deal with single value unquoted parameter values
  oby <- deparse(substitute(by, env = environment()))
  by <- tryCatch({if (typeof(by) %in% c("character", "NULL")) by else oby},
                 error = function(cond) {oby})

  # Deal with single value unquoted parameter values
  oclass <- deparse(substitute(class, env = environment()))
  class <- tryCatch({if (typeof(class) %in% c("character", "NULL")) class else oclass},
                    error = function(cond) {oclass})

  ovar <- deparse(substitute(var, env = environment()))
  var <- tryCatch({if (typeof(var) %in% c("character", "NULL")) var else ovar},
                  error = function(cond) {ovar})

  oopt <- deparse(substitute(options, env = environment()))
  options <- tryCatch({if (typeof(options) %in% c("integer", "double", "character", "NULL")) options else oopt},
                      error = function(cond) {oopt})

  oout <- deparse(substitute(output, env = environment()))
  output <- tryCatch({if (typeof(output) %in% c("character", "NULL")) output else oout},
                     error = function(cond) {oout})

  opaired <- deparse(substitute(paired, env = environment()))
  paired <- tryCatch({if (typeof(paired) %in% c("character", "NULL")) paired else opaired},
                     error = function(cond) {opaired})

  # Parameter checks
  nms <- names(data)

  if (!is.null(by)) {
    if (!all(by %in% nms)) {

      stop(paste("Invalid by name: ", by[!by %in% nms], "\n"))
    }
  }

  if (!is.null(class)) {
    if (length(class) > 1) {

      stop(paste("Class parameter cannot contain more than one variable.\n"))

    }
    if (!all(class %in% nms)) {

      stop(paste("Invalid class name: ", class[!class %in% nms], "\n"))
    }
  }


  if (!is.null(output)) {
    outs <- c("out", "report", "none", "wide", "long", "stacked")
    if (!all(tolower(output) %in% outs)) {

      stop(paste("Invalid output keyword: ", output[!tolower(output) %in% outs], "\n"))
    }

  }

  # https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_ttest_overview.htm
  # https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_ttest_syntax01.htm
  aopts <- c("alpha=", "dist", "H0=", "sides", "test", "tost", "crossover=") # analysis options
  dopts <- c("ci", "cochran", "plots") # display options
  oopts <- c("byvar", "nobyvar")  # Output ordering

  # Parameter checks for options
  if (!is.null(options)) {

    kopts <- c("alpha", "h0", "noprint")

    # Deal with "alpha =" and "maxdec = " by using name instead of value
    nopts <- names(options)

    if (is.null(nopts) & length(options) > 0)
      nopts <- options

    mopts <- ifelse(nopts == "", options, nopts)


    if (!all(tolower(mopts) %in% kopts)) {

      stop(paste0("Invalid options keyword: ", mopts[!tolower(mopts) %in% kopts], "\n"))
    }
  }

  # Parameter check for paired parameter
  if (!is.null(paired)) {

    splt <- strsplit(paired, "*", fixed = TRUE)

    for (i in seq_len(length(splt))) {

      for (j in seq_len(length(splt[[i]]))) {
        vr <- trimws(splt[[i]][j])

        if (!vr %in% nms) {

          stop(paste0("Invalid paired variable name: ", vr, "\n"))

        }
      }
    }
  }


  rptflg <- FALSE
  rptnm <- ""
  rptres <- NULL

  # Kill output request for report
  # Otherwise, this will mess up gen_output_ttest
  if (has_report(output)) {
    rptflg <- TRUE
    rptnm <- "report"
  }

  if (has_view(options))
    view <- TRUE
  else
    view <- FALSE

  res <- NULL


  # Get report if requested
  if (view == TRUE | rptflg) {
    rptres <- gen_report_ttest(data, by = by, var = var, class = class,
                               paired = paired, view = view,
                               titles = titles, #weight = weight,
                               opts = options, output = output)
  }

  # Get output datasets if requested
  if (has_output(output)) {
    res <- gen_output_ttest(data,
                            by = by,
                            class = class,
                            var = var,
                            paired = paired,
                            output = output,
                            opts = options
    )
  }

  # Add report to result if requested
  if (rptflg & !is.null(rptres)) {

    if (is.null(res))
      res <- rptres
    else {
      res <- list(out = res, report = rptres)
    }


  }

  # Log the means function
  log_ttest(data,
            by = by,
            class = class,
            var = var,
            paired = paired,
            output = output,
            # weight = weight,
            view = view,
            titles = titles,
            options = options,
            outcnt = ifelse("data.frame" %in% class(res),
                            1, length(res)))

  # If only one dataset returned, remove list
  if (length(res) == 1)
    res <- res[[1]]

  if (log_output()) {
    log_logr(res)
    return(res)
  }

  return(res)
}


log_ttest <- function(data,
                      by = NULL,
                      class = NULL,
                      var = NULL,
                      paired = NULL,
                      output = NULL,
                      #weight = NULL,
                      view = TRUE,
                      titles = NULL,
                      options = NULL,
                      outcnt = NULL) {

  ret <- c()

  indt <- paste0(rep(" ", 12), collapse = "")

  ret <- paste0("proc_ttest: input data set ", nrow(data),
                " rows and ", ncol(data), " columns")

  if (!is.null(by))
    ret[length(ret) + 1] <- paste0(indt, "by: ", paste(by, collapse = " "))

  if (!is.null(class))
    ret[length(ret) + 1] <- paste0(indt, "class: ",
                                   paste(class, collapse = " "))

  if (!is.null(var))
    ret[length(ret) + 1] <- paste0(indt, "var: ",
                                   paste(var, collapse = " "))

  if (!is.null(paired))
    ret[length(ret) + 1] <- paste0(indt, "paired: ",
                                   paste(paired, collapse = " "))

  if (!is.null(output))
    ret[length(ret) + 1] <- paste0(indt, "output: ", paste(output, collapse = " "))

  # if (!is.null(weight))
  #   ret[length(ret) + 1] <- paste0(indt, "weight: ", paste(weight, collapse = " "))

  if (!is.null(view))
    ret[length(ret) + 1]<- paste0(indt, "view: ", paste(view, collapse = " "))

  if (!is.null(titles))
    ret[length(ret) + 1] <- paste0(indt, "titles: ", paste(titles, collapse = "\n"))


  if (!is.null(outcnt))
    ret[length(ret) + 1] <- paste0(indt, "output: ", outcnt, " datasets")


  log_logr(ret)

}


# Utilities -----------------------------------------------------------------

tlbls <- c(METHOD = "Method", VARIANCES = "Variances", NDF = "Num DF", DDF = "Den DF",
           FVAL = "F Value", PROBF = "Pr > F", mlbls)

ttest_fc <- fcat(N = "%d", MEAN = "%.4f", STD = "%.4f", STDERR = "%.4f",
                 MIN = "%.4f", MAX = "%.4f", UCLM = "%.4f", LCLM = "%.4f",
                 DF = "%.3f", "T" = "%.2f", PROBT = "%.4f", NDF = "%.3f", DDF = "%.3f",
                 FVAL = "%.2f", PROBF = "%.4f", UCLMSTD = "%.4f", LCLMSTD = "%.4f",
                 log = FALSE)

get_output_specs_ttest <- function(data, var, paired, class, opts, output,
                                   report = FALSE) {

  dat <- data
  spcs <- list()


  for (vr in var) {
    if (!vr %in% names(dat))
      stop("Variable '" %p% vr %p% "' not found in data.")
  }

  if (is.null(paired) & is.null(var) & is.null(class)) {

    stop("Procedure requires 'var', 'paired', or 'class' parameters to be specified")


  } else if (!is.null(paired) & !is.null(var)) {

      stop("Specify either the 'var' or 'paired' parameter, but not both.")

  } else if (!is.null(class)) {


    if (report == TRUE) {

      stats <- c("n", "mean", "std", "stderr", "min", "max")

      for (vr in var) {

        vlbl <- ""
        if (length(var) > 1)
          vlbl <- paste0(vr, ":")

        spcs[[paste0(vlbl, "Statistics")]] <- out_spec(var = vr, stats = stats, shape = "wide",
                                         type = FALSE, freq = FALSE, report = report)

        spcs[[paste0(vlbl, "ConfLimits")]] <- out_spec(var = vr, stats = c("mean", "clm", "std", "clmstd"), shape = "wide",
                                         types = FALSE, freq = FALSE, report = report)

        spcs[[paste0(vlbl, "TTests")]] <- out_spec(stats = c("df", "t", "probt"),
                                     shape = "wide",
                                     type = FALSE, freq = FALSE,
                                     var = vr, report = report)

        spcs[[paste0(vlbl, "Equality")]] <- out_spec(stats = "dummy", var = vr, types = FALSE, freq = FALSE)

      }

    } else {

      shp <- "wide"
      if (!is.null(output)) {
        if ("long" %in% output)
          shp <- "long"
        else if ("stacked" %in% output)
          shp <- "stacked"

      }

      stats <- c("n", "mean", "std", "stderr", "min", "max")


      spcs[["Statistics"]] <- out_spec(var = var, stats = stats, shape = shp,
                                       type = FALSE, freq = FALSE, report = report)

      spcs[["ConfLimits"]] <- out_spec(var = var, stats = c("mean", "clm", "std", "clmstd"), shape = shp,
                                       types = FALSE, freq = FALSE, report = report)

      spcs[["TTests"]] <- out_spec(stats = c("df", "t", "probt"),
                                   shape = shp,
                                   type = FALSE, freq = FALSE,
                                   var = var, report = report)

      spcs[["Equality"]] <- out_spec(stats = "dummy", var = var, types = FALSE, freq = FALSE, shape = shp)


    }


  } else if (is.null(paired) & !is.null(var) & is.null(class)) {


    h0 <- get_option(opts, "h0", 0)

    shp <- "wide"
    if (report == FALSE) {
      if ("long" %in% output)
        shp <- "long"
      else if ("stacked" %in% output)
        shp <- "stacked"
    }

    if (report == FALSE) {

      for (vr in var) {
        dat[[paste0("..", vr)]] <- dat[[vr]] - h0
      }

      vrs <- paste0("..", var)

      stats <- c("n", "mean", "std", "stderr", "min", "max")

      spcs[["Statistics"]] <- out_spec(stats = stats, shape = shp,
                                                   type = FALSE, freq = FALSE,
                                                   var = var, format = "%.4f",
                                       report = report)

      spcs[["ConfLimits"]] <- out_spec(stats = c("mean", "clm", "std", "clmstd"), shape = shp,
                                                   types = FALSE, freq = FALSE,
                                       var = var, report = report)

      spcs[["TTests"]] <- out_spec(stats = c("df", "t", "probt"),
                                               shape =  shp,
                                               type = FALSE, freq = FALSE,
                                               var = vrs, report = report,
                                   varlbl = var)


    } else {

      for (vr in var) {

        vn <- ""
        if (length(var) > 1)
          vn <- paste0(vr, ":")

        dat[[paste0("..", vr)]] <- dat[[vr]] - h0

        stats <- c("n", "mean", "std", "stderr", "min", "max")

        spcs[[paste0(vn, "Statistics")]] <- out_spec(stats = stats, shape = shp,
                                             type = FALSE, freq = FALSE,
                                             var = vr, format = "%.4f", report = report)

        spcs[[paste0(vn, "ConfLimits")]] <- out_spec(stats = c("mean", "clm", "std", "clmstd"), shape = shp,
                                         types = FALSE, freq = FALSE, var = vr, report = report)

        spcs[[paste0(vn, "TTests")]] <- out_spec(stats = c("df", "t", "probt"),
                                     shape =  shp,
                                     type = FALSE, freq = FALSE,
                                     var = paste0("..", vr), report = report, varlbl = vr)

      }

    }




  } else if (!is.null(paired)) {

    if (report == TRUE) {
      for (i in seq_len(length(paired))) {

        pr <- paired[i]

        splt <- trimws(strsplit(pr, "*", fixed = TRUE)[[1]])
        v1 <- splt[1]
        v2 <- splt[2]

        vnm <- paste0("..diff", i)

        if (report == TRUE)
          lnm <- paste0("diff", i, ":")
        else
          lnm <- ""

        dat[[vnm]] <- dat[[v1]] - dat[[v2]]

        vr <- paste0(v1, "-", v2)

        stats <- c("n", "mean", "std", "stderr", "min", "max")

        spcs[[paste0(lnm, "Statistics")]] <- out_spec(stats = stats, shape = "wide",
                                         type = FALSE, freq = FALSE,
                                         var = vnm, report = report, varlbl = vr)

        spcs[[paste0(lnm, "ConfLimits")]] <- out_spec(stats = c("mean", "clm", "std", "clmstd"), shape = "wide",
                                         types = FALSE, freq = FALSE, var = vnm,
                                         varlbl = vr, report = report)

        spcs[[paste0(lnm, "TTests")]] <- out_spec(stats = c("df", "t", "probt"),
                                     shape = "wide",
                                     type = FALSE, freq = FALSE,
                                     var = vnm, varlbl = vr,
                                     report = report)

      }

    } else {

      shp <- "wide"
      if (report == FALSE) {
        if ("long" %in% output)
          shp <- "long"
        else if ("stacked" %in% output)
          shp <- "stacked"
      }


      vrs <- c()
      vlbls <- c()
      for (i in seq_len(length(paired))) {

        pr <- paired[i]

        splt <- trimws(strsplit(pr, "*", fixed = TRUE)[[1]])
        v1 <- splt[1]
        v2 <- splt[2]

        vnm <- paste0("..diff", i)

        dat[[vnm]] <- dat[[v1]] - dat[[v2]]

        vrs[i] <- vnm
        vlbls[i] <- paste0(v1, "-", v2)

      }

        stats <- c("n", "mean", "std", "stderr", "min", "max")

        spcs[["Statistics"]] <- out_spec(stats = stats, shape = shp,
                                                      type = FALSE, freq = FALSE,
                                                      var = vrs,
                                                      varlbl = vlbls,
                                                      report = report)

        spcs[["ConfLimits"]] <- out_spec(stats = c("mean", "clm", "std", "clmstd"), shape = shp,
                                                      types = FALSE, freq = FALSE, var = vrs,
                                                      varlbl = vlbls,
                                                      report = report)

        spcs[["TTests"]] <- out_spec(stats = c("df", "t", "probt"),
                                                  shape = shp,
                                                  type = FALSE, freq = FALSE,
                                                  var = vrs,
                                                  varlbl = vlbls,
                                                  report = report)



    }


  } else {



  }



  ret <- list(data = dat, outreq = spcs)

  return(ret)

}


#' @import sasLM
#' @import common
get_class_ttest <- function(data, var, class, report = TRUE, opts = NULL,
                            byvar = NULL, byval = NULL, shape = NULL) {

  ret <- list()

  if (is.factor(data[[class]]) == FALSE)
    data$sfact <- factor(data[[class]])
  else
    data$sfact <- data[[class]]

  lst <- split(data, f = data$sfact, drop=FALSE)

  nms <- names(lst)

  v1 <- lst[[1]][[var]]
  v2 <- lst[[2]][[var]]

  # # SAS appears to be putting the shorter vector as the experiment
  # if (length(v2) < length(v1)) {
  #   v1 <- lst[[2]][[var]]
  #   v2 <- lst[[1]][[var]]
  #
  # }

  alph <- 1 - get_alpha(opts)

  ttret1 <- TTEST(v1, v2, alph)
  ttret2 <- TTEST(v2, v1, alph)

  ft1 <- as.data.frame(unclass(ttret1$`F-test for the ratio of variances`),
                      stringsAsFactors = FALSE)
  ft2 <- as.data.frame(unclass(ttret2$`F-test for the ratio of variances`),
                       stringsAsFactors = FALSE)

  # SAS appears to be taking the one with the greatest F Value
  if (ft1$`F value` > ft2$`F value`) {
    ft <- ft1
  } else {

    ft <- ft2
  }

  st <- as.data.frame(ttret1$`Statistics by group`,
                      stringsAsFactors = FALSE)
  tt <- as.data.frame(unclass(ttret1$`Two groups t-test for the difference of means`),
                      stringsAsFactors = FALSE)
  # ft <- as.data.frame(unclass(ttret$`F-test for the ratio of variances`),
  #                     stringsAsFactors = FALSE)

  vcls <- c("Diff (1-2)")
  empt <- c(NA, NA)
  mth <- c("Pooled", "Satterthwaite")
  vari <- c("Equal", "Unequal")

  ret[["Statistics"]] <- data.frame(CLASS = vcls, METHOD = mth, N = empt,
                                    MEAN = tt$PE,
                                    STD = empt, STDERR = tt$SE, MIN = empt,
                                    MAX = empt,
                                    stringsAsFactors = FALSE)

  ret[["ConfLimits"]] <- data.frame(CLASS = vcls, METHOD = mth, MEAN = tt$PE,
                                    LCLM = tt$LCL ,
                                    UCLM = tt$UCL, STD = empt,
                                    stringsAsFactors = FALSE)


  ret[["TTests"]] <- data.frame(METHOD = mth, VARIANCES = vari, DF = tt$Df,
                                "T" = tt$`t value`, PROBT = tt$`Pr(>|t|)`,
                                stringsAsFactors = FALSE)

  ret[["Equality"]] <- data.frame(METHOD = "Folded F", NDF = ft$`Num Df`,
                                  DDF = ft$`Denom Df`, FVAL = ft$`F value`,
                                  PROBF = ft$`Pr(>F)`,
                                  stringsAsFactors = FALSE)


  if (report == FALSE) {

    if (is.null(byvar)) {
      ret[["Statistics"]] <- data.frame(VAR = var, ret[["Statistics"]],
                                        stringsAsFactors = FALSE)

      ret[["ConfLimits"]] <- data.frame(VAR = var, ret[["ConfLimits"]],
                                        stringsAsFactors = FALSE)

      ret[["TTests"]] <- data.frame(VAR = var, ret[["TTests"]],
                                        stringsAsFactors = FALSE)

      ret[["Equality"]] <- data.frame(VAR = var, ret[["Equality"]],
                                        stringsAsFactors = FALSE)

    } else {

      vlst <- list(VAR = var)

      for (nm in names(byval))
        vlst[[nm]] <- byval[nm]

      vdat <- as.data.frame(vlst, stringsAsFactors = FALSE, row.names = NULL)

      ret[["Statistics"]] <- data.frame(vdat, ret[["Statistics"]],
                                        stringsAsFactors = FALSE)

      ret[["ConfLimits"]] <- data.frame(vdat, ret[["ConfLimits"]],
                                        stringsAsFactors = FALSE)

      ret[["TTests"]] <- data.frame(vdat, ret[["TTests"]],
                                    stringsAsFactors = FALSE)

      ret[["Equality"]] <- data.frame(vdat, ret[["Equality"]],
                                      stringsAsFactors = FALSE)


    }

    if (!is.null(shape)) {
      ret[["Statistics"]] <- shape_ttest_data(ret[["Statistics"]], shape = shape)

      ret[["ConfLimits"]] <- shape_ttest_data(ret[["ConfLimits"]], shape = shape)

      ret[["TTests"]] <- shape_ttest_data(ret[["TTests"]], shape = shape)

      ret[["Equality"]] <- shape_ttest_data(ret[["Equality"]], shape = shape)
    }
  }

  return(ret)
}

# Function to set two tables with unequal columns
add_class_ttest <- function(rtbl, ctbl) {


  ret <- perform_set(rtbl, ctbl)

  nms <- names(ret)

  # Output datasets
  if ("VAR" %in% nms) {

    bynms <- find.names(ret, "BY*")

    onms <- nms[!nms %in% c("VAR", bynms, "CLASS", "METHOD")]

    ret <- ret[ , c("VAR", bynms, "CLASS", "METHOD", onms)]

  } else { # Report datasets
    onms <- nms[!nms %in% c("CLASS", "METHOD")]

    ret <- ret[ , c("CLASS", "METHOD", onms)]

  }

  return(ret)
}

# Specifically for shaping results of get_class_ttest()
shape_ttest_data <- function(ds, shape, copy = NULL) {

  # Assumed to be wide
  ret <- ds

  if (!is.null(shape)) {
    if (all(shape == "long")) {

      bv <- c("CLASS", "METHOD")
      if (!"CLASS" %in% names(ds))
        bv <- "METHOD"

      bnms <- find.names(ds, "BY*")
      if (!is.null(bnms))
        bv <- c(bnms, bv)

      ret <- proc_transpose(ds, by = bv,
                            name = "STAT", id = "VAR", copy = copy,
                            log = FALSE)

      # ret <- proc_transpose(ret, id = "VAR", name = "STAT",
      #                       copy = copy, log = FALSE)


    } else if (all(shape == "stacked")) {


      bv <- c("VAR", "CLASS", "METHOD")
      if (!"CLASS" %in% names(ds))
        bv <- c("VAR", "METHOD")

      bnms <- find.names(ds, "BY*")
      if (!is.null(bnms))
        bv <- c(bnms, bv)

      ret <- proc_transpose(ds, by = bv, name = "STAT",
                     copy = copy, log = FALSE)

      # ret <- proc_transpose(ret, name = "STAT", by = "VAR",
      #                       copy = copy, log = FALSE)

      rnms <- names(ret)
      rnms[rnms %in% "COL1"] <- "VALUES"

      names(ret) <- rnms

    }
  }

  return(ret)

}

# Drivers -----------------------------------------------------------------

#' @import common
gen_report_ttest <- function(data,
                             by = NULL,
                             class = NULL,
                             var = NULL,
                             paired = NULL,
                             weight = NULL,
                             opts = NULL,
                             output = NULL,
                             view = TRUE,
                             titles = NULL) {


  spcs <- get_output_specs_ttest(data, var, paired, class,
                                 opts, output, report = TRUE)

  data <- spcs$data
  outreq <- spcs$outreq
  nms <- names(outreq)

  # Declare return list
  res <- list()
  byres <- list()

  # Assign CL Percentage on Labels
  alph <- (1 - get_alpha(opts)) * 100
  tlbls[["UCLM"]] <- sprintf(tlbls[["UCLM"]], alph)
  tlbls[["LCLM"]] <- sprintf(tlbls[["LCLM"]], alph)
  tlbls[["UCLMSTD"]] <- sprintf(tlbls[["UCLMSTD"]], alph)
  tlbls[["LCLMSTD"]] <- sprintf(tlbls[["LCLMSTD"]], alph)

  #browser()

  if (length(outreq) > 0) {

    bylbls <- c()
    if (!is.null(by)) {

      lst <- unclass(data)[by]
      for (nm in names(lst))
        lst[[nm]] <- as.factor(lst[[nm]])
      dtlst <- split(data, lst, sep = "|", drop = TRUE)

      snms <- strsplit(names(dtlst), "|", fixed = TRUE)

      for (k in seq_len(length(snms))) {
        for (l in seq_len(length(by))) {
          lv <- ""
          if (!is.null(bylbls[k])) {
            if (!is.na(bylbls[k])) {
              lv <- bylbls[k]
            }
          }

          if (l == length(by))
            cma <- ""
          else
            cma <- ", "

          bylbls[k] <- paste0(lv, by[l], "=", snms[[k]][l], cma)
        }
      }

    } else {

      dtlst <- list(data)
    }

    # Loop through by groups
    for (j in seq_len(length(dtlst))) {

      # Get table for this by group
      dt <- dtlst[[j]]
      bynm <- "main"

      for (i in seq_len(length(outreq))) {

        outp <- outreq[[i]]
        nm <- nms[i]
        tnm <- get_ttest_type(nm)


        # Get class-level t-tests
        if (!is.null(class)) {

          ctbl <- get_class_ttest(dt, outp$var, class, TRUE, opts)
        }

        if (is.null(class) ||
           (!is.null(class) && tnm %in% c("Statistics", "ConfLimits"))) {   # Fix this

          # data, var, class, outp, freq = TRUE,
          # type = NULL, byvals = NULL
          #outp <- out_spec(stats = stats, shape = "wide")
          smtbl <- get_class_report(dt, outp$var, class, outp, freq = FALSE, opts = opts)



          # Add in class-level tests if needed
          if (!is.null(class)) {

            smtbl <- add_class_ttest(smtbl, ctbl[[tnm]])
          }

        } else if (!is.null(class)) {

          # Add extra tests for class comparison
          smtbl <- ctbl[[tnm]]

        }

        if (tnm == "Statistics") {

          if (!is.null(outp$varlbl)) {
            if (!is.null(paired)) {
              attr(smtbl, "ttls") <- paste0("Difference: ", outp$varlbl)
            } else {
              attr(smtbl, "ttls") <- paste0("Variable: ", outp$varlbl)
            }

          } else {
            attr(smtbl, "ttls") <- paste0("Variable: ", outp$var)
          }
        }


        # Add spanning headers if there are by groups
        if (!is.null(by) & !is.null(smtbl)) {

          # Add spanning headers
          # spn <- span(1, ncol(smtbl), label = bylbls[j], level = 1)
          # attr(smtbl, "spans") <- list(spn)

          bynm <-  bylbls[j]

          if (tnm == "Statistics") {

            attr(smtbl, "ttls") <- c(attr(smtbl, "ttls"), bynm)

          }

        }

        # Add default formats
        # fmt <- "%.4f"
        formats(smtbl) <- ttest_fc
        # for (cnm in names(smtbl)) {
        #
        #   if (typeof(smtbl[[cnm]]) %in% c("double")) {
        #
        #     attr(smtbl[[cnm]], "format") <- fmt
        #   }
        #
        # }


        # Assign labels
        if (is.null(class))
          labels(smtbl) <- tlbls
        else {

          cv <- class
          if (length(class) == 1)
            cnms <- "CLASS"
          else
            cnms <- paste0("CLASS", seq(1, length(class)))

          names(cv) <- cnms

          labels(smtbl) <- append(as.list(cv), tlbls)

        }

        # Kill var variable for reports
        if ("VAR" %in% names(smtbl)) {

          smtbl[["VAR"]] <- NULL

        } else {

          if (!is.null(outp$varlbl))
            smtbl[["VAR"]] <- outp$varlbl
        }

        # Convert to tibble if incoming data is a tibble
        if ("tbl_df" %in% class(data)) {
          res[[nm]] <- as_tibble(smtbl)
        } else {
          res[[nm]] <- smtbl
        }

      }

      byres[[bynm]] <- res
    }

    # Add summary plot  - Commented for now because I can't get it to match SAS.
    # res[["SummaryPanel"]] <- gen_summarypanel(dt, var, confidence = alph)

  }

  # Assign return object
  if (length(byres) == 1)
    ret <- byres[[1]]
  else
    ret <- byres

  # Determine if printing
  gv <- options("procs.print")[[1]]
  if (is.null(gv))
    gv <- TRUE

  # Create viewer report if requested
  if (gv) {
    if (view == TRUE && interactive()) {

      vrfl <- tempfile()

      if (is.null(titles))
        titles <- "The TTEST Function"


     # attr(ret, "ttls") <- c(titles, attr(ret, "ttls"))

      # for (bc in seq_len(length(byres))) {
      #
      #   ttls <- c()
      #   for (tc in seq_len(length(var))) {
      #     if (!is.null(var))
      #       ttls[length(ttls) + 1] <- paste0("Variable: ", var[tc])
      #     else if (!is.null(paired))
      #       ttls[length(ttls) + 1] <- paste0("Variable: ", sub("*", "-", paired, fixed = TRUE))
      #
      #
      #   }
      #
      #   if (length(byres) > 1) {
      #     ttls[length(ttls) + 1] <- names(byres)[bc]
      #
      #     attr(ret[[bc]], "ttls") <- ttls
      #
      #   } else {
      #
      #     attr(ret, "ttls") <- ttls
      #   }
      #
      # }

      out <- output_report(ret, dir_name = dirname(vrfl),
                           file_name = basename(vrfl), out_type = "HTML",
                           titles = titles, margins = .5, viewer = TRUE,
                           pages = length(byres))

      show_viewer(out)
    }
  }

  return(ret)

}



#' @import fmtr
#' @import common
gen_output_ttest <- function(data,
                             by = NULL,
                             class = NULL,
                             var = NULL,
                             paired = NULL,
                             weight = NULL,
                             output = NULL,
                             opts = NULL) {

  # Assign CL Percentage on Labels
  alph <- (1 - get_alpha(opts)) * 100
  tlbls[["UCLM"]] <- sprintf(tlbls[["UCLM"]], alph)
  tlbls[["LCLM"]] <- sprintf(tlbls[["LCLM"]], alph)
  tlbls[["UCLMSTD"]] <- sprintf(tlbls[["UCLMSTD"]], alph)
  tlbls[["LCLMSTD"]] <- sprintf(tlbls[["LCLMSTD"]], alph)

  spcs <- get_output_specs_ttest(data, var, paired, class,
                                 opts, output, report = FALSE)

  data <- spcs$data
  outreq <- spcs$outreq



  res <- list()
  ctbl <- NULL

  if (length(outreq) > 0) {

    nms <- names(outreq)
    for (i in seq_len(length(outreq))) {

      outp <- outreq[[i]]
      nm <- nms[i]
      tnm <- get_ttest_type(nm)

      # Create vector of NA class values
      cls <- NULL
      ctypes <- list() # Not used but not ready to get rid of it yet
      if (!is.null(class)) {
        cls <- rep(NA, length(class))
        names(cls) <- class

        if (has_option(opts, "completetypes")) {
          for (cnm in class) {
            if (is.factor(data[[cnm]])) {
              ctypes[[cnm]] <- levels(data[[cnm]])
            } else {
              ctypes[[cnm]] <- unique(data[[cnm]])
            }
          }
        }
      }

      bdat <- list(data)
      if (!is.null(by)) {

        bdat <- split(data, data[ , by, drop = FALSE], sep = "|", drop = TRUE)

      }
      bynms <- names(bdat)

      # Make up by variable names for output ds
      byn <- NULL
      bylbls <- NULL
      if (!is.null(by)) {
        if (length(by) == 1)
          byn <- "BY"
        else
          byn <- paste0("BY", seq(1, length(by)))

        bylbls <- by
        names(bylbls) <- byn
      }


      byres <- NULL

      for (j in seq_len(length(bdat))) {

        tmpres <- NULL
        dat <- bdat[[j]]

        # Deal with by variable values
        bynm <- NULL
        if (!is.null(bynms)) {
          bynm <- strsplit(bynms[j], "|", fixed = TRUE)[[1]]
          names(bynm) <- byn
        }


        if (is.null(class)) {

          # Always add type 0
          tmpby <- get_output(dat, var = outp$var,
                              by = bynm,
                              class = cls,
                              stats = outp$stats,
                              shape = outp$shape,
                              freq = outp$parameters$freq,
                              type = outp$parameters$type,
                              opts = opts)

          if (!is.null(paired)) {
            tmpby <- add_paired_vars(tmpby, outp$varlbl, outp$shape)
          }

          # Get rid of temporary var names
          tmpby <- fix_var_names(tmpby, outp$var, outp$varlbl, outp$shape, tnm)


          if (is.null(tmpres))
            tmpres <- tmpby
          else
            tmpres <- rbind(tmpres, tmpby)

        }


        if (!is.null(class)) {

          for (vr in outp$var) {

            # Get class-level t-tests
            ctbl <- get_class_ttest(dat, vr, class, FALSE, opts,
                                    byvar = by, byval = bynm,
                                    shape = outp$shape)


            if (nm %in% c("Statistics", "ConfLimits")) {

              # Get standard statistics
              tmpcls <- get_class_output(dat, var = vr,
                                         class = class, outp = outp,
                                         freq = outp$parameters$freq,
                                         type = outp$parameters$type,
                                         byvals = bynm, opts = opts,
                                         stats = outp$stats)

              tmpcls <- add_class_ttest(tmpcls, ctbl[[nm]])

            } else {

              tmpcls <- ctbl[[nm]]
            }


            if (is.null(tmpres))
              tmpres <- tmpcls
            else {

              if (outp$shape == "long")
                tmpres[[vr]] <- tmpcls[[vr]]
              else
                tmpres <- rbind(tmpres, tmpcls)

            }

          }

        }

        if (is.null(byres))
          byres <- tmpres
        else {
          byres <- rbind(byres, tmpres)
        }
      }

      tmpres <- byres
      byres <- NULL


      # Replace VAR value
      if (!is.null(paired)) {
        if ("DIFF" %in% names(tmpres)) {
          if (!is.null(outp$varlbl))
            tmpres[["DIFF"]] <- outp$varlbl
          else
            tmpres[["DIFF"]] <- outp$var
        }
      } else if ((!is.null(var) && is.null(class))) {
        if (outp$shape != "stacked") {
          if ("VAR" %in% names(tmpres)) {
            if (!is.null(outp$varlbl))
              tmpres[["VAR"]] <- outp$varlbl
            else
              tmpres[["VAR"]] <- outp$var
          }
        }
      }

      # Kill type variable for output datasets
      if ("TYPE" %in% names(tmpres)) {

        tmpres[["TYPE"]] <- NULL
      }

      # System Labels
      if (!is.null(tmpres))
        labels(tmpres) <- append(tlbls, bylbls)

      # Class labels
      clbls <- NULL
      if (!is.null(class)) {
        if (length(class) == 1)
          cnms <- "CLASS"
        else {
          cnms <- paste0("CLASS", seq(1, length(class)))
        }
        clbls <- as.list(class)
        names(clbls) <- cnms

        labels(tmpres) <- clbls
      }

      # User labels
      if (!is.null(outp$label))
        labels(tmpres) <- outp$label

      # Formats
      if (!is.null(outp$format))
        formats(tmpres) <- outp$format

      # Reset rownames
      rownames(tmpres) <- NULL



      res[[nms[i]]]  <- tmpres

    }
  }

  if (length(res) == 1)
    res <- res[[1]]


  return(res)

}

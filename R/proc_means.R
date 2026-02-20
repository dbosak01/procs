
# Means Procedure ---------------------------------------------------------



#' @title Calculates Summary Statistics
#' @encoding UTF-8
#' @description The \code{proc_means} function generates summary statistics
#' for selected variables on the input dataset.  The variables are identified
#' on the \code{var} parameter.  The statistics to perform are identified
#' on the \code{stats} parameter.  Results are displayed in
#' the viewer interactively and returned from the function.
#' @details
#' The \code{proc_means} function is for analysis of continuous variables.
#' Data is passed in on the \code{data}
#' parameter.  The desired statistics are specified using keywords
#' on the \code{stats} parameter.  The function can segregate data into
#' groups using the \code{by} and \code{class} parameters. There are also
#' options to determine whether and what results are returned.
#'
#' @section Interactive Output:
#' By default, \code{proc_freq} results will
#' be immediately sent to the viewer as an HTML report.  This functionality
#' makes it easy to get a quick analysis of your data. To turn off the
#' interactive report, pass the "noprint" keyword
#' to the \code{options} parameter.
#'
#' The \code{titles} parameter allows you to set one or more titles for your
#' report.  Pass these titles as a vector of strings.
#'
#' The exact datasets used for the interactive report can be returned as a list.
#' To return these datasets as a list, pass
#' the "report" keyword on the \code{output} parameter. This list may in
#' turn be passed to \code{\link{proc_print}} to write the report to a file.
#'
#' @section Dataset Output:
#' Dataset results are also returned from the function by default.
#' If the results are a single dataset, a single
#' data frame will be returned.  If there are multiple results, a list of data
#' frames will be returned.
#'
#' The output datasets generated are optimized for data manipulation.
#' The column names have been standardized, and additional variables may
#' be present to help with data manipulation.  For example, the by variable
#' will always be named "BY", and the class variable will always be named
#' "CLASS".  In addition, data values in the
#' output datasets are intentionally not rounded or formatted
#' to give you the most accurate statistical results.
#'
#' @section Statistics Keywords:
#' The following statistics keywords can be passed on the \code{stats}
#' parameter.  Normally, each statistic
#' will be contained in a separate column and the column name will be
#' the same as the statistic keyword. You may pass statistic keywords as a
#' quoted vector of strings, or an unquoted vector using the \code{v()} function.
#' \itemize{
#'   \item{\strong{css}: Corrected Sum of Squares.}
#'   \item{\strong{clm, lclm, uclm}: Upper and lower confidence limits.}
#'   \item{\strong{cv}: Coefficient of Variation.}
#'   \item{\strong{kurt/kurtosis}: The Kurtosis is a description of the
#'   distribution tails. It requires at least 4 complete observations.}
#'   \item{\strong{mean}: The arithmetic mean.}
#'   \item{\strong{median}: The median.}
#'   \item{\strong{mode}: The mode of the target variable.}
#'   \item{\strong{min, max}: The minimum and maximum values of the target
#'   variable.}
#'   \item{\strong{n}: The number of non-missing observations.}
#'   \item{\strong{nmiss}: The number of missing observations.}
#'   \item{\strong{nobs}: The number of observations, whether
#'   missing or non-missing.}
#'   \item{\strong{p1 - p99}: Percentile ranges from p1 to p99, in increments
#'   of 5.}
#'   \item{\strong{qrange, q1, q3}: Quantile ranges for the first and third quantiles.}
#'   \item{\strong{range}: Difference between the minimum and maximum values.}
#'   \item{\strong{skew/skewness}: A measure of distribution skewness. It
#'   requires at least 3 complete observations.}
#'   \item{\strong{std/stddev}: Standard deviation.}
#'   \item{\strong{stderr}: Standard error.}
#'   \item{\strong{sum}: The sum of variable values.}
#'   \item{\strong{uss}: Uncorrected sum of squares.}
#'   \item{\strong{vari}: The variance.}
#' }
#' The function supports the following keywords to perform hypothesis testing:
#' \itemize{
#'   \item{\strong{t}: Student's t statistic.}
#'   \item{\strong{prt/probt}: A two-tailed p-value for the Student's t statistic.}
#'   \item{\strong{df}: Degrees of freedom for the Student's t statistic.}
#' }
#'
#' @section Options:
#' The \code{proc_means} function recognizes the following options.  Options may
#' be passed as a quoted vector of strings, or an unquoted vector using the
#' \code{v()} function.
#' \itemize{
#' \item{\strong{alpha = }: The "alpha = " option will set the alpha
#' value for confidence limit statistics.  Set the alpha as a decimal value
#' between 0 and 1.  For example, you can set a 90% confidence limit as
#' \code{alpha = 0.1}.
#' }
#' \item{\strong{completetypes}: The "completetypes" option will generate
#' all combinations of the class variable, even if there is no data
#' present for a particular level.
#' Combinations will be distinguished by the TYPE variable. To use the "completetypes"
#' option, define the class variable(s) as a factor.
#' }
#' \item{\strong{maxdec = }: The "maxdec = " option will set the maximum
#' of decimal places displayed on report output. For example, you can set 4 decimal
#' places as follows: \code{maxdec = 4}.  Default is 7 decimal places.
#' This option will not round any values on the output dataset.
#' }
#' \item{\strong{nofreq, nonobs}: Turns off the FREQ column
#' on the output datasets.
#' }
#' \item{\strong{noprint}: Whether to print the interactive report to the
#' viewer.  By default, the report is printed to the viewer. The "noprint"
#' option will inhibit printing.
#' }
#' \item{\strong{notype}: Turns off the TYPE column on the output
#' dataset.
#' }
#' \item{\strong{nway}: Returns only the highest level TYPE combination.  By
#' default, the function returns all TYPE combinations.
#' }
#' \item{\strong{vardef}: Controls the denominator used in variance-related
#' statistics. This option supports several denominator definitions:
#'   \itemize{
#'     \item{\strong{DF}: Uses \(n - 1\), the sample degrees of freedom.}
#'     \item{\strong{N}: Uses \(n\), the total count of observations.}
#'     \item{\strong{WEIGHT/WGT}: Uses the sum of weights.}
#'     \item{\strong{WDF}: Uses the sum of weights minus one.}
#'   }
#' By default, this option use \strong{DF}.
#' }
#' }
#' @section TYPE and FREQ Variables:
#' The TYPE and FREQ variables appear on the output dataset by default.
#'
#' The FREQ variable contains a count of the number of input rows/observations that were
#' included in the statistics for that output row. The FREQ count can be different
#' from the N statistic. The FREQ count is a count of the number of rows/observations,
#' while the N statistic is a count of non-missing values.  These counts can
#' be different if you have missing values in your data.  If you want to remove
#' the FREQ column from the output dataset, use the "nofreq" option.
#'
#' The TYPE variable identifies combinations of class categories, and produces
#' summary statistics for each of those combinations.  For example, the
#' output dataset normally produces statistics for TYPE 0, which is all
#' class categories, and a TYPE 1 which is each class category.  If there
#' are multiple classes, there will be multiple TYPE values for each level
#' of class combinations.  If you do no want to show the various
#' type combinations, use the "nway" option. If you want to remove the TYPE
#' column from the output dataset, use the "notype" option.
#'
#' @section Using Factors:
#' There are some occasions when you may want to define the \code{class} variable(s)
#' as a factor. One occasion is for sorting/ordering,
#' and the other is for obtaining zero-counts on sparse data.
#'
#' To order the class categories in the means output, define the
#' \code{class} variable as a factor in the desired order. The function will
#' then retain that order for the class categories in the output dataset
#' and report.
#'
#' You may also wish to
#' define the class variable as a factor if you are dealing with sparse data
#' and some of the class categories are not present in the data. To ensure
#' these categories are displayed with zero-counts, define the \code{class} variable
#' as a factor and use the "completetypes" option.
#' @section Data Shaping:
#' The output dataset produced by the "out" keyword can be shaped
#' in different ways. These shaping options allow you to decide whether the
#' data should be returned long and skinny, or short and wide. The shaping
#' options can reduce the amount of data manipulation necessary to get the
#' frequencies into the desired form. The
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
# @section Data Constraints:
# Explain limits of data for each stat option.  Number of non-missing
# values, etc.
#'
#' @param data The input data frame for which to calculate summary statistics.
#' This parameter is required.
#' @param var The variable(s) to calculate summary statistics for. If no
#' variables are specified,
#' summary statistics will be generated for all numeric variables on the
#' input data frame.
#' @param stats A vector of summary statistics keywords.  Valid
#' keywords are: "css", "clm", "cv", "kurt", "kurtosis",
#' "lclm", "mean", "median", "mode",
#' "min", "max", "n",
#' "nmiss", "nobs",
#' "p1", "p5", "p10", "p20", "p25", "p30", "p40",
#' "p50", "p60", "p70", "p75", "p80", "p90",
#' "p95", "p99", "q1", "q3", "qrange", "range", "skew", "skewness",
#' "std", "stddev", "stderr", "sum",
#' "uclm", "uss", and "vari". For hypothesis testing, the function
#' supports "t", "prt", "probt", and "df".
#' Default statistics are: "n", "mean", "std",
#' "min", and "max".
#' @param output Whether or not to return datasets from the function. Valid
#' values are "out", "none", and "report".  Default is "out", and will
#' produce dataset output specifically designed for programmatic use. The "none"
#' option will return a NULL instead of a dataset or list of datasets.
#' The "report" keyword returns the datasets from the interactive report, which
#' may be different from the standard output. The output parameter also accepts
#' data shaping keywords "long, "stacked", and "wide".
#' The shaping keywords control the structure of the output data. See the
#' \strong{Data Shaping} section for additional details. Note that
#' multiple output keywords may be passed on a
#' character vector. For example,
#' to produce both a report dataset and a "long" output dataset,
#' use the parameter \code{output = c("report", "out", "long")}.
#' @param by An optional by group. If you specify a by group, the input
#' data will be subset on the by variable(s) prior to performing any
#' statistics.
#' @param class The \code{class} parameter is similar to the \code{by}
#' parameter, but the output is different.  By groups will create completely
#' separate tables, while class groups will be continued in the same table.
#' When a \code{by} and a \code{class} are both specified, the \code{class}
#' will be nested in the \code{by}.
#' @param weight The name of a variable to use for weighted statistics.
#' This parameter is optional, and NULL by default.
#' The weight can be used to
#' calculate weighted means, sums, variances, etc. Note that any observations with
#' NA values for the weight will be excluded from the analysis.  Also observe
#' the "vardef" option for weighted variances, as this option can effect
#' which denominator is used.
#' @param options A vector of optional keywords. Valid values are: "alpha =",
#' "completetypes", "maxdec =", "noprint", "notype", "nofreq", "nonobs", "nway",
#' "vardef=". The "notype", "nofreq" and "nonobs"  keywords will turn
#' off columns on the output datasets.  The "alpha = " option will set the alpha
#' value for confidence limit statistics.  The default is 95% (alpha = 0.05).
#' The "maxdec = " option sets the maximum number of decimal places displayed
#' on report output. The "nway" option returns only the highest type values.
#' The "vardef=" option specifies the variance divisor.
#' @param titles A vector of one or more titles to use for the report output.
#' @return Normally, the requested summary statistics are shown interactively
#' in the viewer, and output results are returned as a data frame.
#' If the request produces multiple data frames, they will be returned in a list.
#' You may then access individual datasets from the list.
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
#' # Default statistics on iris
#' res1 <- proc_means(iris)
#'
#' # View results
#' res1
#' #   TYPE FREQ          VAR   N     MEAN       STD MIN MAX
#' # 1    0  150 Sepal.Length 150 5.843333 0.8280661 4.3 7.9
#' # 2    0  150  Sepal.Width 150 3.057333 0.4358663 2.0 4.4
#' # 3    0  150 Petal.Length 150 3.758000 1.7652982 1.0 6.9
#' # 4    0  150  Petal.Width 150 1.199333 0.7622377 0.1 2.5
#'
#' # Defaults statistics with by
#' res2 <- proc_means(iris,
#'                    by = Species)
#' # View results
#' res2
#' #            BY TYPE FREQ          VAR  N  MEAN       STD MIN MAX
#' # 1      setosa    0   50 Sepal.Length 50 5.006 0.3524897 4.3 5.8
#' # 2      setosa    0   50  Sepal.Width 50 3.428 0.3790644 2.3 4.4
#' # 3      setosa    0   50 Petal.Length 50 1.462 0.1736640 1.0 1.9
#' # 4      setosa    0   50  Petal.Width 50 0.246 0.1053856 0.1 0.6
#' # 5  versicolor    0   50 Sepal.Length 50 5.936 0.5161711 4.9 7.0
#' # 6  versicolor    0   50  Sepal.Width 50 2.770 0.3137983 2.0 3.4
#' # 7  versicolor    0   50 Petal.Length 50 4.260 0.4699110 3.0 5.1
#' # 8  versicolor    0   50  Petal.Width 50 1.326 0.1977527 1.0 1.8
#' # 9   virginica    0   50 Sepal.Length 50 6.588 0.6358796 4.9 7.9
#' # 10  virginica    0   50  Sepal.Width 50 2.974 0.3224966 2.2 3.8
#' # 11  virginica    0   50 Petal.Length 50 5.552 0.5518947 4.5 6.9
#' # 12  virginica    0   50  Petal.Width 50 2.026 0.2746501 1.4 2.5
#'
#' # Specified variables, statistics, and options
#' res3 <- proc_means(iris,
#'                    var = v(Petal.Length, Petal.Width),
#'                    class = Species,
#'                    stats = v(n, mean, std, median, qrange, clm),
#'                    options = nofreq,
#'                    output = long)
#' # View results
#' res3
#' #         CLASS TYPE   STAT Petal.Length Petal.Width
#' # 1        <NA>    0      N  150.0000000 150.0000000
#' # 2        <NA>    0   MEAN    3.7580000   1.1993333
#' # 3        <NA>    0    STD    1.7652982   0.7622377
#' # 4        <NA>    0 MEDIAN    4.3500000   1.3000000
#' # 5        <NA>    0 QRANGE    3.5000000   1.5000000
#' # 6        <NA>    0   LCLM    3.4731854   1.0763533
#' # 7        <NA>    0   UCLM    4.0428146   1.3223134
#' # 8      setosa    1      N   50.0000000  50.0000000
#' # 9      setosa    1   MEAN    1.4620000   0.2460000
#' # 10     setosa    1    STD    0.1736640   0.1053856
#' # 11     setosa    1 MEDIAN    1.5000000   0.2000000
#' # 12     setosa    1 QRANGE    0.2000000   0.1000000
#' # 13     setosa    1   LCLM    1.4126452   0.2160497
#' # 14     setosa    1   UCLM    1.5113548   0.2759503
#' # 15 versicolor    1      N   50.0000000  50.0000000
#' # 16 versicolor    1   MEAN    4.2600000   1.3260000
#' # 17 versicolor    1    STD    0.4699110   0.1977527
#' # 18 versicolor    1 MEDIAN    4.3500000   1.3000000
#' # 19 versicolor    1 QRANGE    0.6000000   0.3000000
#' # 20 versicolor    1   LCLM    4.1264528   1.2697993
#' # 21 versicolor    1   UCLM    4.3935472   1.3822007
#' # 22  virginica    1      N   50.0000000  50.0000000
#' # 23  virginica    1   MEAN    5.5520000   2.0260000
#' # 24  virginica    1    STD    0.5518947   0.2746501
#' # 25  virginica    1 MEDIAN    5.5500000   2.0000000
#' # 26  virginica    1 QRANGE    0.8000000   0.5000000
#' # 27  virginica    1   LCLM    5.3951533   1.9479453
#' # 28  virginica    1   UCLM    5.7088467   2.1040547
#'
proc_means <- function(data,
                       var = NULL,
                       stats = c("n", "mean", "std", "min", "max"),
                       output = NULL,
                       by = NULL,
                       class = NULL,
                       weight = NULL,
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

  oweight <- deparse(substitute(weight, env = environment()))
  weight <- tryCatch({if (typeof(weight) %in% c("character", "NULL")) weight else oweight},
                     error = function(cond) {oweight})

  ostats <- deparse(substitute(stats, env = environment()))
  stats <- tryCatch({if (typeof(stats) %in% c("character", "NULL")) stats else ostats},
                    error = function(cond) {ostats})

  oopt <- deparse(substitute(options, env = environment()))
  options <- tryCatch({if (typeof(options) %in% c("integer", "double", "character", "NULL")) options else oopt},
                      error = function(cond) {oopt})

  oout <- deparse(substitute(output, env = environment()))
  output <- tryCatch({if (typeof(output) %in% c("character", "NULL")) output else oout},
                     error = function(cond) {oout})

  # Parameter checks

  if (!"data.frame" %in% class(data)) {
    stop("Input data is not a data frame.")
  }

  if (nrow(data) == 0) {
    stop("Input data has no rows.")
  }

  nms <- names(data)

  if (is.null(var)) {
    var <- c()
    for (nm in nms) {
      if (is.numeric(data[[nm]])) {

        var[length(var) + 1] <- nm
      }

    }
  }

  var <- var[!var %in% c(by, class)]

  if (!is.null(by)) {
    if (!all(by %in% nms)) {

      stop(paste("Invalid by name: ", by[!by %in% nms], "\n"))
    }
  }

  if (is.null(var)) {
    stop("var parameter is required.")
  } else {
    if (!all(var %in% nms)) {

      stop(paste("Invalid variable name: ", var[!var %in% nms], "\n"))
    }
  }
  if (!is.null(weight)){
   if (!all(weight %in% nms)) {

      stop(paste("Invalid weight variable name: ", weight[!weight %in% nms], "\n"))

    }else if (!is.numeric(data[[weight[1]]])){

      stop("Weight variable must be numeric.\n")
    }
  }


  if (!is.null(class)) {
    if (!all(class %in% nms)) {

      stop(paste("Invalid class name: ", class[!class %in% nms], "\n"))
    }
  }

  if (is.null(stats)) {
    stop("stats parameter is required.")
  } else {
    st <- c( "css", "cv", "n", "mean", "median", "std", "stddev", "min", "max",
             "nmiss", "nobs", "range", "sum", "stderr", "vari", "clm", "uclm",
             "lclm","mode", "q1", "q3", "p1", "p5", "p10", "p20",
             "p25", "p30", "p40",
             "p50", "p60", "p70", "p75", "p80", "p90",
             "p95", "p99", "qrange", "t", "prt", "probt", "df", "uss", "skew",
             "skewness", "kurt", "kurtosis")
    if (!all(tolower(stats) %in% st)) {

      stop(paste("Invalid stat name: ", stats[!tolower(stats) %in% st], "\n"))
    }
  }

  if (!is.null(output)) {
    outs <- c("out", "report", "none", "wide", "long", "stacked")
    if (!all(tolower(output) %in% outs)) {

      stop(paste("Invalid output keyword: ", output[!tolower(output) %in% outs], "\n"))
    }

  }

  if (!is.null(options)) {
    kopts <- c("alpha", "completetypes", "long", "maxdec",
               "noprint", "notype", "nofreq", "nonobs", "nway", "vardef")

    # Deal with "alpha =" and "maxdec = " by using name instead of value
    nopts <- names(options)
    mopts <- ifelse(nopts == "", options, nopts)


    if (!all(tolower(mopts) %in% kopts)) {

      stop(paste("Invalid options keyword: ", mopts[!tolower(mopts) %in% kopts], "\n"))
    }
  }


  # Generate output specs
  if (has_output(output))
    outreq <- get_output_specs_means(list(), stats, options, output)
  else
    outreq <- NULL

  rptflg <- FALSE
  rptnm <- ""
  rptres <- NULL

  # Kill output request for report
  # Otherwise, this will mess up gen_output_means
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
    rptres <- gen_report_means(data, by = by, var = var, class = class,
                               stats = stats, view = view,
                               titles = titles, weight = weight,
                               opts = options)
  }

  # Get output datasets if requested
  if (length(outreq) > 0) {
    res <- gen_output_means(data,
                            by = by,
                            class = class,
                            var = var,
                            weight = weight,
                            output = outreq,
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
  log_means(data,
            by = by,
            class = class,
            var = var,
            stats = stats,
            output = output,
            weight = weight,
            view = view,
            titles = titles,
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


log_means <- function(data,
                      by = NULL,
                      class = NULL,
                      var = NULL,
                      stats = NULL,
                      output = NULL,
                      weight = NULL,
                      view = TRUE,
                      titles = NULL,
                      opts = NULL,
                      outcnt = NULL) {

  ret <- c()

  indt <- paste0(rep(" ", 12), collapse = "")

  ret <- paste0("proc_means: input data set ", nrow(data),
                " rows and ", ncol(data), " columns")

  if (!is.null(by))
    ret[length(ret) + 1] <- paste0(indt, "by: ", paste(by, collapse = " "))

  if (!is.null(class))
    ret[length(ret) + 1] <- paste0(indt, "class: ",
                                   paste(class, collapse = " "))

  if (!is.null(var))
    ret[length(ret) + 1] <- paste0(indt, "var: ",
                                   paste(var, collapse = " "))

  if (!is.null(weight))
    ret[length(ret) + 1] <- paste0(indt, "weight: ",
                                   paste(weight, collapse = " "))

  if (!is.null(stats))
    ret[length(ret) + 1] <- paste0(indt, "stats: ",
                                   paste(stats, collapse = " "))

  if (!is.null(output))
    ret[length(ret) + 1] <- paste0(indt, "output: ", paste(output, collapse = " "))

  if (!is.null(weight))
    ret[length(ret) + 1] <- paste0(indt, "weight: ", paste(weight, collapse = " "))

  if (!is.null(view))
    ret[length(ret) + 1]<- paste0(indt, "view: ", paste(view, collapse = " "))

  if (!is.null(titles))
    ret[length(ret) + 1] <- paste0(indt, "titles: ", paste(titles, collapse = "\n"))


  if (!is.null(outcnt))
    ret[length(ret) + 1] <- paste0(indt, "output: ", outcnt, " datasets")


  log_logr(ret)

}

# Stats ---------------------------------------------------------------


get_output_specs_means <- function(outs, stats, opts, output) {

  # outreq <- NULL

  # Set default statistics for output parameters
  # if (!is.null(outs)) {
  #   outreq <- outs  # need to check this
  #
  # } else {
  outreq <- outs
  if (length(outreq) >= 1) {
    # for (nm in names(outreq)) {
    #   if ("out_req" %in% class(outreq[[nm]])) {
    #     if (is.null(outreq[[nm]]$stats)) {
    #
    #       outreq[[nm]]$stats <- stats
    #     }
    #     if (is.null(outreq[[nm]]$shape)) {
    #
    #       outreq[[nm]]$shape <- "wide"
    #     }
    #   } else {
    #
    #     warning("proc_means: Unknown parameter '" %p% nm %p% "'")
    #     outreq[[nm]] <- out_spec(shape = "wide")
    #   }
    # }
  } else {

    if (option_true(output, "long")) {
      outreq[["out"]] <- out_spec(stats = stats, shape = "long",
                                  type = TRUE, freq = TRUE)
    } else if (option_true(output, "stacked")) {
      outreq[["out"]] <- out_spec(stats = stats, shape = "stacked",
                                  type = TRUE, freq = TRUE)
    } else {
      outreq[["out"]] <- out_spec(stats = stats, shape = "wide",
                                  type = TRUE, freq = TRUE)
    }

  }
  # }

  return(outreq)

}


# Purpose of this function is to prepare data for output.
# It will append the by, class, type, and freq columns to the subset df.
# These subsets will get stacked up in the driver function.
get_output <- function(data, var, weight = NULL, stats, missing = FALSE,
                       shape = "wide", type = NULL, freq = FALSE,
                       by = NULL, class = NULL, opts = NULL,
                       keep_names = FALSE) {

  if (is.null(stats))
    stats <- c("n", "mean", "std", "min", "max")

  ret <- get_summaries(data, var, weight, stats, missing = missing,
                       shape = shape, opts = opts)

  if (freq)
    ret <- cbind(data.frame(FREQ = nrow(data)), ret)


  if (!is.null(type))
    ret <- cbind(data.frame(TYPE = type), ret)

  if (!is.null(class)) {

    # Create a new vector of class names and values
    cv <- class

    if (keep_names) {
      cnms <- names(class)
    } else {
      if (length(cv) == 1)
        cnms <- "CLASS"
      else
        cnms <- paste0("CLASS", seq(1, length(cv)))

    }

    names(cv) <- cnms

    # Put class names and values in a list so it
    # can be easily turned into a data frame.
    tmp <- list()
    for(nm in cnms) {
      tmp[[nm]] <- cv[nm]
    }

    # Convert to data frame
    df <- as.data.frame(tmp, stringsAsFactors = FALSE)

    # Clear out rownames
    rownames(df) <- NULL

    # Bind class columns to statistics
    ret <- cbind(df, ret)
  }

  if (!is.null(by)) {

    # Create a new vector of by names and values
    bv <- by

    if (length(bv) == 1)
      bnms <- "BY"
    else
      bnms <- paste0("BY", seq(1, length(bv)))

    names(bv) <- bnms

    tmp <- list()
    for(nm in names(bv)) {
      tmp[[nm]] <- bv[nm]
    }

    df <- as.data.frame(tmp, stringsAsFactors = FALSE)

    rownames(df) <- NULL

    ret <- cbind(df, ret)
  }


  return(ret)
}

# This is where most of the summary statistics get calculated.
get_summaries <- function(data, var, weight=NULL, stats, missing = FALSE,
                          shape = "wide", opts = NULL) {

  narm <- TRUE
  ret <- NULL
  vardef <- tolower(get_option(opts, "vardef", "df"))
  if (vardef=="wgt") vardef <- "weight"

  for (nm in var) {

    w <- NULL

    if (!nm %in% names(data)) {

      stop(paste0("Variable '", nm, "' not found on input dataset."))
    } else {
      sts <- tolower(stats)
      rw <- data.frame(VAR = nm, stringsAsFactors = FALSE)
      var <- data[[nm]]
      #create one copy of variable to use for some statistic calculation
      var_all <- var

      #adjust variable by weight if specified
      if (!is.null(weight)) {

        #create one copy of variable to use for some statistic calculation
        keep <- !is.na(data[[weight]])
        var_all <- var[keep]

        #create w and var vector for stats calculations
        keep <- !is.na(data[[weight]]) & !is.na(var) & data[[weight]] > 0
        w = data[[weight]][keep]
        var <- var[keep]

      } else if (vardef == "weight" || vardef == "wdf") {
        # If weight is not specified, but vardef is weight or wdf, then we will use equal weights for all non-missing values.
        keep <- !is.na(var_all)
        w <- rep(1, sum(keep))
        var <- var[keep]

      }

      # compute denominator

      denom <- switch(
        vardef,
        df = sum(!is.na(var)) - 1,
        n  = sum(!is.na(var)),
        weight = sum(w),
        wdf = sum(w) - 1,
        stop(paste0("Unexpected vardef value ", vardef,"'"))
      )

      #browser()

      for (st in sts) {

        if (st == "n") {

          rw[["N"]] <- sum(!is.na(var_all))
        }

        if (st == "css") {

          if (all(is.na(var)))
            rw[["CSS"]] <- NA
          else if (is.null(weight))
            rw[["CSS"]] <- sum((var - mean(var, na.rm = narm))^2, na.rm = narm)
          else
            rw[["CSS"]] <- sum(w*(var - sum(var*w, na.rm = narm)/sum(w))^2, na.rm = narm)
        }

        if (st == "cv") {

          if (all(is.na(var)))
            rw[["CV"]] <- NA
          else if (is.null(weight))
             rw[["CV"]] <- sqrt(get_variance(var, denom, w, narm))/ mean(var, na.rm = narm) * 100
          else
            rw[["CV"]] <- sqrt(get_variance(var, denom, w, narm))/ (sum(var*w, na.rm = narm)/sum(w)) * 100
        }

        if (st == "mean") {

          if (all(is.na(var)))
            rw[["MEAN"]] <- NA
          else if (is.null(weight))
             rw[["MEAN"]] <- mean(var, na.rm = narm)
          else
            rw[["MEAN"]] <- sum(var*w, na.rm = narm)/sum(w)
        }

        if (st == "mode") {

          if (all(is.na(var)))
            rw[["MODE"]] <- NA
          else if (!is.null(weight))
            rw[["MODE"]] <- NA
          else
            rw[["MODE"]] <- get_mode(var)
        }

        if (st == "max") {
          if (all(is.na(var)))
            rw[["MAX"]] <- NA
          else
            rw[["MAX"]] <- max(var, na.rm = narm)
        }

        if (st == "min") {

          if (all(is.na(var)))
            rw[["MIN"]] <- NA
          else
            rw[["MIN"]] <- min(var, na.rm = narm)
        }

        if (st == "median") {

          if (all(is.na(var)))
            rw[["MEDIAN"]] <- NA
          else if (is.null(weight))
            rw[["MEDIAN"]] <- median(var, na.rm = narm)
          else
            rw[["MEDIAN"]] <- get_weighted_quantile(var, w, probs = c(0.5), narm = narm)
        }

        if (st == "nobs") {

          rw[["NOBS"]] <- nrow(data)
        }

        if (st == "nmiss") {

          rw[["NMISS"]] <- sum(is.na(var_all))
        }

        if (st %in% c("std", "stddev")) {

          if (all(is.na(var)))
            rw[["STD"]] <- NA
          else
            rw[["STD"]] <- sqrt(get_variance(var, denom, w, narm))
        }

        if (st == "sum") {

          if (all(is.na(var)))
            rw[["SUM"]] <- NA
          else
            rw[["SUM"]] <- sum(var, na.rm = narm)

        }

        if (st == "range") {

          if (all(is.na(var_all))) {
            rw[["RANGE"]] <- NA
          } else {
            rng <- range(var_all, na.rm = narm)
            if (!is.null(rng) & length(rng) == 2)
              rw[["RANGE"]] <- rng[2] - rng[1]
            else
              rw[["RANGE"]] <- NA
          }
        }

        if (st == "vari") {

          rw[["VARI"]] <- get_variance(var, denom, w, narm)
        }

        if (st == "stderr") {
          if (vardef == "df")
            rw[["STDERR"]] <- get_stderr(var, denom, w, narm)
          else
            rw[["STDERR"]] <- NA

        }


        # Check for two-sided CLM
        if (st == "clm" || all(c("lclm", "uclm") %in% sts)) {

          alph <- get_alpha(opts)
          tmp <- get_clm(var, denom, w, narm, alph)


          if (vardef == "df"){
            rw[["LCLM"]] <- tmp[["lcl"]]
            rw[["UCLM"]] <- tmp[["ucl"]]
          } else {
            rw[["LCLM"]] <- NA
            rw[["UCLM"]] <- NA
          }


        } else {

          # Check for one-sided LCLM
          if (st == "lclm") {

            alph <- get_alpha(opts)

            tmp <- get_clm(var, denom, w, narm, alph, onesided = TRUE)

            if (vardef == "df")
              rw[["LCLM"]] <- tmp[["lcl"]]
            else
              rw[["LCLM"]] <- NA


          }

          # Check for one-sided UCLM
          if (st == "uclm") {


            alph <- get_alpha(opts)

            tmp <- get_clm(var, denom, w, narm, alph, onesided = TRUE)

            if (vardef == "df")
              rw[["UCLM"]] <- tmp[["ucl"]]
            else
              rw[["UCLM"]] <- NA
          }
        }

        if (st == "clmstd") {

          alph <- get_alpha(opts)

          tmp <- get_clmstd(var, denom, w, narm, alph)

          if (vardef == "df"){
            rw[["LCLMSTD"]] <- tmp[["lcl"]]
            rw[["UCLMSTD"]] <- tmp[["ucl"]]
          } else {
            rw[["LCLMSTD"]] <- NA
            rw[["UCLMSTD"]] <- NA
          }
        }

        if (st == "t") {

          alph <- get_alpha(opts)

          tmp <- get_t(var, denom, w, alpha = alph)

          if (vardef == "df")
            rw[["T"]] <- tmp[["T"]]
          else
            rw[["T"]] <- NA
        }

        if (st == "prt") {

          alph <- get_alpha(opts)

          tmp <- get_t(var, denom, w, alpha = alph)

          if (vardef == "df")
            rw[["PRT"]] <- tmp[["PRT"]]
          else
            rw[["PRT"]] <- NA

        }

        if (st == "probt") {

          alph <- get_alpha(opts)

          tmp <- get_t(var, denom, w, alpha = alph)

          if (vardef == "df")
            rw[["PROBT"]] <- tmp[["PRT"]]
          else
            rw[["PROBT"]] <- NA
        }

        if (st == "df") {

          alph <- get_alpha(opts)

          tmp <- get_t(var, denom, w, alpha = alph)

          rw[["DF"]] <- tmp[["DF"]]

        }


        if (st == "uss") {
          if (is.null(w))
             rw[["USS"]] <- sum(var^2, na.rm = narm)
          else
            rw[["USS"]] <-  sum(w*var^2, na.rm = narm)
        }

        if (st == "p1") {

          rw[["P1"]] <- get_weighted_quantile(var, w, probs = c(0.01), narm = narm)

        }

        if (st == "p5") {

          rw[["P5"]] <- get_weighted_quantile(var, w, probs = c(0.05), narm = narm)

        }

        if (st == "p10") {

          rw[["P10"]] <- get_weighted_quantile(var, w, probs = c(0.1), narm = narm)

        }

        if (st == "p20") {

          rw[["P20"]] <- get_weighted_quantile(var, w, probs = c(0.2), narm = narm)

        }

        if (st == "p25") {

          rw[["P25"]] <- get_weighted_quantile(var, w, probs = c(0.25), narm = narm)

        }

        if (st == "q1") {

          rw[["Q1"]] <- get_weighted_quantile(var, w, probs = c(0.25), narm = narm)

        }

        if (st == "p30") {

          rw[["P30"]] <- get_weighted_quantile(var, w, probs = c(0.3), narm = narm)

        }
        if (st == "p40") {

          rw[["P40"]] <- get_weighted_quantile(var, w, probs = c(0.4), narm = narm)

        }

        if (st == "p50") {

          rw[["P50"]] <- get_weighted_quantile(var, w, probs = c(0.5), narm = narm)

        }

        if (st == "p60") {

          rw[["P60"]] <- get_weighted_quantile(var, w, probs = c(0.6), narm = narm)

        }
        if (st == "p70") {

          rw[["P70"]] <- get_weighted_quantile(var, w, probs = c(0.7), narm = narm)

        }

        if (st == "p75") {

          rw[["P75"]] <- get_weighted_quantile(var, w, probs = c(0.75), narm = narm)

        }

        if (st == "q3") {

          rw[["Q3"]] <- get_weighted_quantile(var, w, probs = c(0.75), narm = narm)

        }

        if (st == "p80") {

          rw[["P80"]] <- get_weighted_quantile(var, w, probs = c(0.8), narm = narm)

        }

        if (st == "p90") {

          rw[["P90"]] <- get_weighted_quantile(var, w, probs = c(0.9), narm = narm)

        }

        if (st == "p95") {

          rw[["P95"]] <- get_weighted_quantile(var, w, probs = c(0.95), narm = narm)

        }

        if (st == "p99") {

          rw[["P99"]] <- get_weighted_quantile(var, w, probs = c(0.99), narm = narm)

        }

        if (st == "qrange") {

          q25 <- get_weighted_quantile(var, w, probs = c(0.25), narm = narm)
          q75 <- get_weighted_quantile(var, w, probs = c(0.75), narm = narm)

          rw[["QRANGE"]] <- q75 - q25

        }

        if (st %in% c("skew", "skewness")) {

          if (is.null(w))
            rw[["SKEW"]] <- get_skewness(var,denom)
          else
            rw[["SKEW"]] <- NA

        }

        if (st %in% c("kurt", "kurtosis")) {

          if (is.null(w))
            rw[["KURT"]] <- get_kurtosis(var,denom)
          else
            rw[["KURT"]] <- NA

        }


      }

    }

    if (is.null(ret))
      ret <- rw
    else
      ret <- rbind(ret, rw)
  }

  if (!is.null(shape)) {
    ret <- shape_means_data(ret, shape)
  }


  return(ret)

}

shape_means_data <- function(ds, shape, copy = NULL) {

  # Assumed to be wide
  ret <- ds

  if (!is.null(shape)) {
    if (all(shape == "long")) {

      ret <- proc_transpose(ret, id = "VAR", name = "STAT",
                            copy = copy, log = FALSE)


    } else if (all(shape == "stacked")) {

      ret <- proc_transpose(ret, name = "STAT", by = "VAR",
                            copy = copy, log = FALSE)

      rnms <- names(ret)
      rnms[rnms %in% "COL1"] <- "VALUES"

      names(ret) <- rnms

    }
  }

  return(ret)

}


# Drivers --------------------------------------------------------------------
mlbls <- list(MEAN = "Mean", STD = "Std Dev", MEDIAN = "Median", MIN = "Minimum",
              MAX = "Maximum", VAR = "Variable", STDERR = "Std Err",
              STAT = "Statistics", VARI = "Variance", QRANGE = "Quartile Range",
              RANGE = "Range", MODE = "Mode", NMISS = "NMiss",
              LCLM = "Lower %s%% CL for Mean", UCLM = "Upper %s%% CL for Mean",
              TYPE = "Type", FREQ = "Frequency", SUM = "Sum", "T" = "t Value",
              PRT = "Pr > |t|", PROBT = "Pr > |t|", DF = "DF",
              USS = "Uncorrected SS", CV = "Coeff of Variation",
              CSS = "Corrected SS", VARI = "Variance", SKEW = "Skewness",
              KURT = "Kurtosis", P1 = "1st Pctl", P5 = "5th Pctl", P10 = "10th Pctl",
              P20 = "20th Pctl", P25 = "25th Pctl", P30 = "30th Pctl",
              P40 = "40th Pctl", P50 = "50th Pctl", P60 = "60th Pctl",
              P70 = "70th Pctl", P75 = "75th Pctl", P80 = "80th Pctl",
              P90 = "90th Pctl", P95 = "95th Pctl", P99 = "99th Pctl",
              Q1 = "Lower Quartile", Q3 = "Upper Quartile",
              UCLMSTD = "Upper %s%% CL for Std Dev",
              LCLMSTD = "Lower %s%% CL for Std Dev"
)

#' @import common
#' @import tibble
gen_report_means <- function(data,
                             by = NULL,
                             class = NULL,
                             var = NULL,
                             stats = c("n", "mean", "std", "min", "max"),
                             weight = NULL,
                             opts = NULL,
                             view = TRUE,
                             titles = NULL) {

  # Declare return list
  res <- list()

  # Assign CL Percentage on Labels
  alph <- (1 - get_alpha(opts)) * 100
  mlbls[["UCLM"]] <- sprintf(mlbls[["UCLM"]], alph)
  mlbls[["LCLM"]] <- sprintf(mlbls[["LCLM"]], alph)
  mlbls[["UCLMSTD"]] <- sprintf(mlbls[["UCLMSTD"]], alph)
  mlbls[["LCLMSTD"]] <- sprintf(mlbls[["LCLMSTD"]], alph)

  #browser()

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

    # data, var, class, outp, freq = TRUE,
    # type = NULL, byvals = NULL
    outp <- out_spec(stats = stats, shape = "wide")
    smtbl <- get_class_report(dt, var, class, outp, freq = FALSE, weight=weight, opts = opts)

    nm <- length(res) + 1

    # Add spanning headers if there are by groups
    if (!is.null(by) & !is.null(smtbl)) {

      # Add spanning headers
      spn <- span(1, ncol(smtbl), label = bylbls[j], level = 1)
      attr(smtbl, "spans") <- list(spn)

      nm <-  bylbls[j]
    }

    # Add default formats
    fmt <- get_maxdec(opts)
    for (cnm in names(smtbl)) {

      if (typeof(smtbl[[cnm]]) %in% c("double")) {

        attr(smtbl[[cnm]], "format") <- fmt
      }

    }


    # Assign labels
    if (is.null(class))
      labels(smtbl) <- mlbls
    else {

      cv <- class
      if (length(class) == 1)
        cnms <- "CLASS"
      else
        cnms <- paste0("CLASS", seq(1, length(class)))

      names(cv) <- cnms

      labels(smtbl) <- append(as.list(cv), mlbls)

    }

    # Convert to tibble if incoming data is a tibble
    if ("tbl_df" %in% class(data)) {
      res[[nm]] <- as_tibble(smtbl)
    } else {
      res[[nm]] <- smtbl
    }

  }

  gv <- options("procs.print")[[1]]
  if (is.null(gv))
    gv <- TRUE

  # Create viewer report if requested
  if (gv) {
    if (view == TRUE && interactive()) {


      vrfl <- tempfile()

      if (is.null(titles)) {
        titles <- "The MEANS Function"
      }

      out <- output_report(res, dir_name = dirname(vrfl),
                           file_name = basename(vrfl), out_type = "HTML",
                           titles = titles, margins = .5, viewer = TRUE)

      show_viewer(out)
    }
  }

  if (length(res) == 1)
    res <- res[[1]]

  return(res)

}



get_class_report <- function(data, var, class, outp, freq = TRUE,
                             type = NULL, byvals = NULL, weight = NULL, opts = NULL) {


  res <- NULL
  aovds <- NULL

  clslist <- list(data)
  cnms <- NULL
  if (!is.null(class)) {

    if (has_option(opts, "completetypes"))
      clslist <- split(data, data[ , class], sep = "|", drop = FALSE)
    else
      clslist <- split(data, data[ , class], sep = "|", drop = TRUE)

    cnms <- names(clslist)
    if (length(clslist) == 0) {

      clslist <- list(data)
      cnms <- class

    }

  }




  for (k in seq_len(length(clslist))) {

    cnmv <- NULL
    if (!is.null(cnms)) {
      cnmv <- strsplit(cnms[k], "|", fixed = TRUE)[[1]]
      names(cnmv) <- class

    }


    tmpres <- get_output(clslist[[k]], var = var, weight = weight,
                         by = byvals,
                         class = cnmv,
                         stats = outp$stats,
                         shape = outp$shape,
                         freq = freq,
                         type = type,
                         opts = opts)


    if (!is.null(res))
      res <- rbind(res, tmpres)
    else
      res <- tmpres
  }

  if (!is.null(class)) {
    clsnms <- "CLASS"
    if (length(class) > 1) {

      clsnms <- paste0("CLASS", seq(1, length(class)))
    }


    res <- restore_datatypes(res, data, class, clsnms)

    res <-  sort(res, by = clsnms)
    rownames(res) <- NULL
  }


  return(res)
}


#' @import fmtr
#' @import common
#' @import tibble
gen_output_means <- function(data,
                             by = NULL,
                             class = NULL,
                             var = NULL,
                             weight = NULL,
                             output = NULL,
                             opts = NULL) {

  res <- list()
  if (length(output) > 0) {

    nms <- names(output)
    for (i in seq_len(length(output))) {

      outp <- output[[i]]

      # Whether to include type variable
      tp <- 0
      if (has_option(opts, "notype"))
        tp <- NULL

      # Whether to include freq variable
      frq <- TRUE
      if (has_option(opts, "nonobs") |
          has_option(opts, "nofreq"))
        frq <- FALSE

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

      tmpres <- NULL

      for (j in seq_len(length(bdat))) {

        dat <- bdat[[j]]

        if (!is.null(tp))
          tp <- 0

        # Deal with by variable values
        bynm <- NULL
        if (!is.null(bynms)) {
          bynm <- strsplit(bynms[j], "|", fixed = TRUE)[[1]]
          names(bynm) <- byn
        }


        if (has_option(opts, "nway") == FALSE ||
            (has_option(opts, "nway") == TRUE && is.null(class))) {

          # Always add type 0
          tmpby <- get_output(dat, var = var, weight = weight,
                              by = bynm,
                              class = cls,
                              stats = outp$stats,
                              shape = outp$shape,
                              freq = frq,
                              type = tp,
                              opts = opts)

          tmpby <- restore_datatypes(tmpby, dat, class)

          if (is.null(tmpres))
            tmpres <- tmpby
          else
            tmpres <- rbind(tmpres, tmpby)

        }


        if (!is.null(class)) {

          if (!is.null(tp))
            tp <- 1


          tmpcls <- get_class_output(dat, var = var, weight = weight,
                                     class = class, outp = outp,
                                     freq = frq, type = tp, byvals = bynm, opts = opts,
                                     stats = outp$stats)

          if (is.null(tmpres))
            tmpres <- tmpcls
          else
            tmpres <- rbind(tmpres, tmpcls)

        }
      }


      # System Labels
      if (!is.null(tmpres))
        labels(tmpres) <- append(mlbls, bylbls)

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

      # Cast to tibble if incoming data was a tibble
      if ("tbl_df" %in% class(data)) {
        if (!is.null(tmpres))
          tmpres <- as_tibble(tmpres)
      }

      res[[nms[i]]]  <- tmpres

    }
  }

  if (length(res) == 1)
    res <- res[[1]]


  return(res)

}

#' @import utils
get_class_output <- function(data, var, class, outp, freq = TRUE,
                             type = NULL, byvals = NULL, weight = NULL,
                             opts = NULL, stats = NULL) {


  res <- NULL

  if (is.null(class)) {

    res <- get_output(data, var = var, weight = weight,
                      by = byvals,
                      class = NULL,
                      stats = outp$stats,
                      shape = outp$shape,
                      freq = freq,
                      type = type,
                      opts = opts)


  } else {

    # Create name lookup
    clsnms <- get_class_names(class)

    tp <- 1

    for (i in seq_len(length(class))) {

      cbm <- combn(class, i, simplify = FALSE)

      #for (j in seq_len(length(cbm))) {
      for (j in seq(length(cbm), 1, -1)) {

        tmpcls <- cbm[[j]]

        if (has_option(opts, "completetypes"))
          clslist <- split(data, data[ , tmpcls], sep = "|", drop = FALSE)
        else
          clslist <- split(data, data[ , tmpcls], sep = "|", drop = TRUE)

        cnms <- names(clslist)
        if (length(clslist) == 0) {

          clslist <- list(data)
          cnms <- tmpcls

        }


        for (k in seq_len(length(clslist))) {

          cnmv <- NULL
          if (!is.null(cnms)) {
            cnmv <- strsplit(cnms[k], "|", fixed = TRUE)[[1]]
            names(cnmv) <- tmpcls

          }


          tmpres <- get_output(clslist[[k]], var = var, weight = weight,
                               by = byvals,
                               class = cnmv,
                               stats = outp$stats,
                               shape = outp$shape,
                               freq = freq,
                               type = tp,
                               opts = opts,
                               keep_names = TRUE)

          # Translate names so they merge properly
          tmpnms <- names(tmpres)
          names(tmpres) <- ifelse(tmpnms %in% class, clsnms[tmpnms], tmpnms)


          if (!is.null(res))
            res <- merge(res, tmpres, all = TRUE, no.dups = FALSE, sort = FALSE)
          else
            res <- tmpres
        }

        tp <- tp + 1

      }




    }

    clsnms <- get_class_names(class)

    res <- restore_datatypes(res, data, class, clsnms)

    if ("STAT" %in% names(res)) {
      # Sort stats by supplied order
      # res[["STAT"]] <- factor(res[["STAT"]], levels = toupper(stats))  # stats not complete list
      res[["STAT"]] <- factor(res[["STAT"]], levels = unique(res[["STAT"]]))

      # Deal with NA values in sort
      ccnms <- as.character(clsnms)
      # for (cnm in ccnms) {
      #
      #   res[[cnm]] <- ifelse(is.na(res[[cnm]]), "...", res[[cnm]])
      # }

      # Sort
      res <- sort(res, by = c("TYPE", ccnms, "STAT"), na.last = FALSE)

      # Restore stat column
      res[["STAT"]] <- as.character(res[["STAT"]])

      # Restore CLASS columns
      # for (cnm in ccnms) {
      #
      #   res[[cnm]] <- ifelse(res[[cnm]] == "...", NA, res[[cnm]])
      # }

    } else {


      res <-  sort(res, by = c("TYPE", as.character(clsnms)))

    }

    if (has_option(opts, "nway")) {

      res <- res[res$TYPE == max(res$TYPE), ]
    }

    rnms <- names(res)
    res <- res[ , c(clsnms, rnms[!rnms %in% clsnms])]

    if (is.null(type)) {

      # Kill TYPE column if requested
      if ("TYPE" %in% names(res)) {

        res[["TYPE"]] <- NULL
      }
    }


  }



  return(res)
}



# Proc Freq ---------------------------------------------------------------



#' @title Generates Frequency Statistics
#' @encoding UTF-8
#' @description The \code{proc_freq} function generates frequency statistics.
#' It is both an interactive function that can be used for data exploration,
#' and can produce dataset output for further analysis.
#' The function can perform one and two-way frequencies.  Two-way
#' frequencies are produced as a cross-tabulation by default.  There
#' are many options to control the generated tables.  The function will return
#' requested tables in a named list.
#' @details
#' The \code{proc_freq} function generates frequency statistics
#' for one-way and two-way tables.  Data is passed in on the \code{data}
#' parameter.  The desired frequencies are specified on the \code{tables}
#' parameter.
#'
#' @section Report Output:
#' By default, \code{proc_freq} results will
#' be immediately sent to the viewer as an HTML report.  This functionality
#' makes it easy to get a quick analysis of your data with very little
#' effort. To turn off the interactive report, pass the "noprint" keyword
#' to the \code{options} parameter or set \code{options("procs.print" = FALSE)}.
#'
#' The \code{titles} parameter allows you to set one or more titles for your
#' report.  Pass these titles as a vector of strings.
#'
#' If the frequency variables have a label assigned, that label
#' will be used in the report output. This feature gives you some control
#' over the column headers in the final report.
#'
#' The exact datasets used for the interactive output can be returned as a list.
#' To return these datasets as a list, pass
#' the "report" keyword on the \code{output} parameter. This list may in
#' turn be passed to \code{\link{proc_print}} to write the report to a file.
#'
#' @section Data Frame Output:
#' The \code{proc_freq} function returns output datasets.
#' If you are requesting only one table, a single
#' data frame will be returned.  If you request multiple tables, a list of data
#' frames will be returned.
#'
#' By default, the list items are named according to the
#' strings specified on the \code{tables} parameter. You may control
#' the names of the returned results by using a named vector on the
#' \code{tables} parameter.
#'
#' The standard output datasets are optimized for data manipulation.
#' Column names have been standardized, and additional variables may
#' be present to help with data manipulation. For instance, the by variable will
#' always be named "BY", and the frequency category will always be named "CAT".
#' In addition, data values in the
#' output datasets are not rounded or formatted
#' to give you the most accurate statistical results.
#'
#' @section Frequency Weight:
#' Normally the \code{proc_freq} function counts each row in the
#' input data equally. In some cases, however, each row in the data
#' can represent multiple observations, and rows should not be treated
#' equally.  In these cases, use the \code{weight} parameter.  The parameter
#' accepts a variable/column name to use as the weighted value.  If the
#' \code{weight} parameter is used, the function will sum the weighted values
#' instead of counting rows.
#'
#' @section By Groups:
#' You may request that frequencies be separated into by groups using the
#' \code{by} parameter.  The parameter accepts one or more variable names
#' from the input dataset. When this parameter is assigned, the data
#' will be subset by the "by" variable(s) before frequency counts are
#' calculated.  On the interactive report, the by groups will appear in
#' separate tables.  On the output dataset, the by groups will be identified
#' by additional columns.
#'
#' @section Options:
#' The \code{options} parameter accepts a vector of options.  Normally, these
#' options must be quoted.  But you may pass them unquoted using the \code{v()}
#' function.  For example, you can request the number of category levels
#' and the Chi-Square statistic like this: \code{options = v(nlevels, chisq)}.
#'
#' Below are all the available options and a description of each:
#' \itemize{
#' \item{\strong{crosstab}: Two-way output tables are a list style by default.
#' If you want a crosstab style, pass the "crosstab" option.
#' }
#' \item{\strong{list}: Two-way interactive tables are a crosstab style
#' by default.  If you want a list style two-way table, pass the "list" option.
#' }
#' \item{\strong{missing}: Normally, missing values are not counted and not
#' shown on frequency tables.  The "missing" option allows you to treat
#' missing (NA) values as normal values, so that they are counted and
#' shown on the frequency table.  Missing levels will appear on the
#' table as a single dot (".").
#' }
#' \item{\strong{nlevels}: The "nlevels" option will display the number of unique
#' values for each variable in the frequency table. These levels are generated
#' as a separate table that appears on the report, and will also be output from
#' the function as a separate dataset.
#' }
#' \item{\strong{nocol}: Two-way cross tabulation tables include column percents
#' by default.  To turn them off, pass the "nocol" option.
#' }
#' \item{\strong{nocum}: Whether to include the cumulative frequency and percent
#' columns on one-way, interactive tables. These columns are included by default.
#' To turn them off, pass the "nocum" option.
#' }
#' \item{\strong{nofreq}: The "nofreq" option will remove the frequency column
#' from one-way and two-way tables.
#' }
#' \item{\strong{nopercent}: The "nopercent" option will remove the percent column
#' from one-way and two-way tables.
#' }
#' \item{\strong{noprint}: Whether to print the interactive report to the
#' viewer.  By default, the report is printed to the viewer. The "noprint"
#' option will inhibit printing.
#' }
#' \item{\strong{nonobs}: Whether to include the number of observations "N"
#' column on the output and interactive tables.  By default, the N column
#' will be included.  The "nonobs" option turns it off.
#' }
#' \item{\strong{norow}: Whether to include the row percentages on two-way
#' crosstab tables. The "norow" option will turn them off.
#' }
#' \item{\strong{nosparse/sparse}: Whether to include categories for which there are no
#' frequency counts.  Zero-count categories will be included by default, which
#' is the "sparse" option.  If the
#' "nosparse" option is present, zero-count categories will be removed.
#' }
#' \item{\strong{notable}: Whether to include the frequency table in the output
#' dataset list. Normally, the frequency table is included.  You may want to
#' exclude the frequency table in some cases, for instance, if you only
#' want the Chi-Square statistic.
#' }
#' \item{\strong{outcum}: Whether to include the cumulative frequency and percent
#' on output frequency tables.  By default, these columns are not included.
#' The "outcum" option will include them.
#' }
#' }
#' @section Statistics Options:
#' In addition to the above options, the \code{options} parameter accepts
#' some statistics options.  The following keywords will generate
#' an additional tables of specialized statistics. These statistics
#' options are only available on two-way tables:
#' \itemize{
#' \item{\strong{chisq}: Requests that the Chi-square statistics be produced.
#' }
#' \item{\strong{fisher}: Requests that the Fisher's exact statistics be produced.
#' }
#' }
#' @section Using Factors:
#' There are some occasions when you may want to define the \code{tables} variable
#' or \code{by} variables as a factor. One occasion is for sorting/ordering,
#' and the other is for obtaining zero-counts on sparse data.
#'
#' To order the frequency categories in the frequency output, define the
#' \code{tables} variable as a factor in the desired order. The function will
#' then retain that order for the frequency categories in the output dataset
#' and report.
#'
#' You may also wish to
#' define the tables variable as a factor if you are dealing with sparse data
#' and some of the frequency categories are not present in the data. To ensure
#' these categories are displayed with zero-counts, define the \code{tables} variable
#' or \code{by} variable
#' as a factor and use the "sparse" option.  Note
#' that the "sparse" option is actually the default.
#'
#' If you do not want to
#' show the zero-count categories on a variable that is defined as a factor,
#' pass the "nosparse" keyword on the \code{options} parameter.
#'
#' @section Data Shaping:
#' By default, the \code{proc_freq} function returns an output dataset of
#' frequency results.  If running interactively, the function also prints
#' the frequency results to the viewer.  As described above, the output
#' dataset can be somewhat different than the dataset sent to the viewer.
#' The \code{output} parameter allows you to choose which datasets to return.
#' There are three choices:
#' "out", "report", and "none".  The "out" keyword returns the default output
#' dataset.  The "report" keyword returns the dataset(s) sent to the viewer. You
#' may also pass "none" if you don't want any datasets returned from the function.
#'
#' In addition, the output dataset produced by the "out" keyword can be shaped
#' in different ways. These shaping options allow you to decide whether the
#' data should be returned long and skinny, or short and wide. The shaping
#' options can reduce the amount of data manipulation necessary to get the
#' frequencies into the desired form. The
#' shaping options are as follows:
#' \itemize{
#' \item{\strong{long}: Transposes the output datasets
#' so that statistics are in rows and frequency categories are in columns.
#' }
#' \item{\strong{stacked}: Requests that output datasets
#' be returned in "stacked" form, such that both statistics and frequency
#' categories are in rows.
#' }
#' \item{\strong{wide}: Requests that output datasets
#' be returned in "wide" form, such that statistics are across the top in
#' columns, and frequency categories are in rows. This shaping option
#' is the default.
#' }
#' }
#'
#' @param data The input data frame to perform frequency calculations on.
#' Input data as the first parameter makes this function pipe-friendly.
#' @param tables The variable or variables to perform frequency counts on.
#' The table specifications are passed as a vector of strings. For one-way
#' frequencies, simply pass the variable name.
#' For two-way tables, pass the desired combination of variables separated by a
#' star (*) operator.  The parameter does not accept SAS® style grouping syntax.
#' All cross combinations should be listed explicitly. If the
#' table request is named, the name will be used as the list item name on the
#' return list of tables. See "Example 3" for an illustration on how to name an
#' output table.
#' @param output Whether or not to return datasets from the function. Valid
#' values are "out", "none", and "report".  Default is "out". This parameter
#' also accepts the data shaping options "long", "stacked", and "wide". See
#' the \strong{Data Shaping} section for a description of these options. Multiple
#' output keywords may be passed on a character vector. For example,
#' to produce both a report dataset and a "long" output dataset,
#' use the parameter \code{output = c("report", "out", "long")}.
#' @param by An optional by group. Parameter accepts a vector of one or more
#' variable names. When this parameter is set, data
#' will be subset for each by group, and tables will be generated for
#' each subset.
#' @param weight An optional weight parameter.  This parameter is passed
#' as a variable name to use for the weight.  If a weight variable is
#' indicated, the weighted value will be summed to calculate the frequency
#' counts.
# @param order How to order the output.
# @param plots Any plots to produce.
#' @param options The options desired for the function.
#' Options are passed to the parameter as a vector of quoted strings. You may
#' also use the \code{v()} function to pass unquoted strings.
#' The following options are available:
#' "chisq", "crosstab", "fisher", "list", "missing",
#' "nlevels", "nocol",
#' "nocum", "nofreq", "nopercent", "noprint",
#' "nonobs", "norow", "nosparse", "notable", "outcum". See
#' the \strong{Options} section for a description of these options.
#' @param titles A vector of titles to assign to the interactive report.
#' @return The function will return all requested datasets by default.  This is
#' equivalent to the \code{output = "out"} option.  To return the datasets
#' as created for the interactive report, pass the "report" output option.  If
#' no output datasets are desired, pass the "none" output option. If a
#' single dataset is requested, the function
#' will return a single dataset.  If multiple datasets are requested, the function
#' will return a list of datasets.  The type of data frame returned will
#' correspond to the type of data frame passed in on the \code{data} parameter.
#' If the input data is a tibble, the output data will be a
#' tibble.  If the input data is a Base R data frame, the output data will be
#' a Base R data frame.
#' @seealso For summary statistics, see \code{\link{proc_means}}.  To pivot
#' or transpose the data coming from \code{proc_freq},
#' see \code{\link{proc_transpose}}.
#' @examples
#' library(procs)
#'
#' # Turn off printing for CRAN checks
#' options("procs.print" = FALSE)
#'
#' # Create sample data
#' df <- as.data.frame(HairEyeColor, stringsAsFactors = FALSE)
#'
#' # Assign labels
#' labels(df) <- list(Hair = "Hair Color",
#'                    Eye = "Eye Color",
#'                    Sex = "Sex at Birth")
#'
#' # Example #1: One way frequencies on Hair and Eye color with weight option.
#' res <- proc_freq(df,
#'                  tables = v(Hair, Eye),
#'                  options = outcum,
#'                  weight = Freq)
#'
#' # View result data
#' res
#' # $Hair
#' #    VAR   CAT   N CNT      PCT CUMSUM    CUMPCT
#' # 1 Hair Black 592 108 18.24324    108  18.24324
#' # 2 Hair Blond 592 127 21.45270    235  39.69595
#' # 3 Hair Brown 592 286 48.31081    521  88.00676
#' # 4 Hair   Red 592  71 11.99324    592 100.00000
#' #
#' # $Eye
#' #   VAR   CAT   N CNT      PCT CUMSUM    CUMPCT
#' # 1 Eye  Blue 592 215 36.31757    215  36.31757
#' # 2 Eye Brown 592 220 37.16216    435  73.47973
#' # 3 Eye Green 592  64 10.81081    499  84.29054
#' # 4 Eye Hazel 592  93 15.70946    592 100.00000
#'
#' # Example #2: 2 x 2 Crosstabulation table with Chi-Square statistic
#' res <- proc_freq(df, tables = Hair * Eye,
#'                      weight = Freq,
#'                      options = v(crosstab, chisq))
#'
#' # View result data
#' res
#' #$`Hair * Eye`
#' #   Category Statistic       Blue      Brown      Green     Hazel     Total
#' #1     Black Frequency  20.000000  68.000000  5.0000000 15.000000 108.00000
#' #2     Black   Percent   3.378378  11.486486  0.8445946  2.533784  18.24324
#' #3     Black   Row Pct  18.518519  62.962963  4.6296296 13.888889        NA
#' #4     Black   Col Pct   9.302326  30.909091  7.8125000 16.129032        NA
#' #5     Blond Frequency  94.000000   7.000000 16.0000000 10.000000 127.00000
#' #6     Blond   Percent  15.878378   1.182432  2.7027027  1.689189  21.45270
#' #7     Blond   Row Pct  74.015748   5.511811 12.5984252  7.874016        NA
#' #8     Blond   Col Pct  43.720930   3.181818 25.0000000 10.752688        NA
#' #9     Brown Frequency  84.000000 119.000000 29.0000000 54.000000 286.00000
#' #10    Brown   Percent  14.189189  20.101351  4.8986486  9.121622  48.31081
#' #11    Brown   Row Pct  29.370629  41.608392 10.1398601 18.881119        NA
#' #12    Brown   Col Pct  39.069767  54.090909 45.3125000 58.064516        NA
#' #13      Red Frequency  17.000000  26.000000 14.0000000 14.000000  71.00000
#' #14      Red   Percent   2.871622   4.391892  2.3648649  2.364865  11.99324
#' #15      Red   Row Pct  23.943662  36.619718 19.7183099 19.718310        NA
#' #16      Red   Col Pct   7.906977  11.818182 21.8750000 15.053763        NA
#' #17    Total Frequency 215.000000 220.000000 64.0000000 93.000000 592.00000
#' #18    Total   Percent  36.317568  37.162162 10.8108108 15.709459 100.00000
#'
#' # $`chisq:Hair * Eye`
#' #                         STAT DF      VAL         PROB
#' # 1                 Chi-Square  9 138.2898 2.325287e-25
#' # 2 Continuity Adj. Chi-Square  9 138.2898 2.325287e-25
#'
#' #' # Example #3: By variable with named table request
#' res <- proc_freq(df, tables = v(Hair, Eye, Cross = Hair * Eye),
#'                  by = Sex,
#'                  weight = Freq)
#'
#' # View result data
#' res
#' # $Hair
#' #       BY  VAR   CAT   N CNT      PCT
#' # 1 Female Hair Black 313  52 16.61342
#' # 2 Female Hair Blond 313  81 25.87859
#' # 3 Female Hair Brown 313 143 45.68690
#' # 4 Female Hair   Red 313  37 11.82109
#' # 5   Male Hair Black 279  56 20.07168
#' # 6   Male Hair Blond 279  46 16.48746
#' # 7   Male Hair Brown 279 143 51.25448
#' # 8   Male Hair   Red 279  34 12.18638
#' #
#' # $Eye
#' #       BY VAR   CAT   N CNT       PCT
#' # 1 Female Eye  Blue 313 114 36.421725
#' # 2 Female Eye Brown 313 122 38.977636
#' # 3 Female Eye Green 313  31  9.904153
#' # 4 Female Eye Hazel 313  46 14.696486
#' # 5   Male Eye  Blue 279 101 36.200717
#' # 6   Male Eye Brown 279  98 35.125448
#' # 7   Male Eye Green 279  33 11.827957
#' # 8   Male Eye Hazel 279  47 16.845878
#' #
#' # $Cross
#' #        BY VAR1 VAR2  CAT1  CAT2   N CNT        PCT
#' # 1  Female Hair  Eye Black  Blue 313   9  2.8753994
#' # 2  Female Hair  Eye Black Brown 313  36 11.5015974
#' # 3  Female Hair  Eye Black Green 313   2  0.6389776
#' # 4  Female Hair  Eye Black Hazel 313   5  1.5974441
#' # 5  Female Hair  Eye Blond  Blue 313  64 20.4472843
#' # 6  Female Hair  Eye Blond Brown 313   4  1.2779553
#' # 7  Female Hair  Eye Blond Green 313   8  2.5559105
#' # 8  Female Hair  Eye Blond Hazel 313   5  1.5974441
#' # 9  Female Hair  Eye Brown  Blue 313  34 10.8626198
#' # 10 Female Hair  Eye Brown Brown 313  66 21.0862620
#' # 11 Female Hair  Eye Brown Green 313  14  4.4728435
#' # 12 Female Hair  Eye Brown Hazel 313  29  9.2651757
#' # 13 Female Hair  Eye   Red  Blue 313   7  2.2364217
#' # 14 Female Hair  Eye   Red Brown 313  16  5.1118211
#' # 15 Female Hair  Eye   Red Green 313   7  2.2364217
#' # 16 Female Hair  Eye   Red Hazel 313   7  2.2364217
#' # 17   Male Hair  Eye Black  Blue 279  11  3.9426523
#' # 18   Male Hair  Eye Black Brown 279  32 11.4695341
#' # 19   Male Hair  Eye Black Green 279   3  1.0752688
#' # 20   Male Hair  Eye Black Hazel 279  10  3.5842294
#' # 21   Male Hair  Eye Blond  Blue 279  30 10.7526882
#' # 22   Male Hair  Eye Blond Brown 279   3  1.0752688
#' # 23   Male Hair  Eye Blond Green 279   8  2.8673835
#' # 24   Male Hair  Eye Blond Hazel 279   5  1.7921147
#' # 25   Male Hair  Eye Brown  Blue 279  50 17.9211470
#' # 26   Male Hair  Eye Brown Brown 279  53 18.9964158
#' # 27   Male Hair  Eye Brown Green 279  15  5.3763441
#' # 28   Male Hair  Eye Brown Hazel 279  25  8.9605735
#' # 29   Male Hair  Eye   Red  Blue 279  10  3.5842294
#' # 30   Male Hair  Eye   Red Brown 279  10  3.5842294
#' # 31   Male Hair  Eye   Red Green 279   7  2.5089606
#' # 32   Male Hair  Eye   Red Hazel 279   7  2.5089606
#' @import fmtr
#' @import tibble
#' @import common
#' @export
proc_freq <- function(data,
                      tables = NULL,
                      output = NULL,
                      by = NULL,
                      weight = NULL,
                      options = NULL,
                      titles = NULL
                    #  order = NULL,
                    #  plots = NULL
                      ) {


  # Deal with single value unquoted parameter values
  oby <- deparse(substitute(by, env = environment()))
  by <- tryCatch({if (typeof(by) %in% c("character", "NULL")) by else oby},
                 error = function(cond) {oby})

  otables <- deparse(substitute(tables, env = environment()))
  tables <- tryCatch({if (typeof(tables) %in% c("character", "NULL")) tables else otables},
                  error = function(cond) {otables})

  owgt <- deparse(substitute(weight, env = environment()))
  weight <- tryCatch({if (typeof(weight) %in% c("character", "NULL")) weight else owgt},
                 error = function(cond) {owgt})

  oopt <- deparse(substitute(options, env = environment()))
  options <- tryCatch({if (typeof(options) %in% c("character", "NULL")) options else oopt},
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
  if (!is.null(by)) {
    if (!all(by %in% nms)) {

      stop(paste("Invalid by name: ", by[!by %in% nms], "\n"))
    }
  }

  if (!is.null(weight)) {
    if (!all(weight %in% nms)) {

      stop(paste("Invalid weight name: ", weight[!weight %in% nms], "\n"))
    }
  }

  if (!is.null(tables)) {
    locs <- grepl("*", tables, fixed = TRUE)
    slocs <- unlist(strsplit(tables[locs], "*", fixed = TRUE))

    vars <- c(tables[!locs], trimws(slocs))

    vars <- unique(vars)

    if (!all(vars %in% nms)) {

      stop(paste("Invalid tables name: ", vars[!vars %in% nms], "\n"))
    }
  }

  if (!is.null(output)) {
    outs <- c("out", "report", "none", "wide", "long", "stacked")
    if (!all(tolower(output) %in% outs)) {

      stop(paste("Invalid output keyword: ", output[!tolower(output) %in% outs], "\n"))
    }

  }

  if (!is.null(options)) {
    kopts <- c("noprint",
               "list", "nocol", "nocum", "nofreq", "nopercent",
               "norow", "nosparse", "outcum",
               "sparse", "crosstab",
               "notable", "nonobs", "missing", "nlevels",
               "chisq", "fisher", # Statistics options
               "alpha" # Setting options
               )

    # Future options
    # "expected", "outexpect", "missprint", "cl", "maxlevels"

    if (!all(tolower(options) %in% kopts)) {

      stop(paste("Invalid options keyword: ", options[!tolower(options) %in% kopts], "\n"))
    }

  }

  # Set default statistics for output parameters
  if (has_output(output))
    outreq <- get_output_specs(tables, list(), options, output)
  else
    outreq <- NULL


  rptflg <- FALSE
  rptnm <- ""
  if (has_report(output)) {
    rptflg <- TRUE
  }

  if (has_view(options))
    view <- TRUE
  else
    view <- FALSE

  rptres <- NULL
  res <- NULL

  if (view | rptflg) {
    rptres <- gen_report_freq(data = data,
                              by = by,
                              tables = tables,
                              options = options,
                              weight = weight,
                              view = view,
                              titles = titles)
  }

  if (length(outreq) > 0) {

    res <- gen_output_freq(data = data,
                           by = by,
                           tables = tables,
                           options = options,
                           weight = weight,
                           output = outreq)

  }

  # Add report list if requested
  if (rptflg & !is.null(rptres)) {

    if (is.null(res))
      res <- rptres
    else {
      res <- list(out = res, report = rptres)

    }
  }

  log_freq(data = data, by = by, tables = tables, options = options, output = output,
           weight = weight, view = view, titles = titles, outcnt = length(res))


  # If only one dataset returned, remove list
  if (length(res) == 1) {
    res <- res[[1]]
  }

  if (log_output()) {
    log_logr(res)
    return(res)
  }

  return(res)

}

log_freq <- function(data,
                     by = NULL,
                     tables = NULL,
                     output = NULL,
                     options = NULL,
                     weight = NULL,
                     view = TRUE,
                     titles = NULL,
                     outcnt = NULL) {

  ret <- c()

  indt <- "           "

  ret <- paste0("proc_freq: input data set ", nrow(data),
                " rows and ", ncol(data), " columns")

  if (!is.null(tables))
    ret[length(ret) + 1] <- paste0(indt, "tables: ",
                                   paste(tables, collapse = " "))


  if (!is.null(by))
    ret[length(ret) + 1] <- paste0(indt, "by: ", paste(by, collapse = " "))

  if (!is.null(output))
    ret[length(ret) + 1] <- paste0(indt, "output: ", paste(output, collapse = " "))

  if (!is.null(weight))
    ret[length(ret) + 1] <- paste0(indt, "weight: ", paste(weight, collapse = " "))

  if (!is.null(view))
    ret[length(ret) + 1]<- paste0(indt, "view: ", paste(view, collapse = ""))

  if (!is.null(titles))
    ret[length(ret) + 1] <- paste0(indt, "titles: ", paste(titles, collapse = "\n"))


  if (!is.null(outcnt))
    ret[length(ret) + 1] <- paste0(indt, "output: ", outcnt, " datasets")


  log_logr(ret)

}

# Sub Procedures ----------------------------------------------------------



#' @import fmtr
#' @import stats
#' @import reporter
#' @import common
#' @noRd
freq_oneway <- function(data, tb, weight, options, out = FALSE, stats = NULL) {

  if (is.null(stats))
    stats <- c("n", "cnt", "pct", "cumsum", "cumpct")

  # Get target variable vector
  var <- data[[tb]]

  if (has_option(options, "missing")) {

    if (is.factor(var)) {

      nmiss <- sum(is.na(var))

      if (nmiss > 0) {

        var <- as.character(var)
        var <- ifelse(is.na(var), ".", var)
      }
    } else {
      var <- ifelse(is.na(var), ".", var)
    }

  }

  # Get frequency counts
  if (is.null(weight)) {

    categories <- names(sort(table(var)))
    frequencies <- as.vector(sort(table(var)))

  } else {


    if (nrow(data) == 0) {
      cnts <- 0
      categories <- ""
      frequencies <- 0


    } else {
      cnts <- aggregate(data[[weight]], list(var), FUN = sum)

      if (nrow(cnts) == 0) {
        categories <- ""
        frequencies <- 0

      } else {
        categories <- cnts$Group.1
        frequencies <- cnts$x
      }
    }


  }

  # Perform calculations
  n <- sum(frequencies, na.rm = TRUE)
  percentages <- frequencies / n * 100
  cum_frequencies <- cumsum(frequencies)
  cum_percentages <- cumsum(percentages)


  # Create result data frame
  result <- data.frame("CAT" = categories,
                       "N" = n,
                       "CNT" = frequencies,
                       "PCT" = percentages,
                       "CUMSUM" = cum_frequencies,
                       "CUMPCT" = cum_percentages,
                       stringsAsFactors = FALSE)



  # Get any existing label for target variable
  lbl <- attr(data[[tb]], "label")

  if (is.null(lbl))
    lbl <- tb

  # Clear out any names
  names(tb) <- NULL

  # Apply default labels
  labels(result) <- c(CAT= tb,
                      CNT = "Frequency",
                      PCT = "Percent",
                      CUMSUM = "Cumulative Frequency",
                      CUMPCT = "Cumulative Percent")

  # Apply default formats
  formats(result) <- list(CUMPCT = "%.2f",
                          PCT = "%.2f")

 # browser()



  if (option_true(options, "nonobs", FALSE) |
      !"n" %in% stats) {

    result[["N"]] <- NULL

  }

  # Kill freq if requested
  if (option_true(options, "nofreq", FALSE) |
      !"cnt" %in% stats) {

    result[["CNT"]] <- NULL
  }

  # Kill pct if requested
  if (option_true(options, "nopercent", FALSE) |
      !"pct" %in% stats) {

    result[["PCT"]] <- NULL
  }

  # Kill cum freq if requested
  if ((out == FALSE & option_true(options, "nocum", FALSE)) |
      (out == TRUE & !option_true(options, "outcum", FALSE))) {

    result[["CUMSUM"]] <- NULL
  }

  # Kill cum pct if requested
  if ((out == FALSE & option_true(options, "nocum", FALSE)) |
      (out == TRUE & !option_true(options, "outcum", FALSE))) {

    result[["CUMPCT"]] <- NULL
  }

  if (!is.null(stats)) {
    # mp <- c(n = "N", cnt = "Frequency", pct = "Percent",
    #         cumsum = "Cum_Freq", cumpct = "Cum_Pct")

    fstats <- stats[toupper(stats) %in% names(result)]

    result <- result[ , c("CAT", toupper(fstats))]
  }


  # Add spanning headers
  spn <- span(1, ncol(result), label = paste("Table of", lbl), level = 1)
  attr(result, "spans") <- list(spn)

  # Add footnote for missing values
  if (!has_option(options, "missing")) {

    nas <- is.na(data[[tb]])
    if (!is.null(weight)) {
      nacnts <- sum(data[[weight]][nas])
    } else {
      nacnts <- sum(nas)
    }

    if (nacnts > 0) {
      ftns <- ftn("Frequency Missing = " %p% nacnts, align = "center",
                  blank_row = "none", borders = "all")
      attr(result, "footnotes") <- list(ftns)
    }
  }


  return(result)
}


#' @import fmtr
#' @import stats
#' @import common
#' @noRd
freq_twoway <- function(data, tb1, tb2, weight, options,
                        out = FALSE, stats = NULL) {

  if (is.null(stats))
    stats <- c("n", "cnt", "pct", "cumsum", "cumpct")

  # Assign 1 to count column
  if (is.null(weight) | weight != "__cnt")
    data[["__cnt"]] <- 1

  l1 <- NULL
  l2 <- NULL

  # Get target variables into vectors
  if (is.factor(data[[tb1]])) {
    l1 <- levels(data[[tb1]])
    v1 <- as.character(data[[tb1]])

  } else {
    v1 <- data[[tb1]]
  }

  if (is.factor(data[[tb2]])) {
    l2 <- levels(data[[tb2]])
    v2 <- as.character(data[[tb2]])
  } else {
    v2 <- data[[tb2]]
  }

  # Deal with missing
  if (has_option(options, "missing")) {

    if (is.factor(v1)) {

      nmiss <- sum(is.na(v1))

      if (nmiss > 0) {

        v1 <- as.character(v1)
        v1 <- ifelse(is.na(v1), ".", v1)
      }
    } else {
      v1 <- ifelse(is.na(v1), ".", v1)
    }

    if (is.factor(v2)) {

      nmiss <- sum(is.na(v2))

      if (nmiss > 0) {

        v2 <- as.character(v2)
        v2 <- ifelse(is.na(v2), ".", v2)
      }
    } else {
      v2 <- ifelse(is.na(v2), ".", v2)
    }

  }

  # Get unique values of variables
  t1 <- names(sort(table(v1)))
  t2 <- names(sort(table(v2)))

  # Get unique combinations of variable values for zero-fill
  ex <- expand.grid(tb1 = t1, tb2 = t2, stringsAsFactors = FALSE)

  # Assign zero fill value
  ex[["__cnt"]] <- 0

  # Use weight variable if requested
  if (is.null(weight)) {

    c1 <- data[["__cnt"]]

  } else {

    c1 <-data[[weight]]

  }

  # Append zero fills
  if (!option_true(options, "nosparse", FALSE)) {
    c1 <- append(c1, ex[["__cnt"]])
    v1 <- append(v1, ex[["tb1"]])
    v2 <- append(v2, ex[["tb2"]])
  }

  # Get frequencies
  cnts <- aggregate(c1, list(v1, v2), FUN = sum)
  categories1 <- cnts$Group.1
  categories2 <- cnts$Group.2
  frequencies <- cnts$x

  # Perform calculations
  n <- sum(frequencies, na.rm = TRUE)
  percentages <- frequencies / n * 100



  # Create result data frame
  result <- data.frame("CAT1" = categories1,
                       "CAT2" = categories2,
                       "N" = n,
                       "CNT" = frequencies,
                       "PCT" = percentages,
                       stringsAsFactors = FALSE)

  # Restore factors if necessary
  if (!is.null(l1)) {
    result$CAT1 <- factor(result$CAT1, levels = l1)
  }
  if (!is.null(l2)) {
    result$CAT2 <- factor(result$CAT2, levels = l2)
  }


  # Sort result data frame
  result <- sort(result, by = c("CAT1", "CAT2"))


  # Kill rownames
  rownames(result) <- NULL

  result$CUMSUM =  cumsum(result$CNT)
  result$CUMPCT = cumsum(result$PCT)

  # Get labels on target variables if they exist
  lbl1 <- attr(data[[tb1]], "label")
  lbl2 <- attr(data[[tb2]], "label")

  if (is.null(lbl1))
    lbl1 <- tb1
  if (is.null(lbl2))
    lbl2 <- tb2

  # Assign labels
  labels(result) <- c(CAT1 = lbl1,
                      CAT2 = lbl2,
                      CNT = "Frequency",
                      PCT = "Percent",
                      CUMSUM = "Cumulative Frequency",
                      CUMPCT = "Cumulative Percent")

  # Assign default formats
  formats(result) <- list(PCT = paste0("%.4f"),
                          CUMPCT = paste0("%.4f"))

  if (out == FALSE |
      option_true(options, "nonobs", FALSE) | !"n" %in% stats) {

    result[["N"]] <- NULL
  }

  # Kill freq if requested
  if (option_true(options, "nofreq", FALSE) |
      !"cnt" %in% stats) {

    result[["CNT"]] <- NULL
  }

  # Kill pct if requested
  if (option_true(options, "nopercent", FALSE) |
      !"pct" %in% stats) {

    result[["PCT"]] <- NULL
  }


  # Kill cum freq if requested
  if (out == FALSE & option_true(options, "nocum"))
    result[["CUMSUM"]] <- NULL
  else if (out & !option_true(options, "outcum", FALSE)) {

    result[["CUMSUM"]] <- NULL
  }

  # Kill cum pct if requested
  if (out == FALSE & option_true(options, "nocum"))
    result[["CUMPCT"]] <- NULL
  else if (out & !option_true(options, "outcum", FALSE)) {

    result[["CUMPCT"]] <- NULL
  }


  if (!is.null(stats)) {
    # mp <- c(n = "N", cnt = "Frequency", pct = "Percent",
    #         cumsum = "Cum_Freq", cumpct = "Cum_Pct")

    fstats <- stats[toupper(stats) %in% names(result)]

    result <- result[ , c("CAT1", "CAT2", toupper(fstats))]
  }

  # Add footnote for missing values
  if (!has_option(options, "missing")) {

    nas1 <- is.na(data[[tb1]])
    nas2 <- is.na(data[[tb2]])

    if (!is.null(weight)) {
      na1cnts <- sum(data[[weight]][nas1])
      na2cnts <- sum(data[[weight]][nas2])
    } else {
      na1cnts <- sum(nas1)
      na2cnts <- sum(nas2)
    }

    if ((na1cnts + na2cnts) > 0) {
      ftns <- ftn("Frequency Missing = " %p% (na1cnts + na2cnts),
                  align = "center",
                  blank_row = "none", borders = "all")
      attr(result, "footnotes") <- list(ftns)
    }
  }

  return(result)

}

#' @import fmtr
#' @import stats
#' @import common
#' @noRd
cross_tab <- function(freqdata, options, var1, var2, bylbl = NULL) {

  lbl1 <- attr(freqdata$CAT1, "label")
  lbl2 <- attr(freqdata$CAT2, "label")

  #browser()
  if (has_option(options, "missing")) {

    v1 <- freqdata$CAT1
    v2 <- freqdata$CAT2

    if (is.factor(v1)) {

      nmiss <- sum(is.na(v1))

      if (nmiss > 0) {

        v1 <- as.character(v1)
        v1 <- ifelse(is.na(v1), ".", v1)
      }
    } else {
      v1 <- ifelse(is.na(v1), ".", v1)
    }

    if (is.factor(v2)) {

      nmiss <- sum(is.na(v2))

      if (nmiss > 0) {

        v2 <- as.character(v2)
        v2 <- ifelse(is.na(v2), ".", v2)
      }
    } else {
      v2 <- ifelse(is.na(v2), ".", v2)
    }

    freqdata$CAT1 <- v1
    freqdata$CAT2 <- v2
  }

  nms <- names(freqdata)
  if ("CUMSUM" %in% nms) {
    freqdata$CUMSUM <- NULL
  }
  if ("CUMPCT" %in% nms) {
    freqdata$CUMPCT <- NULL
  }

  # Group by both dimensions
  cat1grp <- aggregate(freqdata$CNT, list(freqdata$CAT1), FUN=sum)
  cat2grp <- aggregate(freqdata$CNT, list(freqdata$CAT2), FUN=sum)

  # Create lookup from cat1 group (rows)
  lkp1 <- cat1grp$x
  names(lkp1) <- cat1grp$Group.1

  # Create lookup from cat2 group (columns)
  lkp2 <- cat2grp$x
  names(lkp2) <- cat2grp$Group.1

  # Assign data to new variable
  dt <- freqdata

  # Create freq columns for both dimensions
  dt$rowcnt <- lkp1[dt$CAT1]
  dt$colcnt <- lkp2[dt$CAT2]

  # Create percentages for both dimensions
  #dt$Percentage <- dt$Percentage
  dt$rowpct <- dt$CNT / dt$rowcnt * 100
  dt$colpct <- dt$CNT / dt$colcnt * 100

  # Transpose Frequency statistics
  dt1 <- reshape(dt, timevar = "CAT2", idvar = "CAT1",
                 v.names = "CNT", direction = "wide",
                 drop = c("PCT", "rowcnt", "colcnt", "rowpct", "colpct"))
  dt1$Total <- lkp1[dt1$CAT1]
  dt1$Order <- 1
  dt1$Statistic <- "Frequency"
  names(dt1) <- gsub("CNT.",  "", names(dt1), fixed = TRUE)

  # Transpose Percents
  dt2 <- NULL
  if (!option_true(options, "nopercent", FALSE)) {
    dt2 <- reshape(dt, timevar = "CAT2", idvar = "CAT1",
                   v.names = "PCT", direction = "wide",
                   drop = c("CNT", "rowcnt", "colcnt", "rowpct", "colpct"))
    dt2$Total <- lkp1[dt1$CAT1] / sum(lkp1, na.rm = TRUE) * 100
    dt2$Order <- 2
    dt2$Statistic <- "Percent"
    names(dt2) <- gsub("PCT.",  "", names(dt2), fixed = TRUE)
  }


  # Transpose Row Percents
  dt3 <- NULL
  if (!option_true(options, "norow", FALSE)) {
    dt3 <- reshape(dt, timevar = "CAT2", idvar = "CAT1",
                   v.names = "rowpct", direction = "wide",
                   drop = c("PCT", "rowcnt", "colcnt", "CNT", "colpct"))
    dt3$Total <- NA
    dt3$Order <- 3
    dt3$Statistic <- "Row Pct"
    names(dt3) <- gsub("rowpct.",  "", names(dt3), fixed = TRUE)
  }

  # Transpose Col Percents
  dt4 <- NULL
  if (!option_true(options, "nocol", FALSE)) {
    dt4 <- reshape(dt, timevar = "CAT2", idvar = "CAT1",
                   v.names = "colpct", direction = "wide",
                   drop = c("PCT", "rowcnt", "colcnt", "CNT", "rowpct"))
    dt4$Total <- NA
    dt4$Order <- 4
    dt4$Statistic <- "Col Pct"
    names(dt4) <- gsub("colpct.",  "", names(dt4), fixed = TRUE)
  }

  #browser()
  dt5 <- NULL
  dt6 <- NULL

  # Add row total if requested
  if (option_true(options, "totrow", TRUE)) {
    dt5 <- data.frame(CAT1 = "Total")
    for (nm in names(lkp2)) {
      dt5[[nm]] <- lkp2[[nm]]
    }
    dt5$Total = sum(lkp2, na.rm = TRUE)
    dt5$Order = 1
    dt5$Statistic = "Frequency"

    if (!option_true(options, "nopercent", FALSE)) {
      dt6 <- data.frame(CAT1 = "Total")
      for (nm in names(lkp2)) {
        dt6[[nm]] <- lkp2[[nm]] / sum(lkp2, na.rm = TRUE) * 100
      }
      dt6$Total = 100
      dt6$Order = 2
      dt6$Statistic = "Percent"
    }
  }


  # Combine everthing
  ret <- rbind(dt1, dt2, dt3, dt4,
               make.row.names = FALSE,
               stringsAsFactors = FALSE)

  # Get all value column names
  nnms <- names(ret)[!names(ret) %in% c("CAT1", "Order", "Statistic")]

  # Sort data frame by category and order
  ret <- ret[order(ret$CAT1, ret$Order), c("CAT1", "Statistic", nnms) ]


  # Make sure total rows are at the end, after the sort
  if (!is.null(dt5) & !is.null(dt6)) {
    ret <- rbind(ret,
                 dt5[,  c("CAT1", "Statistic", nnms)],
                 dt6[,  c("CAT1", "Statistic", nnms)],
                 make.row.names = FALSE, stringsAsFactors = FALSE)
  } else if (!is.null(dt5)) {

    ret <- rbind(ret,
                 dt5[,  c("CAT1", "Statistic", nnms)],
                 make.row.names = FALSE, stringsAsFactors = FALSE)
  }

  # Kill rownames
  rownames(ret) <- NULL

  # Rename to Category so output_report() will recognize as a stub
  names(ret)[1] <- "CAT"

  # Get format
  fmt <- get_option(options, "format", "%.2f")

  # Create formatting list
  lst <- list(Frequency = "%d", Percent = fmt,
              'Row Pct' = fmt, 'Col Pct' = fmt)
  fl <- as.flist(lst, type = "row", lookup = ret$Statistic)

  # Assign flist to data columns
  fmts <- list()
  for (nm in nnms) {
    fmts[[nm]] <- fl
  }
  formats(ret) <- fmts

  # Kill column total if requested
  if (!option_true(options, "totcol", TRUE)) {

    ret$Total <- NULL
  }

  # Assign label to Category
  attr(ret$CAT, "label") <- lbl1

  # Add spanning headers
  if (!is.null(bylbl)) {
    lbl <- paste0(bylbl, "Table of ", var1, " by ", var2)
  } else {
    lbl <- paste0("Table of ", var1, " by ", var2)
  }
  spn2 <- span(1, ncol(ret), label = lbl, level = 2)
  spn1 <- span(3, ncol(ret), label = lbl2, level = 1)
  attr(ret, "spans") <- list(spn1, spn2)

  # Add footnote for missing values
  if (!has_option(options, "missing")) {
    fa <- attr(freqdata, "footnotes")
    if (!is.null(fa)) {
      attr(ret, "footnotes") <- fa
    }
  }

  return(ret)
}

get_output_oneway <- function(data, tb, weight = NULL, options = NULL,
                              by = NULL, shape = "wide", stats = NULL) {

  # Get frequencies
  ret <- freq_oneway(data = data, tb = tb, weight = weight,
                     options = options, stats = stats, out = TRUE)

  # Bind variable name
  tmp <- list(VAR = tb)
  df <- as.data.frame(tmp, stringsAsFactors = FALSE)
  ret <- cbind(df, ret)

  # Bind by variables
  if (!is.null(by)) {
    tmp <- list()
    for(nm in names(by)) {
      tmp[[nm]] <- by[nm]
    }

    df <- as.data.frame(tmp, stringsAsFactors = FALSE)

    rownames(df) <- NULL

    ret <- cbind(df, ret)
  }

  nms <- names(ret)


  if (!is.null(shape)) {
    if (shape == "long") {

      ret <- proc_transpose(ret, copy = c(names(by), "VAR"),
                            id = "CAT", name = "STAT", log = FALSE)

    } else if (all(shape == "stacked")) {

      ret <- proc_transpose(ret, name = "STAT",
                            by = c(names(by), "VAR", "CAT"), log = FALSE)

      rnms <- names(ret)
      rnms[rnms %in% "COL1"] <- "VALUES"

      names(ret) <- rnms

    }

  }

  return(ret)
}


get_output_twoway <- function(data, tb1, tb2, weight, options, out = FALSE,
                              by = NULL, shape = "wide", stats = NULL) {

  ret <- freq_twoway(data = data, tb1 = tb1, tb2 = tb2, weight = weight,
                     options = options, out = out, stats = stats)

  # Bind variable names
  tmp <- list(VAR1 = tb1, VAR2 = tb2)
  df <- as.data.frame(tmp, stringsAsFactors = FALSE)
  ret <- cbind(df, ret)


  if (!is.null(by)) {
    tmp <- list()
    for(nm in names(by)) {
      tmp[[nm]] <- by[nm]
    }

    df <- as.data.frame(tmp, stringsAsFactors = FALSE)

    rownames(df) <- NULL

    ret <- cbind(df, ret)
  }

  nms <- names(ret)



  if (!is.null(shape)) {
    if (shape == "long") {

      ret <- proc_transpose(ret, id = c("CAT1", "CAT2"),
                            copy = c(names(by), "VAR1", "VAR2"),
                            name = "STAT", log = FALSE)
    } else if (all(shape == "stacked")) {

      ret <- proc_transpose(ret, name = "STAT",
                            by = c(names(by), "VAR1", "VAR2", "CAT1", "CAT2"),
                            log = FALSE)

      rnms <- names(ret)
      rnms[rnms %in% "COL1"] <- "VALUES"

      names(ret) <- rnms

    }
  }

  return(ret)

}

#' @import common
get_output_specs <- function(tbls, outs, opts, output) {


  ret <- list()
  sts <- c("n", "cnt", "pct", "cumsum", "cumpct")

  if (length(outs) >= 1) {
    for (nm in names(outs)) {
      if ("out_req" %in% class(outs[[nm]])) {
        if (outs[[nm]]$report %eq% TRUE) {
          ret[[nm]] <- outs[[nm]]

        } else if (is.null(outs[[nm]]$table)) {
          ot <- outs[[nm]]

          tnms <- names(tbls)
          if (is.null(tnms))
            tnms <- tbls

          for (i in seq_len(length(tbls))) {
            if (length(tbls) == 1) {
              tnm <- nm
            } else {
              tnm <- tnms[[i]]
              if (tnm == "")
                tnm <- tbls[[i]]
            }

            ot$table <- tbls[[i]]
            if (is.null(ot$shape))
              ot$shape <- "wide"

            ret[[tnm]] <- ot

          }



        } else {

          ot <- outs[[nm]]
          if (is.null(ot$shape))
            ot$shape = "wide"
          ret[[nm]] <- ot

        }

      } else {

       warning("proc_freq: Unknown parameter '" %p% nm %p% "'")
      }
    }
  } else {

    tnms <- names(tbls)
    if (is.null(tnms))
      tnms <- tbls

    for (i in seq_len(length(tbls))) {
      nm <- tnms[[i]]
      if (nm == "")
        nm <- tbls[[i]]

      if (option_true(output, "long", FALSE)) {
        ret[[nm]] <- out_spec(table = tbls[[i]], stats = sts, shape = "long")
      } else if (option_true(output, "stacked", FALSE)) {
        ret[[nm]] <- out_spec(table = tbls[[i]], stats = sts, shape = "stacked")
      } else {
        ret[[nm]] <- out_spec(table = tbls[[i]], stats = sts, shape = "wide")
      }

    }

  }


  return(ret)
}

get_nlevels <- function(data, var1, var2 = NULL, byvars = NULL,
                        out = FALSE, missing = FALSE) {

  ret <- NULL
  vars <- c(var1)
  l1miss <- NULL
  l2miss <- NULL

  if (missing) {
    l1vals <- data[[var1]]
    l1miss <- sum(any(is.na(data[[var1]])))
  } else {

    l1vals <- data[[var1]][!is.na(data[[var1]])]

  }

  lvls <- c(length(unique(l1vals)))
  lbl <- ""


  if (!is.null(var2)) {
    if (missing) {
      l2vals <- data[[var2]]
      l2miss <- sum(any(is.na(data[[var2]])))
    } else
      l2vals <- data[[var2]][!is.na(data[[var2]])]

  }


  if (out) {

    if (!is.null(var2)) {

      if (missing) {

        l2cnt <-  length(unique(l2vals))

        ret <- data.frame(VAR1 = lvls, VAR1.MISS = l1miss,
                          VAR1.NONMISS = lvls - l1miss,
                          VAR2 =  l2cnt, VAR2.MISS = l2miss,
                          VAR2.NONMISS = l2cnt - l2miss,
                          stringsAsFactors = FALSE)

        labels(ret) <- list(VAR1 = var1, VAR2 = var2,
                            VAR1.MISS = var1 %p% "Missing Levels",
                            VAR1.NONMISS = var1 %p% "Nonmissing Levels",
                            VAR2.MISS = var2 %p% "Missing Levels",
                            VAR2.NONMISS = var2 %p% "Nonmissing Levels"
                            )


      } else {
        ret <- data.frame(VAR1 = lvls, VAR2 =  length(unique(l2vals)),
                          stringsAsFactors = FALSE)

        labels(ret) <- list(VAR1 = var1, VAR2 = var2)
      }

    } else {

      if (missing) {

        ret <- data.frame(VAR = lvls, MISS = l1miss, NONMISS = lvls - l1miss,
                          stringsAsFactors = FALSE)
        labels(ret) <- list(VAR = var1, MISS = "Missing Levels",
                            NONMISS = "Nonmissing Levels")

      } else {
        ret <- data.frame(VAR = lvls, stringsAsFactors = FALSE)
        labels(ret) <- list(VAR = var1)
      }
    }

    bv <- list()
    if (!is.null(byvars)) {

      if (length(byvars) == 1)
        nms <- "BY"
      else
        nms <- paste0("BY", seq_len(length(byvars)))

      for (i in seq_along(nms)) {
        bv[[nms[[i]]]] <- byvars[[names(byvars)[[i]]]]
      }

      bret <- as.data.frame(bv, stringsAsFactors = FALSE)
      labels(bret) <- names(byvars)
      ret <- cbind(bret, ret)

      lbl <- paste0(byvars, collapse = "")
    }
  } else {

    if (!is.null(var2)) {
      vars[2] <- var2
      lvls[2] <- length(unique(l2vals))
    }

    if (!missing) {


      ret <- data.frame(stub = vars, levels = lvls, stringsAsFactors = FALSE)

      labels(ret) <- list(stub = "Variable", levels = "Levels")
    } else {

      if (!is.null(var2)) {
       mcnt <- c(l1miss, l2miss)
       nmcnt <- c(lvls[1] - l1miss, lvls[2] - l2miss)

      } else {
        mcnt <- l1miss
        nmcnt <- lvls[1] - l1miss

      }

      ret <- data.frame(stub = vars, levels = lvls,
                        MISS = mcnt, NONMISS = nmcnt,
                        stringsAsFactors = FALSE)

      labels(ret) <- list(stub = "Variable", levels = "Levels",
                          MISS = "Missing Levels",
                          NONMISS = "Nonmissing Levels")

    }

    lbl <- gsub(",", "", byvars, fixed = TRUE)
  }

  # Add spanning headers
  if (!is.null(byvars)) {

    spn2 <- span(1, ncol(ret), label = lbl, level = 1)
   # spn1 <- span(1, ncol(ret), label = "Number of Variable Levels", level = 2)
    attr(ret, "spans") <- list(spn2)

  } else {
    # lbl <- paste0("Table of ", var1, " by ", var2)

    # spn2 <- span(1, ncol(ret), label = lbl, level = 2)
    # #spn1 <- span(1, ncol(ret), label = "Number of Variable Levels", level = 1)
    # attr(ret, "spans") <- list(spn1)
  }



  return(ret)
}


# Zero Fill -------------------------------------------------------------



# Appends missing combination to data frame and
# adds a variable __cnt by which you can count correctly.
get_nway_zero_fills <- function(data, outs, by = NULL, weight = NULL, options = NULL) {


  if (option_true(options, "nosparse", FALSE) ) {

    if (is.null(weight))
      data[["__cnt"]] <- 1
    else
      data[["__cnt"]] <- data[[weight]]

    ret <- data

  } else {

    # Get table variable vectors
    if ("list" %in% class(outs))
      ots <- get_output_tables(outs)
    else
      ots <- outs

    # Split into vectors
    tbls <- get_table_list(ots)

    # Set count value on existing records
    if (is.null(weight)) {

      data[["__cnt"]] <- 1
    } else {
      data[["__cnt"]] <- data[[weight]]
    }

    ret <- data

    for (i in seq_len(length(tbls))) {

      tb <- tbls[[i]]

      v1 <- list()
      # Get unique values of target var
      for (i in seq_len(length(tb))) {
        if (is.factor(data[[tb[i]]])) {
          v1[[tb[i]]] <- levels(data[[tb[i]]])
        } else {
          v1[[tb[i]]] <- names(sort(table(as.character(data[[tb[i]]]))))
        }
      }

      # Get unique by values
      if (!is.null(by)) {
        for (i in seq_len(length(by))) {
          if (is.factor(data[[by[i]]])) {
            v1[[by[i]]] <- levels(data[[by[i]]])
          } else {
            v1[[by[i]]] <- names(sort(table(as.character(data[[by[i]]]))))
          }
        }
      }

      # Expand combinations
      ex <- expand.grid(v1, stringsAsFactors = FALSE)

      if (nrow(ex) > 0) {

        # Zero fill combinations
        ex[["__cnt"]] <- 0

        # Merge combinations onto original data
        ret <- merge(ret, ex, sort = FALSE, all = TRUE)

      }
    }

  }

  return(ret)

}

get_table_list <- function(tbls) {

  ret <- NULL
  if (!is.null(tbls)) {


    ret <- strsplit(tbls, "*", fixed = TRUE)


    for (i in seq_len(length(ret))) {

      ret[[i]] <- trimws(ret[[i]])
    }
  }

  return(ret)

}

get_output_tables <- function(outs) {

 ret <- c()

 if (!is.null(outs)) {
   for (ot in outs) {

     ret[length(ret) +  1] <- ot[["table"]]

   }
 }

 return(ret)

}



# Drivers -----------------------------------------------------------------


gen_report_freq <- function(data,
                            by = NULL,
                            tables = NULL,
                            options = NULL,
                            weight = NULL,
                            view = TRUE,
                            titles = NULL ) {

  res <- list()
  # print("Orig print_location")
  # print(print_location)
  # browser()

  # Deal with sparse option
  dta <- get_nway_zero_fills(data, tables, by, weight, options)
  wgt <- "__cnt"

  bylbls <- c()
  if (!is.null(by)) {

    lst <- unclass(dta)[by]
    for (nm in names(lst))
      lst[[nm]] <- as.factor(lst[[nm]])
    dtlst <- split(dta, lst, sep = "|", drop = TRUE)

    snms <- strsplit(names(dtlst), "|", fixed = TRUE)

    for (k in seq_len(length(snms))) {
      for (l in seq_len(length(by))) {
        lv <- ""
        if (!is.null(bylbls[k])) {
          if (!is.na(bylbls[k])) {
            lv <- bylbls[k]
          }
        }

        bylbls[k] <- paste0(lv, by[l], "=", snms[[k]][l], ", ")
      }
    }

  } else {

    dtlst <- list(dta)
  }

  # Loop through by groups
  for (j in seq_len(length(dtlst))) {

    # Get table for this by group
    dt <- dtlst[[j]]

    # Loop through table requests
    for (i in seq_len(length(tables))) {

      nm <- names(tables)[i]
      tb <- tables[i]
      #browser()
      #out <- i == length(tables) & has_option(options, "out")

      crstab <- NULL
      chisq <- NULL
      fisher <- NULL
      nlevels <- NULL

      # Split cross variables
      splt <- trimws(strsplit(tb, "*", fixed = TRUE)[[1]])

      # Perform either one-way or two-way frequency count
      if (length(splt) == 1) {

        # Assign new label if there are by groups
        if (length(bylbls[j]) > 0) {
          if (!is.null(attr(data[[tb]], "label")))
            attr(dt[[tb]], "label") <- paste0(bylbls[j], attr(data[[tb]], "label"))
          else
            attr(dt[[tb]], "label") <- paste0(bylbls[j], tb)
        }

        # Perform one-way frequency
        result <- freq_oneway(dt, tb, wgt, options, out = FALSE)

        # Get nlevels if requested
        if (has_option(options, "nlevels")) {

          nlevels <- get_nlevels(dt, tb, byvars = bylbls[j],
                                 missing = has_option(options, "missing"))
        }

      } else if (length(splt) == 2) {

        bylbl <- NULL
        if (length(bylbls[j]) > 0) {
          bylbl <- bylbls[j]
        }

        # Perform two-way frequency
        result <- freq_twoway(dt, splt[1], splt[2], wgt, options,
                              out = FALSE)

        if (!has_option(options, "list")) {
          # Perform cross tab by default
          crstab <- cross_tab(result, options, splt[1], splt[2], bylbl)
        }

        if (get_option(options, "fisher", FALSE)) {

          if (!is.null(wgt))
            fisher <- get_fisher(dt[[splt[1]]], dt[[splt[[2]]]], dt[[wgt]],
                                 bylbl = bylbls[j])
          else
            fisher <- get_fisher(dt[[splt[1]]], dt[[splt[[2]]]],
                                 bylbl = bylbls[j])
        }

        if (get_option(options, "chisq", FALSE)) {

          if (!is.null(wgt))
            chisq <- get_chisq(dt[[splt[1]]], dt[[splt[[2]]]], dt[[wgt]],
                               bylbl = bylbls[j])
          else
            chisq <- get_chisq(dt[[splt[1]]], dt[[splt[[2]]]], bylbl = bylbls[j])
        }

        # Get nlevels if requested
        if (has_option(options, "nlevels")) {

          nlevels <- get_nlevels(dt, splt[1], splt[2], byvars = bylbls[j],
                                 missing = has_option(options, "missing"))
        }

      } else {

        stop("Procedure does not yet support n-way frequencies.")
      }

      # Cast to tibble if incoming data was a tibble
      if ("tbl_df" %in% class(data)) {
        if (!is.null(crstab))
          crstab <- as_tibble(crstab)

        if (!is.null(result))
          result <- as_tibble(result)

        if (!is.null(fisher))
          fisher <- as_tibble(fisher)

        if (!is.null(chisq))
          chisq <- as_tibble(chisq)

        if (!is.null(nlevels))
          nlevels <- as_tibble(nlevels)

      }

      if (!is.null(nlevels)) {
        res[[paste0("Nlevels:", get_name(nm, tb, bylbls[j]))]] <- nlevels
      }

      # If a cross tab was produced, add it to result
      if (!is.null(crstab)) {

        res[[get_name(nm, tb, bylbls[j])]] <- crstab

        # if ("out" %in% names(options) & i == length(tables)) {
        #
        #   res[[get_name(options[["out"]], "", bylbls[j])]] <- result
        # }

      } else { # Otherwise add list to result

        res[[get_name(nm, tb, bylbls[j])]] <- result

      }

      if (!is.null(chisq)) {

        res[[get_name("Chisq", tb, bylbls[j])]] <- chisq
      }

      if (!is.null(fisher)) {

        res[[get_name("Fisher", tb, bylbls[j])]] <- fisher
      }
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
        titles <- "The FREQ Function"
      }

      out <- output_report(res, dir_name = dirname(vrfl),
                           file_name = basename(vrfl), out_type = "HTML",
                           titles = titles, margins = .5, viewer = TRUE)

      show_viewer(out)
    }
  }

  return(res)

}




gen_output_freq <- function(data,
                            by = NULL,
                            tables = NULL,
                            options = NULL,
                            weight = NULL,
                            output = NULL) {

  # Deal with sparse option
  dta <- get_nway_zero_fills(data, output, by, weight, options)
  wgt <- "__cnt"

  byvals <- list()
  bynms <- NULL
  if (!is.null(by)) {
    if (length(by) == 1)
      bynms <- "BY"
    else
      bynms <- paste0("BY", seq(1, length(by)))

    lst <- unclass(dta)[by]
    for (nm in names(lst))
      lst[[nm]] <- as.factor(lst[[nm]])
    dtlst <- split(dta, lst, sep = "|")

    snms <- strsplit(names(dtlst), "|", fixed = TRUE)

    for (k in seq_len(length(snms))) {

      byvals[[k]] <- snms[[k]]
      names(byvals[[k]]) <- bynms
    }

  } else {

    dtlst <- list(dta)
  }


  res <- list()
  if (length(output) > 0) {

    for (nm in names(output)) {

      # print("Orig print_location")
      # print(print_location)
      # browser()

      outp <- output[[nm]]
      tb <- outp$table
      tmpres <- NULL

      chisq <- NULL
      fisher <- NULL
      nlevels <- NULL

      # Loop through by groups
      for (j in seq_len(length(dtlst))) {

        # Get table for this by group
        dt <- dtlst[[j]]

        if (nrow(dt) > 0 || has_option(options, "nosparse") == FALSE) {

        crstab <- NULL
        tmpchisq <- NULL
        tmpfisher <- NULL
        tmpnlevels <- NULL

        # Split cross variables
        splt <- trimws(strsplit(tb, "*", fixed = TRUE)[[1]])

        # Perform either one-way or two-way frequency count
        if (length(splt) == 1) {

          if (length(byvals) >= j) {
            result <- get_output_oneway(dt, tb, wgt, options,
                                      byvals[[j]], shape = outp$shape,
                                      stats = outp$stats)
          } else {
            result <- get_output_oneway(dt, tb, wgt, options,
                                        NULL, shape = outp$shape,
                                        stats = outp$stats)
          }

          if (has_option(options, "nlevels")) {

            if (length(byvals) >= j) {
              tmpnlevels <- get_nlevels(dt, tb, byvars = byvals[[j]], out = TRUE,
                                      missing = has_option(options, "missing"))
            } else {
              tmpnlevels <- get_nlevels(dt, tb, byvars = NULL, out = TRUE,
                                        missing = has_option(options, "missing"))

            }

          }

        } else if (length(splt) == 2) {




          if (has_option(options, "crosstab")) {

            result <- freq_twoway(dt, splt[1], splt[2], wgt, options,
                                  out = FALSE)
            result <- cross_tab(result, options, splt[1], splt[2], "")


          } else {

            # Perform two-way frequency
            if (length(byvals) >= j) {
              result <- get_output_twoway(dt, splt[1], splt[2], wgt, options,
                                          byvals[[j]], shape = outp$shape,
                                          out = TRUE, stats = outp$stats)
            } else {

              result <- get_output_twoway(dt, splt[1], splt[2], wgt, options,
                                          NULL, shape = outp$shape,
                                          out = TRUE, stats = outp$stats)
            }


          }

          if (has_option(options, "nlevels")) {

            if (length(byvals) >= j) {
              tmpnlevels <- get_nlevels(dt, splt[1], splt[2],
                                        byvars = byvals[[j]], out = TRUE,
                                        missing = has_option(options, "missing"))
            } else {
              tmpnlevels <- get_nlevels(dt, splt[1], splt[2],
                                        byvars = NULL, out = TRUE,
                                        missing = has_option(options, "missing"))

            }
          }


          if (!is.null(outp$stats)) {

            if (option_true(options, "fisher", FALSE)) {

              if (!is.null(wgt))
                tmpfisher <- get_fisher(dt[[splt[1]]], dt[[splt[[2]]]], dt[[wgt]],
                                     bylbl = byvals[j], output = TRUE)
              else
                tmpfisher <- get_fisher(dt[[splt[1]]], dt[[splt[[2]]]],
                                     bylbl = byvals[j], output = TRUE)

              if (is.null(fisher))
                fisher <- tmpfisher
              else
                fisher <- rbind(fisher, tmpfisher)



            }

            if (option_true(options, "chisq", FALSE)) {

              if (!is.null(wgt))
                tmpchisq <- get_chisq(dt[[splt[1]]], dt[[splt[[2]]]], dt[[wgt]],
                                   bylbl = byvals[j], output = TRUE)
              else
                tmpchisq <- get_chisq(dt[[splt[1]]], dt[[splt[[2]]]],
                                   bylbl = byvals[j], output = TRUE)

              if (is.null(chisq))
                chisq <- tmpchisq
              else
                chisq <- rbind(chisq, tmpchisq)


            }
          }

        } else {

          stop("Procedure does not yet support n-way frequencies.")
        }

        # Cast to tibble if incoming data was a tibble
        if ("tbl_df" %in% class(data)) {

          if (!is.null(result))
            result <- as_tibble(result)

          if (!is.null(chisq))
            chisq <- as_tibble(chisq)

          if (!is.null(fisher))
            fisher <- as_tibble(fisher)

          if (!is.null(nlevels))
            nlevels <- as_tibble(nlevels)

        }

        if (has_option(options, "nlevels")) {
          if (is.null(nlevels))
            nlevels <- tmpnlevels
          else
            nlevels <- rbind(nlevels, tmpnlevels)
        }

        if (!is.null(tmpres))
          tmpres <- rbind(tmpres, result)
        else
          tmpres <- result

        }

      }

      if (!is.null(by)) {

        tmpres <- restore_datatypes(tmpres, data, by, bynms)

      }


      # System Labels
      #labels(tmpres) <- append(mlbls, bylbls)

      # User labels
      if (!is.null(outp$label))
        labels(tmpres) <- outp$label

      # Formats
      if (!is.null(outp$format))
        formats(tmpres) <- outp$format



      # Assign to output
      if (!is.null(has_option(options, "nlevels")))
        res[[paste0("NLevels:", nm)]] <- nlevels

      if (!has_option(options, "notable"))
        res[[nm]] <- tmpres

      if (!is.null(chisq))
        res[[paste0("chisq:", nm)]] <- chisq

      if (!is.null(fisher))
        res[[paste0("fisher:", nm)]] <- fisher

    }
  }


  return(res)

}






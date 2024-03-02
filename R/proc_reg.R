
# TTest Procedure ---------------------------------------------------------



#' @title Calculates a Regression
#' @encoding UTF-8
#' @description The \code{proc_reg} function performs a regression
#' for one or more models.  The model(s) are passed on the \code{model} parameter,
#' and the input dataset is passed on the \code{data} parameter.  The \code{stats}
#' parameter allows you to request additional statistics, similar to the
#' model options in SAS.  The \code{by}
#' parameter allows you to subset the data into groups and run the model on each
#' group.  The \code{weight} parameter let's you assign a weight to each observation
#' in the dataset.  The \code{output} and \code{options} parameters provide
#' additional customization of the results.
#' @details
#' The \code{proc_reg} function is a general purpose regression function.  It
#' produces a dataset output by default, and, when working in RStudio,
#' also produces an interactive report.  The function has many convenient options
#' for what statistics are produced and how the analysis is performed. All
#' statistical output from \code{proc_reg} matches SAS.
#'
#' A model may be specified using R model syntax or SAS model syntax.  To use
#' SAS syntax, the model statement must be quoted.  To pass multiple models using
#' R syntax, pass them to the \code{model} parameter in a list.  To pass multiple
#' models using SAS syntax, pass them to the \code{model} parameter as a vector
#' of strings.
#'
#' @section Interactive Output:
#' By default, \code{proc_reg} results will
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
#' \code{proc_reg} typically returns a single dataset. The columns and rows
#' on this dataset can change depending on the keywords passed
#' to the \code{stats} and \code{options} parameters.
#'
#' The default output dataset is optimized for data manipulation.
#' The column names have been standardized, and additional variables may
#' be present to help with data manipulation. The data values in the
#' output dataset are intentionally not rounded or formatted
#' to give you the most accurate numeric results.
#'
#' You may also request
#' to return the datasets used in the interactive report. To request these
#' datasets, pass the "report" option to the \code{output} parameter.  Each report
#' dataset will be named according to the category of statistical
#' results.  There are four standard categories: "Observations",
#' "ANOVA", "Fitness", and "Coefficients". When the "spec" statistics option
#' is passed, the function will also return a "Specification" dataset containing
#' the White's test results.
#'
#' If you don't want any datasets returned, pass the "none" option on the
#' \code{output} parameter.
#'
#' @section Statistics Keywords:
#' The following statistics keywords can be passed on the \code{stats}
#' parameter. You may pass statistic keywords as a
#' quoted vector of strings, or an unquoted vector using the \code{v()} function.
#' An individual statistics keyword can be passed without quoting.
#' \itemize{
#'   \item{\strong{adjrsq}: Adds adjusted r-square value to the output dataset.}
#'   \item{\strong{clb}: Requests confidence limits be added to the interactive report.}
#'   \item{\strong{edf}: Includes the number of regressors, the error degress of freedom,
#'   and the model r-square to the output dataset.}
#'   \item{\strong{est}: Request an output dataset of parameter estimate and optional
#'   model fit summary statistics. This statistics option is the default.}
#'   \item{\strong{hcc}: The "hcc" statistics keyword requests that
#'   heteroscedasticity-consistent standard errors of the parameter estimates be
#'   sent to the interactive report.}
#'   \item{\strong{hccmethod=}: When the "hcc" option is present, the "hccmethod="
#'   option specifies the type of method to use. Valid values are 0 and 3.}
#'   \item{\strong{mse}: Computes the mean squared error for each model and adds to the
#'   output dataset.}
#'   \item{\strong{p}: Computes predicted and residual values and sends to
#'   a separate table on the interactive report.}
#'   \item{\strong{press}: Includes the predicted residual sum of squares
#'   (PRESS) statistic in the output dataset.}
#'   \item{\strong{rsquare}: Include the r-square statistic in the output dataset.
#'   The "rsquare" option has the same effect as the "edf" option.}
#'   \item{\strong{seb}: Outputs the standard errors of the parameter estimates
#'   to the output dataset.  These values will be identified as type "SEB".}
#'   \item{\strong{spec}: Adds the "White's test" table to the interactive output.
#'   This test determines whether the first and second moments of the model
#'   are correctly specified.}
#'   \item{\strong{sse}: Adds the error sum of squares to the output dataset.}
#'   \item{\strong{table}: The "table" keyword is used to send standard
#'   errors, t-statistics, p-values, and confidence limits to the output
#'   dataset.  These additional statistics are identified by types "STDERR",
#'   "T", and "PVALUE". The confidence limits are identified by "LxxB" and "UxxB",
#'   where "xx" is the alpha value in percentage terms. The "table" keyword
#'   on the \code{stats} parameter performs the same functions as the "tableout"
#'   option on the \code{options} parameter.}
#'   }
#'
#' @section Options:
#' The \code{proc_reg} function recognizes the following options.  Options may
#' be passed as a quoted vector of strings, or an unquoted vector using the
#' \code{v()} function.
#' \itemize{
#' \item{\strong{alpha = }: The "alpha = " option will set the alpha
#' value for confidence limit statistics.  Set the alpha as a decimal value
#' between 0 and 1.  For example, you can set a 90% confidence limit as
#' \code{alpha = 0.1}.
#' }
#' \item{\strong{edf}: Includes the number of regressors, the error degress of freedom,
#'   and the model r-square to the output dataset.
#' }
#' \item{\strong{noprint}: Whether to print the interactive report to the
#' viewer.  By default, the report is printed to the viewer. The "noprint"
#' option will inhibit printing.  You may inhibit printing globally by
#' setting the package print option to false:
#' \code{options("procs.print" = FALSE)}.
#' }
#' \item{\strong{outest}: The "outest" option is used to request the parameter
#' estimates and model fit summary statistic be sent to the output dataset.
#' The parameter estimates are identified as type "PARMS" on the output
#' dataset.
#' The "outest" dataset is the default output dataset, and this option
#' does not normally need to be passed.
#' }
#' \item{\strong{outseb}: The "outseb" option is used to request the standard
#' errors be sent to the output dataset.  The standard errors will be added
#' as a new row identified by type "SEB".  This request
#' can also be made by passing the "seb" keyword to the \code{stats} parameter.
#' }
#' \item{\strong{press}: Includes the predicted residual sum of squares
#' (PRESS) statistic in the output dataset.
#' }
#' \item{\strong{rsquare}: Include the r-square statistic in the output dataset.
#' The "rsquare" option has the same effect as the "edf" option.
#' }
#' \item{\strong{tableout}: The "tableout" option is used to send standard
#' errors, t-statistics, p-values, and confidence limits to the output
#' dataset.  These additional statistics are identified by types "STDERR",
#' "T", and "PVALUE". The confidence limits are identified by "LxxB" and "UxxB",
#' where "xx" is the alpha value in percentage terms. The "tableout" option
#' on the \code{options} parameter performs the same functions as the "table"
#' keyword on the \code{stats} parameter.
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
#'
#' @param data The input data frame for which to perform the regression analysis.
#' This parameter is required.
#' @param model A model for the regression to be performed.  The model can be
#' specified using either R syntax or SAS syntax. \code{model = var1 ~ var2 + var3} is
#' an example of R style model syntax. If you wish to pass multiple models using
#' R syntax, pass them in a list.  For SAS syntax, pass the model as a quoted string:
#' \code{model = "var1 = var2 var3"}.  To pass
#' multiple models using SAS syntax, pass them as a vector of strings. By default,
#' the models will be named "MODEL1", "MODEL2", etc.  If you want to name your
#' model, pass it as a named list or named vector.
#' @param by An optional by group. If you specify a by group, the input
#'  data will be subset on the by variable(s) prior to performing the regression.
#'  For multiple by variables, pass them as a quoted vector of variable names.
#'  You may also pass them unquoted using the \code{\link{v}} function.
#' @param stats Optional statistics keywords.  Valid values are "adjrsq", "clb",
#' "est", "edf", "hcc", "hccmethod", "mse", "p", "press", "rsquare",
#' "sse", "spec", "seb", and "table".  A single keyword may be passed with or
#' without quotes. Pass multiple keywords either as a quoted vector, or unquoted
#' vector using the \code{v()} function.  These statistics keywords largely
#' correspond to the options on the "model" statement in SAS. Most of them
#' control which statistics are added to the interactive report.  Some keywords
#' control statistics on the output dataset.  See the \strong{Statistics Keywords}
#' section for details on the purpose and target of each keyword.
#' @param output Whether or not to return datasets from the function. Valid
#' values are "out", "none", and "report".  Default is "out", and will
#' produce dataset output specifically designed for programmatic use. The "none"
#' option will return a NULL instead of a dataset or list of datasets.
#' The "report" keyword returns the datasets from the interactive report, which
#' may be different from the standard output. Note that some statistics are only
#' available on the interactive report.  The output parameter also accepts
#' data shaping keywords "long, "stacked", and "wide".
#' These shaping keywords control the structure of the output data. See the
#' \strong{Data Shaping} section for additional details. Note that
#' multiple output keywords may be passed on a
#' character vector. For example,
#' to produce both a report dataset and a "long" output dataset,
#' use the parameter \code{output = c("report", "out", "long")}.
#' @param weight The name of a variable to use as a weight for each observation.
#' The weight is commonly provided as the inverse of each variance.
#' @param options A vector of optional keywords. Valid values are: "alpha =",
#' "edf", "noprint", "outest", "outseb", "press", "rsquare", and "tableout".
#' The "alpha = " option will set the alpha
#' value for confidence limit statistics.  The default is 95% (alpha = 0.05).
#' The "noprint" option turns off the interactive report. For other options,
#' see the \strong{Options} section for explanations of each.
#' @param titles A vector of one or more titles to use for the report output.
#' @return Normally, the requested regression statistics are shown interactively
#' in the viewer, and output results are returned as a data frame.
#' If you request "report" datasets, they will be returned as a list.
#' You may then access individual datasets from the list using dollar sign
#' ($) syntax.
#' The interactive report can be turned off using the "noprint" option.
#' The output dataset can be turned off using the "none" keyword on the
#' \code{output} parameter. If the output dataset is turned off, the function
#' will return a NULL.
#' @import fmtr
#' @import tibble
#' @export
#' @examples
#' # Turn off printing for CRAN checks
#' options("procs.print" = FALSE)
#'
#' # Prepare sample data
#' set.seed(123)
#' dat <- cars
#' samplecar <- sample(c(TRUE, FALSE), nrow(cars), replace=TRUE, prob=c(0.6, 0.4))
#' dat$group <- ifelse(samplecar %in% seq(1, nrow(cars)), "Group A", "Group B")
#'
#' # Example 1: R Model Syntax
#' res1 <- proc_reg(dat, model = dist ~ speed)
#'
#' # View Results
#' res1
#' #    MODEL  TYPE DEPVAR     RMSE Intercept    speed dist
#' # 1 MODEL1 PARMS   dist 15.37959 -17.57909 3.932409   -1
#'
#' # Example 2: SAS Model Syntax
#' res2 <- proc_reg(dat, model = "dist = speed")
#'
#' # View Results
#' res2
#' #    MODEL  TYPE DEPVAR     RMSE Intercept    speed dist
#' # 1 MODEL1 PARMS   dist 15.37959 -17.57909 3.932409   -1
#'
#' # Example 3: Report Output
#' res3 <- proc_reg(dat, model = dist ~ speed, output = report)
#'
#' # View Results
#' res3
#' # $Observations
#' # stub NOBS
#' # 1 Number of Observations Read   50
#' # 2 Number of Observations Used   50
#' #
#' # $ANOVA
#' # stub DF    SUMSQ     MEANSQ     FVAL        PROBF
#' # 1           Model  1 21185.46 21185.4589 89.56711 1.489919e-12
#' # 2           Error 48 11353.52   236.5317       NA           NA
#' # 3 Corrected Total 49 32538.98         NA       NA           NA
#' #
#' # $Fitness
#' # RMSE DEPMEAN  COEFVAR       RSQ    ADJRSQ
#' # 1 15.37959   42.98 35.78312 0.6510794 0.6438102
#' #
#' # $Coefficients
#' # stub DF        EST    STDERR         T        PROBT
#' # 1 Intercept  1 -17.579095 6.7584402 -2.601058 1.231882e-02
#' # 2     speed  1   3.932409 0.4155128  9.463990 1.489919e-12
#'
#' # Example 4: By variable
#' res4 <- proc_reg(dat, model = dist ~ speed, by = group)
#'
#' # View Results
#' res4
#' #        BY  MODEL  TYPE DEPVAR     RMSE  Intercept    speed dist
#' # 1 Group A MODEL1 PARMS   dist 15.35049 -24.888326 4.275357   -1
#' # 2 Group B MODEL1 PARMS   dist 15.53676  -8.705547 3.484381   -1
#'
#' # Example 5: "tableout" Option
#' res5 <- proc_reg(dat, model = dist ~ speed, options = tableout)
#'
#' # View Results
#' res5
#' #    MODEL   TYPE DEPVAR     RMSE    Intercept        speed dist
#' # 1 MODEL1  PARMS   dist 15.37959 -17.57909489 3.932409e+00   -1
#' # 2 MODEL1 STDERR   dist 15.37959   6.75844017 4.155128e-01   NA
#' # 3 MODEL1      T   dist 15.37959  -2.60105800 9.463990e+00   NA
#' # 4 MODEL1 PVALUE   dist 15.37959   0.01231882 1.489919e-12   NA
#' # 5 MODEL1   L95B   dist 15.37959 -31.16784960 3.096964e+00   NA
#' # 6 MODEL1   U95B   dist 15.37959  -3.99034018 4.767853e+00   NA
#'
#' # Example 6: Multiple Models plus Statistics Keywords
#' res6 <- proc_reg(dat, model = list(mod1 = dist ~ speed,
#'                                    mod2 = speed ~ dist),
#'                  stats = v(press, seb))
#'
#' # View Results
#' res6
#' #  MODEL  TYPE DEPVAR      RMSE      PRESS   Intercept      speed        dist
#' # 1 mod1 PARMS   dist 15.379587 12320.2708 -17.5790949  3.9324088 -1.00000000
#' # 2 mod1   SEB   dist 15.379587         NA   6.7584402  0.4155128 -1.00000000
#' # 3 mod2 PARMS  speed  3.155753   526.2665   8.2839056 -1.0000000  0.16556757
#' # 4 mod2   SEB  speed  3.155753         NA   0.8743845 -1.0000000  0.01749448
proc_reg <- function(data,
                     model,
                     by = NULL,
                     stats = NULL,
                     #var = NULL,
                     output = NULL,
                     # freq = NULL, ?
                     # where = NULL, ?
                     weight = NULL,
                     options = NULL,
                     titles = NULL
) {

  # SAS seems to always ignore these
  # Not sure why R has an option to keep them
  missing <- FALSE

  # Deal with single value unquoted parameter values
  oweight <- deparse(substitute(weight, env = environment()))
  weight <- tryCatch({if (typeof(weight) %in% c("character", "NULL")) weight else oweight},
                    error = function(cond) {oweight})

  # Deal with single value unquoted parameter values
  oby <- deparse(substitute(by, env = environment()))
  by <- tryCatch({if (typeof(by) %in% c("character", "NULL")) by else oby},
                 error = function(cond) {oby})

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

  if (length(model) == 0) {
    stop("Model parameter is required.")
  }

  if (!is.null(by)) {
    if (!all(by %in% nms)) {

      stop(paste("Invalid by name: ", by[!by %in% nms], "\n"))
    }
  }

  if (!is.null(output)) {
    outs <- c("out", "report", "none", "wide", "long", "stacked")
    if (!all(tolower(output) %in% outs)) {

      stop(paste("Invalid output keyword: ", output[!tolower(output) %in% outs], "\n"))
    }

  }

  # https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_reg_syntax01.htm
  # https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_reg_syntax08.htm

  # Parameter checks for options
  if (!is.null(options)) {

    kopts <- c("alpha", "noprint",
               "tableout", "outseb", "outest", "press", "rsquare", "edf")

    # Dataset options
    # outsscp covout edf outseb outstb outvif pcomit? press rsquare

    # Display options
    # corr simple usscp all noprint alpha singular plots

    # Deal with "alpha =" by using name instead of value
    nopts <- names(options)

    if (is.null(nopts) & length(options) > 0)
      nopts <- options

    mopts <- ifelse(nopts == "", options, nopts)


    if (!all(tolower(mopts) %in% kopts)) {

      stop(paste0("Invalid options keyword: ", mopts[!tolower(mopts) %in% kopts], "\n"))
    }
  }

  if (!is.null(stats)) {

    sopts <- c("seb", "table", "est", "press",
               "rsquare", "edf", "adjrsq", "mse", "sse", "spec",
               "clb", "hcc", "hccmethod", "p")

    nsopts <- names(stats)

    if (is.null(nsopts) & length(stats) > 0)
      nsopts <- stats

    mnsopts <- ifelse(nsopts == "", stats, nsopts)


    if (!all(tolower(mnsopts) %in% sopts)) {

      stop(paste0("Invalid stats keyword: ", mnsopts[!tolower(mnsopts) %in% sopts], "\n"))
    }
  }

  if (!is.null(weight)) {

    if (length(weight) > 1)
      stop("Only one variable allowed for the weight parameter.")

    if (!weight %in% nms)
      stop("Variable specified for weight paramter not found in data.")

  }


  rptflg <- FALSE
  rptnm <- ""
  rptres <- NULL

  # Kill output request for report
  # Otherwise, this will mess up gen_output_reg
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
    rptres <- gen_report_reg(data,
                             model = model,
                             by = by, stats = stats,
                             view = view,
                             titles = titles,
                             opts = options,
                             output = output,
                             weight = weight)
  }

  # Get output datasets if requested
  if (has_output(output)) {
    res <- gen_output_reg(data,
                          model = model,
                          by = by,
                          stats = stats,
                          output = output,
                          opts = options,
                          weight = weight
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
  log_reg(data,
            model = model,
            by = by,
            stats = stats,
            output = output,
            weight = weight,
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


log_reg <- function(data,
                      model = NULL,
                      by = NULL,
                      stats = NULL,
                      output = NULL,
                      weight = NULL,
                      view = TRUE,
                      titles = NULL,
                      options = NULL,
                      outcnt = NULL) {

  ret <- c()

  indt <- paste0(rep(" ", 12), collapse = "")

  ret <- paste0("proc_reg: input data set ", nrow(data),
                " rows and ", ncol(data), " columns")

  if (!is.null(model))
    ret[length(ret) + 1] <- paste0(indt, "model: ",
                                   paste(model, collapse = " "))

  if (!is.null(by))
    ret[length(ret) + 1] <- paste0(indt, "by: ", paste(by, collapse = " "))

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


# Utilities -----------------------------------------------------------------

rlbls <- c(METHOD = "Method", VARIANCES = "Variances", NDF = "Num DF", DDF = "Den DF",
           FVAL = "F Value", PROBF = "Pr > F", mlbls)

reg_fc <- fcat(N = "%d", MEAN = "%.4f", STD = "%.4f", STDERR = "%.4f",
                 MIN = "%.4f", MAX = "%.4f", UCLM = "%.4f", LCLM = "%.4f",
                 DF = "%.3f", "T" = "%.2f", PROBT = "%.4f", NDF = "%.3f", DDF = "%.3f",
                 FVAL = "%.2f", PROBF = "%.4f", UCLMSTD = "%.4f", LCLMSTD = "%.4f",
                 log = FALSE)

get_output_specs_reg <- function(model, opts, output,
                                   report = FALSE) {
  ret <- list()
  mlist <- list()

  if (length(model) > 0) {
    if ("formula" %in% class(model)) {

      mlist <- list(model)

    } else if ("character" %in% class(model)) {

      mlist <- get_formulas(model)

    } else if ("list" %in% class(model)) {

      if ("formula" %in% class(model[[1]])) {

        mlist <- model

      } else {

        stop("Model list must contain a formula.")
      }

    }

  }

  if (length(mlist) > 0) {

    # if (report == TRUE) {

      nms <- names(mlist)

      mnum <- 1

      for (mdl in mlist) {

        # Concat model number
        if (length(nms) > 0 && !is.na(nms[mnum]) && nms[mnum] != "")
          vlbl <- nms[mnum]
        else
          vlbl <- paste0("MODEL", mnum)

        # Get dependant variable
        vr <- as.character(mdl)
        if (length(vr) > 2)
          vr <- vr[2]

        ret[[vlbl]] <- out_spec(var = vr, formula = mdl, report = report)

        mnum <- mnum + 1
      }

    # } else {
    #
    #   shp <- "wide"
    #   if (!is.null(output)) {
    #     if ("long" %in% output)
    #       shp <- "long"
    #     else if ("stacked" %in% output)
    #       shp <- "stacked"
    #
    #   }

      # stats <- c("n", "mean", "std", "stderr", "min", "max")
      #
      #
      # spcs[["Statistics"]] <- out_spec(var = var, stats = stats, shape = shp,
      #                                  type = FALSE, freq = FALSE, report = report)
      #
      # spcs[["ConfLimits"]] <- out_spec(var = var, stats = c("mean", "clm", "std", "clmstd"), shape = shp,
      #                                  types = FALSE, freq = FALSE, report = report)
      #
      # spcs[["TTests"]] <- out_spec(stats = c("df", "t", "probt"),
      #                              shape = shp,
      #                              type = FALSE, freq = FALSE,
      #                              var = var, report = report)
      #
      # spcs[["Equality"]] <- out_spec(stats = "dummy", var = var, types = FALSE, freq = FALSE, shape = shp)
      #

    #}
  }

  return(ret)

}


#' @import sasLM
#' @import common
#' @import fmtr
get_reg_report <- function(data, var, model, opts = NULL, weight = NULL, stats = NULL) {

  ret <- list()

  alph <- 1 - get_alpha(opts)

  hasHC <- FALSE
  if (has_option(stats, "spec") || has_option(stats, "hcc")) {
    hasHC <- TRUE
  }

  hasP <- FALSE
  if (has_option(stats, "p")) {
    hasP <- TRUE
  }

  if (!is.null(weight)) {
    reg <- REG(Formula = model, Data = data, conf.level = alph, summarize = TRUE,
               Weights = data[[weight]], HC = hasHC, Resid = hasP)
  } else {
    reg <- REG(Formula = model, Data = data, conf.level = alph, summarize = TRUE,
               HC = hasHC, Resid = hasP)
  }

  # Observations
  oreg <- get_obs(data, model)

  # Anova
  areg <- as.data.frame(unclass(reg$ANOVA), stringsAsFactors = FALSE)
  areg <- data.frame(stub = c("Model", "Error", "Corrected Total"),
                     areg, stringsAsFactors = FALSE)
  rownames(areg) <- NULL

  # Fitness
  freg <- as.data.frame(unclass(reg$Fitness), stringsAsFactors = FALSE)
  rownames(freg) <- NULL

  # Coefficients
  creg <- as.data.frame(unclass(reg$Coefficients), stringsAsFactors = FALSE)
  creg <- data.frame(stub = rownames(reg$Coefficients), creg, stringsAsFactors = FALSE)
  rownames(creg) <- NULL

  hc0reg <- NULL
  hc3reg <- NULL
  wreg <- NULL
  if (hasHC) {
    hc0reg <- as.data.frame(unclass(reg$HC0), stringsAsFactors = FALSE)
    hc3reg <- as.data.frame(unclass(reg$HC3), stringsAsFactors = FALSE)
    wreg <- as.data.frame(unclass(reg$`White Test`), stringsAsFactors = FALSE)
  }

  rreg <- NULL
  preg <- NULL
  if (hasP) {
     rreg <- reg$Residual
     preg <- reg$Fitted
  }

  # Create labels
  rlbls <- c(METHOD = "Method", VARIANCES = "Variances", NDF = "Num DF", DDF = "Den DF",
             FVAL = "F Value", PROBF = "Pr > F", SUMSQ = "Sum of Squares",
             MEANSQ = "Mean Square", RMSE = "Root MSE", DEPMEAN = "Dependent Mean",
             COEFVAR = "Coeff Var", RSQ = "R-Square", ADJRSQ = "Adj R-Sq",
             PRESS = "Pred. SS", PRERSQ = "Pred. R-Sq", EST = "Parameter Estimate",
             mlbls)

  # Replace alpha value in labels
  alph2 <- (1 - get_alpha(opts)) * 100
  rlbls[["UCLM"]] <- sprintf(tlbls[["UCLM"]], alph2)
  rlbls[["LCLM"]] <- sprintf(tlbls[["LCLM"]], alph2)
  rlbls[["UCLMSTD"]] <- sprintf(tlbls[["UCLMSTD"]], alph2)
  rlbls[["LCLMSTD"]] <- sprintf(tlbls[["LCLMSTD"]], alph2)

  # Create
  pfmt <- value(condition(is.na(x), "NA"),
                condition(x < .0001, "<.0001"),
                condition(TRUE, "%.4f"),
                log = FALSE)

  reg_fc <- fcat(N = "%d", MEAN = "%.4f", STD = "%.4f", STDERR = "%.4f",
                 MIN = "%.4f", MAX = "%.4f", UCLM = "%.4f", LCLM = "%.4f",
                 DF = "%d", "T" = "%.2f", PROBT = pfmt, NDF = "%.3f", DDF = "%.3f",
                 FVAL = "%.2f", PROBF = pfmt, UCLMSTD = "%.4f", LCLMSTD = "%.4f",
                 SUMSQ = "%.5f", MEANSQ = "%.5f", RMSE = "%.5f", DEPMEAN = "%.5f",
                 COEFVAR = "%.5f", RSQ = "%.4f", ADJRSQ = "%.4f", PRESS = "%.5f",
                 EST = "%.5f", STDERR = "%.5f", PRERSQ = "%.5f",
                 log = FALSE)

  lkp <- c("Df" = "DF", "Sum.Sq" = "SUMSQ", "Mean.Sq" = "MEANSQ", "F.value" = "FVAL",
           "Pr..F." = "PROBF", "Root.MSE" = "RMSE",
           "Coef.Var" = "COEFVAR", "R.square" = "RSQ", "Adj.R.sq" = "ADJRSQ",
           "PRESS" = "PRESS", "R2pred" = "PRERSQ", "Estimate" = "EST",
           "Std..Error" = "STDERR", "Lower.CL" = "LCLM", "Upper.CL" = "UCLM",
           "t.value" = "T", "Pr...t.." = "PROBT")

  # Add dependant mean
  lkp[paste0(var, ".Mean")] = "DEPMEAN"

  # Translate names
  names(areg) <- fapply(names(areg), lkp)
  names(freg) <- fapply(names(freg), lkp)
  names(creg) <- fapply(names(creg), lkp)

  # Assign DF
  creg$DF <- 1 # Not sure why this is always 1.

  # Kill predicted values
  fregpress <- freg$PRESS
  fregprersq <- freg$PRERSQ
  freg$PRESS <- NULL
  freg$PRERSQ <- NULL

  # Remove parens
  creg$stub <- sub("(Intercept)", "Intercept", creg$stub, fixed = TRUE)

  # Assign formats
  formats(areg) <- reg_fc
  formats(freg) <- reg_fc
  formats(creg) <- reg_fc

  # Assign labels
  labels(areg) <- c(rlbls, stub = "Source")
  labels(freg) <- rlbls
  labels(creg) <- c(rlbls, stub = "Variable")

  # Assign Widths
  # widths(areg) <- list(stub = 3)


  ret <- list()

  # Create return list
  ret[["Observations"]] <- oreg
  ret[["ANOVA"]] <- areg
  ret[["Fitness"]] <- freg

  cols <- c("stub", "DF", "EST", "STDERR", "T", "PROBT")
  if (!is.null(stats)) {
    if ("clb" %in% stats)
      cols <- c("stub", "DF", "EST", "STDERR", "T", "PROBT", "LCLM", "UCLM")
  }

  ret[["Coefficients"]] <- creg[ , cols]

  # Deal with HCC option
  if (has_option(stats, "hcc")) {

    hcctype <- 0
    if (has_option(stats, "hccmethod")) {

      hcctype <- get_option(stats, "hccmethod")
      if (!is.numeric(hcctype)) {
        warning("HCC method invalid.  Set to type 0.")
        hcctype <- 0
      }
    }

    tmpc <- ret[["Coefficients"]]
    tmphc <- hc0reg
    if (hcctype == 3)
      tmphc <- hc3reg

    tmpc$HCSTDERR <- tmphc$`Std. Error`
    tmpc$HCT <- tmphc$`t value`
    tmpc$HCPROBT <- tmphc$`Pr(>|t|)`

    labels(tmpc) <- list(HCSTDERR = "Std Error",
                         HCT = "t Value",
                         HCPROBT = "Pr>|t|")

    formats(tmpc) <- list(HCSTDERR = "%.5f",
                          HCT = "%.2f",
                          HCPROBT = pfmt)

    ret[["Coefficients"]] <- tmpc
  }

  # Deal with White's test/spec keyword
  if (has_option(stats, "spec")) {

    # Create data frame for White's test/Specs
    ret[["Specification"]] <- data.frame(DF = wreg[2, 1],
                                         CHISQ = wreg[1, 1],
                                         PCHISQ = wreg[3, 1],
                                         stringsAsFactors = FALSE)

    # Assign labels
    labels(ret[["Specification"]]) <- list(DF = "DF",
                                           CHISQ = "Chi-Square",
                                           PCHISQ = "Pr > ChiSq")

    # Assign formats
    formats(ret[["Specification"]]) <- list(DF = "%d",
                                            CHISQ = "%.2f",
                                            PCHISQ = pfmt)
  }

  if (hasP) {

    idcol <- seq(1, length(preg))
    vdat <- get_valid_obs(data, model)
    nmscol <- suppressWarnings(as.numeric(rownames(vdat)))

    if (!is.null(nmscol)) {
      if (all(!is.na(nmscol))) {
        idcol <- nmscol
      }
    }

    if (length(idcol) == nrow(vdat)) {

      st <- data.frame("stub" = idcol,
                       "DEPVAL" = vdat[[var]],
                       "PREVAL" = preg,
                       "RESID" = rreg)

      labels(st) <- list(stub = "Obs",
                         DEPVAL = "Dependant Variable",
                         PREVAL = "Predicted Value",
                         RESID = "Residual")

      formats(st) <- list(DEPVAL = "%.4f",
                          PREVAL = "%.4f",
                          RESID = "%.4f")

      ret[["Statistics"]] <- st

      resi <- data.frame(stub = c("Sum of Residuals",
                                  "Sum of Squared Residuals",
                                  "Predicted Residual SS (PRESS)"),
                         VALUE = c(round(sum(rreg), 5),
                                   sum(rreg ^ 2),
                                   fregpress))

      labels(resi) <- list(VALUE = "Value")

      formats(resi) <- list(VALUE = "%.5f")

      ret[["Residuals"]] <- resi

    } else {

      warning("There was a problem creating the statistics and residuals.  Invalid row counts.")
    }
  }

  return(ret)
}

#' @import sasLM
#' @import common
#' @import fmtr
get_reg_output<- function(data, var, model, modelname, opts = NULL, stats = NULL,
                          byvars = NULL, weight = NULL) {

  ret <- list()

  # Get alpha value
  alph <- 1 - get_alpha(opts)

  # Translate options to stat keywords
  if (has_option(opts, "tableout"))
      stats <- append(stats, "table")

  if (has_option(opts, "outseb"))
    stats <- append(stats, "seb")

  if (has_option(opts, "edf"))
    stats <- append(stats, "edf")

  if (has_option(opts, "rsquare"))
    stats <- append(stats, "rsquare")

  if (has_option(opts, "press"))
    stats <- append(stats, "press")

  # Get statistics
  if (!is.null(weight)) {
    reg <- REG(Formula = model, Data = data, conf.level = alph, summarize = TRUE,
               Weights = data[[weight]])
  } else {
    reg <- REG(Formula = model, Data = data, conf.level = alph, summarize = TRUE)
  }

  # Observations
  oreg <- get_obs(data, model)

  # Anova
  areg <- as.data.frame(unclass(reg$ANOVA), stringsAsFactors = FALSE)
  areg <- data.frame(stub = c("Model", "Error", "Corrected Total"),
                     areg, stringsAsFactors = FALSE)
  rownames(areg) <- NULL

  # Fitness
  freg <- as.data.frame(unclass(reg$Fitness), stringsAsFactors = FALSE)
  rownames(freg) <- NULL

  # Coefficients
  creg <- as.data.frame(unclass(reg$Coefficients), stringsAsFactors = FALSE)
  creg <- data.frame(stub = rownames(reg$Coefficients), creg, stringsAsFactors = FALSE)
  rownames(creg) <- NULL


  lkp <- c("Df" = "DF", "Sum.Sq" = "SUMSQ", "Mean.Sq" = "MEANSQ", "F.value" = "FVAL",
           "Pr..F." = "PROBF", "Root.MSE" = "RMSE",
           "Coef.Var" = "COEFVAR", "R.square" = "RSQ", "Adj.R.sq" = "ADJRSQ",
           "PRESS" = "PRESS", "R2pred" = "PRERSQ", "Estimate" = "EST",
           "Std..Error" = "STDERR", "Lower.CL" = "LCLM", "Upper.CL" = "UCLM",
           "t.value" = "T", "Pr...t.." = "PROBT")

  # Add dependtant mean
  lkp[paste0(var, ".Mean")] = "DEPMEAN"

  # Translate names
  names(areg) <- fapply(names(areg), lkp)
  names(freg) <- fapply(names(freg), lkp)
  names(creg) <- fapply(names(creg), lkp)


  # Remove parens
  creg$stub <- sub("(Intercept)", "Intercept", creg$stub, fixed = TRUE)


  vrs <- get_vars(model)

  # Create output list
  ret <- list()

  # Assign byvars if available
  if (!is.null(byvars)) {
    for (bnm in names(byvars)) {
      ret[[bnm]] <- as.character(byvars[bnm])
    }
  }

  # Assign base variables
  ret[["MODEL"]] <- modelname
  ret[["TYPE"]] <- "PARMS"
  ret[["DEPVAR"]] <- var
  ret[["RMSE"]] <- freg[1, "RMSE"]

  # Optional Statistics
  if (has_option(stats, "PRESS"))
    ret[["PRESS"]] <- freg$PRESS

  # Assign independent variables
  for (i in seq_len(length(creg$stub))) {

    ret[[creg$stub[i]]] <- creg[i, "EST"]
  }

  # Assign dependent variable
  ret[[var]] <- -1


  # Optional Statistics
  if (has_option(stats, "RSQUARE") ||
      has_option(stats, "EDF") ||
      has_option(stats, "ADJRSQ") ||
      has_option(stats, "MSE") ||
      has_option(stats, "SSE") ||
      has_option(stats, "SBC")) {

    ret[["IN"]] <- length(creg$stub) - 1
    ret[["P"]] <- length(creg$stub)
    ret[["EDF"]] <- areg[2, "DF"]

    if (has_option(stats, "MSE"))
      ret[["MSE"]] <- areg[2, "MEANSQ"]

    if (has_option(stats, "SSE"))
      ret[["SSE"]] <- areg[2, "SUMSQ"]

    ret[["RSQ"]] <- freg$RSQ

    if (has_option(stats, "ADJRSQ"))
      ret[["ADJRSQ"]] <- freg$ADJRSQ

    if (has_option(stats, "SBC"))
      ret[["SBC"]] <- freg$ADJRSQ

  }

  # Convert to data frame
  ret <- as.data.frame(ret, stringsAsFactors = FALSE)

  if (has_option(stats, "SEB")) {

     lrw <- nrow(ret) + 1

     ret[lrw, "MODEL"] <- modelname
     ret[lrw, "TYPE"] <- "SEB"
     ret[lrw, "DEPVAR"] <- var
     ret[lrw, "RMSE"] <- freg[1, "RMSE"]

     # Assign independent variables
     for (i in seq_len(length(creg$stub))) {

       ret[lrw, creg$stub[i]] <- creg[i, "STDERR"]
     }

     # Assign dependent variable
     ret[lrw, var] <- -1
  }

  if (has_option(stats, "TABLE")) {

    # STDERR
    lrw <- nrow(ret) + 1

    # Assign identifying information
    ret[lrw, "MODEL"] <- modelname
    ret[lrw, "TYPE"] <- "STDERR"
    ret[lrw, "DEPVAR"] <- var
    ret[lrw, "RMSE"] <- freg[1, "RMSE"]

    # Assign independent variables
    for (i in seq_len(length(creg$stub))) {

      ret[lrw, creg$stub[i]] <- creg[i, "STDERR"]
    }

    # Assign dependent variable
    ret[lrw, var] <- NA

    # T
    lrw <- nrow(ret) + 1

    # Assign identifying information
    ret[lrw, "MODEL"] <- modelname
    ret[lrw, "TYPE"] <- "T"
    ret[lrw, "DEPVAR"] <- var
    ret[lrw, "RMSE"] <- freg[1, "RMSE"]

    # Assign independent variables
    for (i in seq_len(length(creg$stub))) {

      ret[lrw, creg$stub[i]] <- creg[i, "T"]
    }

    # Assign dependent variable
    ret[lrw, var] <- NA

    # PVALUE
    lrw <- nrow(ret) + 1

    # Assign identifying information
    ret[lrw, "MODEL"] <- modelname
    ret[lrw, "TYPE"] <- "PVALUE"
    ret[lrw, "DEPVAR"] <- var
    ret[lrw, "RMSE"] <- freg[1, "RMSE"]

    # Assign independent variables
    for (i in seq_len(length(creg$stub))) {

      ret[lrw, creg$stub[i]] <- creg[i, "PROBT"]
    }

    # Assign dependent variable
    ret[lrw, var] <- NA


    # L(alpha)B
    lrw <- nrow(ret) + 1

    # Assign identifying information
    ret[lrw, "MODEL"] <- modelname
    ret[lrw, "TYPE"] <- paste0("L", alph * 100, "B")
    ret[lrw, "DEPVAR"] <- var
    ret[lrw, "RMSE"] <- freg[1, "RMSE"]

    # Assign independent variables
    for (i in seq_len(length(creg$stub))) {

      ret[lrw, creg$stub[i]] <- creg[i, "LCLM"]
    }

    # Assign dependent variable
    ret[lrw, var] <- NA

    # U(alpha)B
    lrw <- nrow(ret) + 1

    # Assign identifying information
    ret[lrw, "MODEL"] <- modelname
    ret[lrw, "TYPE"] <- paste0("U", alph * 100, "B")
    ret[lrw, "DEPVAR"] <- var
    ret[lrw, "RMSE"] <- freg[1, "RMSE"]

    # Assign independent variables
    for (i in seq_len(length(creg$stub))) {

      ret[lrw, creg$stub[i]] <- creg[i, "UCLM"]
    }

    # Assign dependent variable
    ret[lrw, var] <- NA
  }



  return(ret)
}


# Function to set two tables with unequal columns
# Possibly needed for output datasets
add_class_reg <- function(rtbl, ctbl) {


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

# Specifically for shaping results of get_reg_output()
shape_reg_data <- function(ds, shape) {

  # Assumed to be wide
  ret <- ds

  if (!is.null(shape)) {
    if (all(shape == "long")) {

      bv <- c("MODEL", "DEPVAR")
      # if (!"CLASS" %in% names(ds))
      #   bv <- "METHOD"

      bnms <- find.names(ds, "BY*")
      if (!is.null(bnms))
        bv <- c(bnms, bv)

      ret <- proc_transpose(ds, by = bv,
                            name = "STAT", id = "TYPE",
                            log = FALSE)


    } else if (all(shape == "stacked")) {


      bv <- c("MODEL", "DEPVAR", "TYPE")
      # if (!"CLASS" %in% names(ds))
      #   bv <- c("VAR", "METHOD")

      bnms <- find.names(ds, "BY*")
      if (!is.null(bnms))
        bv <- c(bnms, bv)

      ret <- proc_transpose(ds, by = bv, name = "STAT",
                            log = FALSE)

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
gen_report_reg <- function(data,
                           model = NULL,
                           by = NULL,
                           stats = NULL,
                           opts = NULL,
                           output = NULL,
                           view = TRUE,
                           titles = NULL,
                           weight = NULL) {


  spcs <- get_output_specs_reg(model, opts, output, report = TRUE)

  nms <- names(spcs)

  # Declare return list
  res <- list()
  byres <- list()

  # Assign CL Percentage on Labels
  alph <- (1 - get_alpha(opts)) * 100


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

  # Loop through models
  for (nm in nms) {

    outp <- spcs[[nm]]
    vnm <- outp$var

    # Loop through by groups
    for (j in seq_len(length(dtlst))) {

      # Get table for this by group
      dt <- dtlst[[j]]
      bynm <- nm
      if (length(bylbls) > 0)
        bynm <- paste0(nm, ":", bylbls[j])

      # for (i in seq_len(length(outreq))) {

      # data, var, model, report = TRUE, opts = NULL,
      byres[[bynm]] <- get_reg_report(dt, vnm, outp$formula, opts = opts,
                                      weight = weight, stats = stats)

      # Assign titles
      ttls <- c()
      ttls[1] <- paste0("Model: ", nm)
      ttls[2] <- paste0("Dependent Variable: ", vnm)
      if (length(bylbls) > 0)
        ttls[3] <- bylbls[j]

      attr(byres[[bynm]][[1]], "ttls") <- ttls

        # # Get class-level t-tests
        # if (!is.null(class)) {
        #
        #   ctbl <- get_class_ttest(dt, outp$var, class, TRUE, opts)
        # }
        #
        # if (is.null(class) ||
        #     (!is.null(class) && tnm %in% c("Statistics", "ConfLimits"))) {   # Fix this
        #
        #   # data, var, class, outp, freq = TRUE,
        #   # type = NULL, byvals = NULL
        #   #outp <- out_spec(stats = stats, shape = "wide")
        #   smtbl <- get_class_report(dt, outp$var, class, outp, freq = FALSE, opts = opts)
        #
        #
        #
        #   # Add in class-level tests if needed
        #   if (!is.null(class)) {
        #
        #     smtbl <- add_class_ttest(smtbl, ctbl[[tnm]])
        #   }
        #
        # } else if (!is.null(class)) {
        #
        #   # Add extra tests for class comparison
        #   smtbl <- ctbl[[tnm]]
        #
        # }

        # if (tnm == "Statistics") {
        #
        #   if (!is.null(outp$varlbl)) {
        #     if (!is.null(paired)) {
        #       attr(smtbl, "ttls") <- paste0("Difference: ", outp$varlbl)
        #     } else {
        #       attr(smtbl, "ttls") <- paste0("Variable: ", outp$varlbl)
        #     }
        #
        #   } else {
        #     attr(smtbl, "ttls") <- paste0("Variable: ", outp$var)
        #   }
        # }


        # Add spanning headers if there are by groups
        # if (!is.null(by) & !is.null(smtbl)) {
        #
        #   # Add spanning headers
        #   # spn <- span(1, ncol(smtbl), label = bylbls[j], level = 1)
        #   # attr(smtbl, "spans") <- list(spn)
        #
        #   bynm <-  bylbls[j]
        #
        #   if (tnm == "Statistics") {
        #
        #     attr(smtbl, "ttls") <- c(attr(smtbl, "ttls"), bynm)
        #
        #   }
        #
        # }

        # Add default formats
        # fmt <- "%.4f"
    #    formats(smtbl) <- ttest_fc
        # for (cnm in names(smtbl)) {
        #
        #   if (typeof(smtbl[[cnm]]) %in% c("double")) {
        #
        #     attr(smtbl[[cnm]], "format") <- fmt
        #   }
        #
        # }


        # Assign labels
        # if (is.null(class))
        #   labels(smtbl) <- tlbls
        # else {
        #
        #   cv <- class
        #   if (length(class) == 1)
        #     cnms <- "CLASS"
        #   else
        #     cnms <- paste0("CLASS", seq(1, length(class)))
        #
        #   names(cv) <- cnms
        #
        #   labels(smtbl) <- append(as.list(cv), tlbls)
        #
        # }

        # Kill var variable for reports
        # if ("VAR" %in% names(smtbl)) {
        #
        #   smtbl[["VAR"]] <- NULL
        #
        # } else {
        #
        #   if (!is.null(outp$varlbl))
        #     smtbl[["VAR"]] <- outp$varlbl
        # }

        # Convert to tibble if incoming data is a tibble
        # if ("tbl_df" %in% class(data)) {
        #   res[[nm]] <- as_tibble(smtbl)
        # } else {
        #   res[[nm]] <- smtbl
        # }

      # } outreq

      #byres[[bynm]] <- res
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
        titles <- "The REG Function"


      # Add spanning headers
      nmsret <- names(ret)
      if ("ANOVA" %in% nmsret) {
        spn <- span(1, ncol(ret$ANOVA), label = paste("Analysis of Variance"), level = 1)
        attr(ret$ANOVA, "spans") <- list(spn)

      }

      if ("Coefficients" %in% nmsret) {
        if (has_option(stats, "hcc")) {

          spn2 <- span(1, ncol(ret$Coefficients), label = paste("Parameter Estimates"),
                       level = 2)
          spn1 <- span("HCSTDERR", "HCPROBT", label = paste("Heteroscedasticity Consistent"),
                       level = 1)
          attr(ret$Coefficients, "spans") <- list(spn1, spn2)

        } else {
          spn <- span(1, ncol(ret$Coefficients), label = paste("Parameter Estimates"),
                      level = 1)
          attr(ret$Coefficients, "spans") <- list(spn)
        }


      }

      if ("Specification" %in% nmsret) {
        spn <- span(1, ncol(ret$Specification),
                    label = paste("Test of First and Second Moment Specification"),
                    level = 1)
        attr(ret$Specification, "spans") <- list(spn)

      }

      if ("Statistics" %in% nmsret) {

        attr(ret$Statistics, "spans") <- list(span(1, ncol(ret$Statistics),
                                             label = "Output Statistics",
                                             level = 1))

      }


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
gen_output_reg <- function(data,
                           model = NULL,
                           by = NULL,
                           stats = NULL,
                           output = NULL,
                           opts = NULL,
                           weight = NULL) {

  # Prepare specs
  spcs <- get_output_specs_reg(model, opts, output, report = FALSE)

  res <- list()
  ctbl <- NULL

  if (length(spcs) > 0) {


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

    nms <- names(spcs)
    for (j in seq_len(length(bdat))) {

      dat <- bdat[[j]]

      # Deal with by variable values
      bynm <- NULL
      if (!is.null(bynms)) {
        bynm <- strsplit(bynms[j], "|", fixed = TRUE)[[1]]
        names(bynm) <- byn
      }

      byres <- NULL

      for (i in seq_len(length(spcs))) {

        outp <- spcs[[i]]
        nm <- nms[i]

        #if (is.null(class)) {

          # data, var, model, modelname, opts = NULL,
          # byvar = NULL, byval = NULL

          # Always add type 0
          tmpby <- get_reg_output(dat, var = outp$var,
                                  model = outp$formula, modelname = nm,
                                  opts = opts, stats = stats,
                                  byvars = bynm, weight = weight
                                  )

          # Get rid of temporary var names
          # tmpby <- fix_var_names(tmpby, outp$var, outp$varlbl, outp$shape, tnm)


          if (is.null(tmpres))
            tmpres <- tmpby
          else {
            tmpres <- perform_set(tmpres, tmpby)
          }

        #}


        # if (!is.null(class)) {
        #
        #   for (vr in outp$var) {
        #
        #     # Get class-level t-tests
        #     ctbl <- get_class_ttest(dat, vr, class, FALSE, opts,
        #                             byvar = by, byval = bynm,
        #                             shape = outp$shape)
        #
        #
        #     if (nm %in% c("Statistics", "ConfLimits")) {
        #
        #       # Get standard statistics
        #       tmpcls <- get_class_output(dat, var = vr,
        #                                  class = class, outp = outp,
        #                                  freq = outp$parameters$freq,
        #                                  type = outp$parameters$type,
        #                                  byvals = bynm, opts = opts,
        #                                  stats = outp$stats)
        #
        #       tmpcls <- add_class_ttest(tmpcls, ctbl[[nm]])
        #
        #     } else {
        #
        #       tmpcls <- ctbl[[nm]]
        #     }
        #
        #
        #     if (is.null(tmpres))
        #       tmpres <- tmpcls
        #     else {
        #
        #       if (outp$shape == "long")
        #         tmpres[[vr]] <- tmpcls[[vr]]
        #       else
        #         tmpres <- rbind(tmpres, tmpcls)
        #
        #     }
        #
        #   }
        #
        # }

        # if (is.null(byres))
        #   byres <- tmpres
        # else {
        #   byres <- perform_set(byres, tmpres)
        # }
      }

      # tmpres <- byres
      # byres <- NULL


      # Replace VAR value
      # if (!is.null(paired)) {
      #   if ("DIFF" %in% names(tmpres)) {
      #     if (!is.null(outp$varlbl))
      #       tmpres[["DIFF"]] <- outp$varlbl
      #     else
      #       tmpres[["DIFF"]] <- outp$var
      #   }
      # } else if ((!is.null(var) && is.null(class))) {
      #   if (outp$shape != "stacked") {
      #     if ("VAR" %in% names(tmpres)) {
      #       if (!is.null(outp$varlbl))
      #         tmpres[["VAR"]] <- outp$varlbl
      #       else
      #         tmpres[["VAR"]] <- outp$var
      #     }
      #   }
      # }

      # Kill type variable for output datasets
      # if ("TYPE" %in% names(tmpres)) {
      #
      #   tmpres[["TYPE"]] <- NULL
      # }

      # System Labels
      # if (!is.null(tmpres))
      #   labels(tmpres) <- append(tlbls, bylbls)

      # Class labels
      # clbls <- NULL
      # if (!is.null(class)) {
      #   if (length(class) == 1)
      #     cnms <- "CLASS"
      #   else {
      #     cnms <- paste0("CLASS", seq(1, length(class)))
      #   }
      #   clbls <- as.list(class)
      #   names(clbls) <- cnms
      #
      #   labels(tmpres) <- clbls
      # }

      # User labels
      # if (!is.null(outp$label))
      #   labels(tmpres) <- outp$label

      # Formats
      # if (!is.null(outp$format))
      #   formats(tmpres) <- outp$format

      # Reset rownames
      rownames(tmpres) <- NULL



      res[[nms[i]]]  <- tmpres

    } # bygrp
  }  # spcs > 0

  if (length(res) == 1) {
    res <- res[[1]]

    if (has_option(output, "long"))
      res <- shape_reg_data(res, "long")

    if (has_option(output, "stacked"))
      res <- shape_reg_data(res, "stacked")

  }


  return(res)

}



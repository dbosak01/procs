
# TTest Procedure ---------------------------------------------------------



# @title Calculates a Regression
# @encoding UTF-8
# @description The \code{proc_reg} function performs a regression
# for selected variables on the input dataset.
# The variables are identified on the \code{var} parameter or the \code{paired}
# parameter. The function will calculate a standard set of T-Test statistics.
# Results are displayed in
# the viewer interactively and returned from the function.
# @details
# The \code{proc_reg} function is for ...
#
# @section Interactive Output:
# By default, \code{proc_ttest} results will
# be sent to the viewer as an HTML report.  This functionality
# makes it easy to get a quick analysis of your data. To turn off the
# interactive report, pass the "noprint" keyword
# to the \code{options} parameter.
#
# The \code{titles} parameter allows you to set one or more titles for your
# report.  Pass these titles as a vector of strings.
#
# The exact datasets used for the interactive report can be returned as a list.
# To return these datasets, pass
# the "report" keyword on the \code{output} parameter. This list may in
# turn be passed to \code{\link{proc_print}} to write the report to a file.
#
# @section Dataset Output:
# Dataset results are also returned from the function by default.
# \code{proc_ttest} typically returns multiple datasets in a list. Each
# dataset will be named according to the category of statistical
# results.  There are three standard categories: "Statistics",
# "ConfLimits", and "TTests". For the class style analysis, the function
# also returns a dataset called "Equality" that shows the Folded F analysis.
#
# The output datasets generated are optimized for data manipulation.
# The column names have been standardized, and additional variables may
# be present to help with data manipulation.  For example, the by variable
# will always be named "BY".  In addition, data values in the
# output datasets are intentionally not rounded or formatted
# to give you the most accurate numeric results.
#
# @section Options:
# The \code{proc_reg} function recognizes the following options.  Options may
# be passed as a quoted vector of strings, or an unquoted vector using the
# \code{v()} function.
# \itemize{
# \item{\strong{alpha = }: The "alpha = " option will set the alpha
# value for confidence limit statistics.  Set the alpha as a decimal value
# between 0 and 1.  For example, you can set a 90% confidence limit as
# \code{alpha = 0.1}.
# }
# \item{\strong{h0}: The "h0 =" option is used to set the baseline mean value
# for testing a single variable.  Pass the option as a name/value pair,
# such as \code{h0 = 95}.
# }
# \item{\strong{noprint}: Whether to print the interactive report to the
# viewer.  By default, the report is printed to the viewer. The "noprint"
# option will inhibit printing.  You may inhibit printing globally by
# setting the package print option to false:
# \code{options("procs.print" = FALSE)}.
# }
# }
# @section Data Shaping:
# The output datasets produced by the function can be shaped
# in different ways. These shaping options allow you to decide whether the
# data should be returned long and skinny, or short and wide. The shaping
# options can reduce the amount of data manipulation necessary to get the
# data into the desired form. The
# shaping options are as follows:
# \itemize{
# \item{\strong{long}: Transposes the output datasets
# so that statistics are in rows and variables are in columns.
# }
# \item{\strong{stacked}: Requests that output datasets
# be returned in "stacked" form, such that both statistics and
# variables are in rows.
# }
# \item{\strong{wide}: Requests that output datasets
# be returned in "wide" form, such that statistics are across the top in
# columns, and variables are in rows. This shaping option is the default.
# }
# }
# These shaping options are passed on the \code{output} parameter.  For example,
# to return the data in "long" form, use \code{output = "long"}.
#
# @param data The input data frame for which to calculate summary statistics.
# This parameter is required.
# @param model A vector of paired variables to perform a paired T-Test on.
# Variables should
# be separated by a star (*).  The entire string should be quoted, for example,
# \code{paired = "var1 * var2"}.  To test multiple pairs, place the pairs in a
# quoted vector
# : \code{paired = c("var1 * var2", "var3 * var4")}.  The parameter does not
# accept parenthesis, hyphens, or any other shortcut syntax.
# @param by An optional by group. If you specify a by group, the input
# data will be subset on the by variable(s) prior to performing any
# statistics.
# @param output Whether or not to return datasets from the function. Valid
# values are "out", "none", and "report".  Default is "out", and will
# produce dataset output specifically designed for programmatic use. The "none"
# option will return a NULL instead of a dataset or list of datasets.
# The "report" keyword returns the datasets from the interactive report, which
# may be different from the standard output. The output parameter also accepts
# data shaping keywords "long, "stacked", and "wide".
# These shaping keywords control the structure of the output data. See the
# \strong{Data Shaping} section for additional details. Note that
# multiple output keywords may be passed on a
# character vector. For example,
# to produce both a report dataset and a "long" output dataset,
# use the parameter \code{output = c("report", "out", "long")}.
# @param options A vector of optional keywords. Valid values are: "alpha =",
# "h0 =", and "noprint". The "alpha = " option will set the alpha
# value for confidence limit statistics.  The default is 95% (alpha = 0.05).
# The "h0 = " option sets the baseline hypothesis value for single-variable
# hypothesis testing.  The "noprint" option turns off the interactive report.
# @param titles A vector of one or more titles to use for the report output.
# @return Normally, the requested T-Test statistics are shown interactively
# in the viewer, and output results are returned as a list of data frames.
# You may then access individual datasets from the list using dollar sign ($)
# syntax.
# The interactive report can be turned off using the "noprint" option, and
# the output datasets can be turned off using the "none" keyword on the
# \code{output} parameter.
# @import fmtr
# @import tibble
# @export
# @examples
# # Turn off printing for CRAN checks
# options("procs.print" = FALSE)
#
# # Prepare sample data
# dat1 <- subset(sleep, group == 1, c("ID", "extra"))
# dat2 <- subset(sleep, group == 2, c("ID", "extra"))
# dat <- data.frame(ID = dat1$ID, group1 = dat1$extra, group2 = dat2$extra)
#
# # View sample data
# dat
# #    ID group1 group2
# # 1   1    0.7    1.9
# # 2   2   -1.6    0.8
# # 3   3   -0.2    1.1
# # 4   4   -1.2    0.1
# # 5   5   -0.1   -0.1
# # 6   6    3.4    4.4
# # 7   7    3.7    5.5
# # 8   8    0.8    1.6
# # 9   9    0.0    4.6
# # 10 10    2.0    3.4
#
proc_reg <- function(data,
                       model = NULL,
                       by = NULL,
                       #var = NULL,
                       output = NULL,
                       # freq = NULL, ?
                       # weight = NULL, ?
                       options = NULL,
                       titles = NULL
) {

  # SAS seems to always ignore these
  # Not sure why R has an option to keep them
  missing <- FALSE


  # Deal with single value unquoted parameter values
  omodel <- deparse(substitute(model, env = environment()))
  model <- tryCatch({if (typeof(model) %in% c("character", "NULL")) model else omodel},
                    error = function(cond) {omodel})

  # Deal with single value unquoted parameter values
  oby <- deparse(substitute(by, env = environment()))
  by <- tryCatch({if (typeof(by) %in% c("character", "NULL")) by else oby},
                 error = function(cond) {oby})

  # ovar <- deparse(substitute(var, env = environment()))
  # var <- tryCatch({if (typeof(var) %in% c("character", "NULL")) var else ovar},
  #                 error = function(cond) {ovar})

  oopt <- deparse(substitute(options, env = environment()))
  options <- tryCatch({if (typeof(options) %in% c("integer", "double", "character", "NULL")) options else oopt},
                      error = function(cond) {oopt})

  oout <- deparse(substitute(output, env = environment()))
  output <- tryCatch({if (typeof(output) %in% c("character", "NULL")) output else oout},
                     error = function(cond) {oout})

  # opaired <- deparse(substitute(paired, env = environment()))
  # paired <- tryCatch({if (typeof(paired) %in% c("character", "NULL")) paired else opaired},
  #                    error = function(cond) {opaired})

  # Parameter checks
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

  # https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_ttest_overview.htm
  # https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_ttest_syntax01.htm
  # aopts <- c("alpha=", "dist", "H0=", "sides", "test", "tost", "crossover=") # analysis options
  # dopts <- c("ci", "cochran", "plots") # display options
  # oopts <- c("byvar", "nobyvar")  # Output ordering

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
                             by = by, #var = var, class = class,
                             view = view,
                             titles = titles, #weight = weight,
                             opts = options, output = output)
  }

  # Get output datasets if requested
  if (has_output(output)) {
    res <- gen_output_reg(data,
                          model = model,
                          by = by,
                          # class = class,
                          # var = var,
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
  log_reg(data,
            model = model,
            by = by,
            # class = class,
            # var = var,
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


log_reg <- function(data,
                      model = NULL,
                      by = NULL,
                      # class = NULL,
                      # var = NULL,
                      output = NULL,
                      #weight = NULL,
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

  # if (!is.null(var))
  #   ret[length(ret) + 1] <- paste0(indt, "var: ",
  #                                  paste(var, collapse = " "))

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

rlbls <- c(METHOD = "Method", VARIANCES = "Variances", NDF = "Num DF", DDF = "Den DF",
           FVAL = "F Value", PROBF = "Pr > F", mlbls)

reg_fc <- fcat(N = "%d", MEAN = "%.4f", STD = "%.4f", STDERR = "%.4f",
                 MIN = "%.4f", MAX = "%.4f", UCLM = "%.4f", LCLM = "%.4f",
                 DF = "%.3f", "T" = "%.2f", PROBT = "%.4f", NDF = "%.3f", DDF = "%.3f",
                 FVAL = "%.2f", PROBF = "%.4f", UCLMSTD = "%.4f", LCLMSTD = "%.4f",
                 log = FALSE)

get_output_specs_reg <- function(data, model, opts, output,
                                   report = FALSE) {

  dat <- data
  spcs <- list()
  mlist <- list()

  fla <- formula(x ~y)

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
  }

  ret <- list(data = dat, outreq = spcs)

  return(ret)

}


#' @import sasLM
#' @import common
get_class_reg <- function(data, var, class, report = TRUE, opts = NULL,
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

# Specifically for shaping results of get_class_ttest()
shape_reg_data <- function(ds, shape, copy = NULL) {

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
gen_report_reg <- function(data,
                           model = NULL,
                           by = NULL,
                           class = NULL,
                           var = NULL,
                           paired = NULL,
                           weight = NULL,
                           opts = NULL,
                           output = NULL,
                           view = TRUE,
                           titles = NULL) {


  spcs <- get_output_specs_reg(data, model = model,
                               opts, output, report = TRUE)

  data <- spcs$data
  outreq <- spcs$outreq
  nms <- names(outreq)

  # Declare return list
  res <- list()
  byres <- list()

  # Assign CL Percentage on Labels
  alph <- (1 - get_alpha(opts)) * 100
  rlbls[["UCLM"]] <- sprintf(tlbls[["UCLM"]], alph)
  rlbls[["LCLM"]] <- sprintf(tlbls[["LCLM"]], alph)
  rlbls[["UCLMSTD"]] <- sprintf(tlbls[["UCLMSTD"]], alph)
  rlbls[["LCLMSTD"]] <- sprintf(tlbls[["LCLMSTD"]], alph)

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
gen_output_reg <- function(data,
                           model = NULL,
                           by = NULL,
                           class = NULL,
                           var = NULL,
                           paired = NULL,
                           weight = NULL,
                           output = NULL,
                           opts = NULL) {

  # Assign CL Percentage on Labels
  alph <- (1 - get_alpha(opts)) * 100
  # tlbls[["UCLM"]] <- sprintf(tlbls[["UCLM"]], alph)
  # tlbls[["LCLM"]] <- sprintf(tlbls[["LCLM"]], alph)
  # tlbls[["UCLMSTD"]] <- sprintf(tlbls[["UCLMSTD"]], alph)
  # tlbls[["LCLMSTD"]] <- sprintf(tlbls[["LCLMSTD"]], alph)

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

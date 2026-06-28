

# General Linear Model ----------------------------------------------------



#' @title Calculates a General Linear Model
#' @encoding UTF-8
#' @description The \code{proc_glm} function performs an analysis of variance
#' for one or more general linear models.  The model(s) are passed on the
#' \code{model} parameter, and the input dataset is passed on the \code{data}
#' parameter.  Categorical (classification) variables are identified using the
#' \code{class} parameter.  The \code{stats}
#' parameter allows you to request additional statistics, similar to the
#' model options in SAS.  The \code{by}
#' parameter allows you to subset the data into groups and run the model on each
#' group.  The \code{weight} parameter lets you assign a weight to each observation
#' in the dataset.  The \code{output} and \code{options} parameters provide
#' additional customization of the results.
#' @details
#' The \code{proc_glm} function is a general-purpose analysis of variance
#' function.  It uses the principle of least squares to fit general linear
#' models.  Among the statistical methods available in \code{proc_glm} are
#' regression, analysis of variance, and analysis of covariance.  The function
#' produces a dataset output by default, and, when working in RStudio,
#' also produces an interactive report.  The function has many convenient options
#' for what statistics are produced and how the analysis is performed. All
#' statistical output from \code{proc_glm} matches SAS.
#'
#' Unlike \code{\link{proc_reg}}, the \code{proc_glm} function allows you to
#' specify classification (categorical) variables on the \code{class} parameter.
#' Classification variables are coded into the design matrix as a set of
#' indicator variables, which makes it possible to fit analysis of variance
#' and analysis of covariance models in addition to ordinary regression.
#'
#' A model may be specified using R model syntax or SAS model syntax.  To use
#' SAS syntax, the model statement must be quoted.  To pass multiple models using
#' R syntax, pass them to the \code{model} parameter in a list.  To pass multiple
#' models using SAS syntax, pass them to the \code{model} parameter as a vector
#' of strings.
#'
#' @section Interactive Output:
#' By default, \code{proc_glm} results will
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
#' The columns and rows
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
#' results.  There are six standard categories: "NObs",
#' "ClassLevels", "OverallANOVA", "FitStatistics", "ModelANOVA", and
#' "ParameterEstimates".  The "ClassLevels" dataset is only produced when one
#' or more classification variables are specified on the \code{class} parameter.
#' The "ModelANOVA" dataset contains the sums of squares tables, identified by
#' a "TYPE" column.  By default this includes the Type I and Type III tables,
#' matching SAS PROC GLM.
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
#'   \item{\strong{ss1}: Requests the Type I (sequential) sums of squares table.
#'   By default, the Type I and Type III tables are produced, matching SAS PROC
#'   GLM.  Passing one or more of the "ss1", "ss2", or "ss3" keywords restricts
#'   the output to only the requested sums of squares types.}
#'   \item{\strong{ss2}: Requests the Type II (partial) sums of squares table.}
#'   \item{\strong{ss3}: Requests the Type III (partial) sums of squares table.}
#'   \item{\strong{clparm}: Requests confidence limits for the parameter
#'   estimates be added to the interactive report and the "ParameterEstimates"
#'   dataset.  The confidence level is controlled by the "alpha=" option.}
#'   \item{\strong{p}: Computes predicted and residual values and sends them to
#'   a separate "OutputStatistics" table on the interactive report.}
#'   \item{\strong{solution}: Requests that the parameter estimates ("solution"
#'   of the normal equations) be displayed.  When classification variables are
#'   present, SAS suppresses the parameter estimates unless this keyword is
#'   passed.}
#'   }
#'
#' @section Options:
#' The \code{proc_glm} function recognizes the following options.  Options may
#' be passed as a quoted vector of strings, or an unquoted vector using the
#' \code{v()} function.
#' \itemize{
#' \item{\strong{alpha = }: The "alpha = " option will set the alpha
#' value for confidence limit statistics.  Set the alpha as a decimal value
#' between 0 and 1.  For example, you can set a 90% confidence limit as
#' \code{alpha = 0.1}.
#' }
#' \item{\strong{noprint}: Whether to print the interactive report to the
#' viewer.  By default, the report is printed to the viewer. The "noprint"
#' option will inhibit printing.  You may inhibit printing globally by
#' setting the package print option to false:
#' \code{options("procs.print" = FALSE)}.
#' }
#' \item{\strong{outstat}: The "outstat" option is used to request that an
#' output dataset of model sums of squares and associated statistics be
#' returned.  This dataset corresponds to the "OUTSTAT=" dataset in SAS.
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
#' @param data The input data frame for which to perform the analysis of
#' variance. This parameter is required.
#' @param model A model for the analysis to be performed.  The model can be
#' specified using either R syntax or SAS syntax. \code{model = var1 ~ var2 + var3} is
#' an example of R style model syntax. If you wish to pass multiple models using
#' R syntax, pass them in a list.  For SAS syntax, pass the model as a quoted string:
#' \code{model = "var1 = var2 var3"}.  To pass
#' multiple models using SAS syntax, pass them as a vector of strings. By default,
#' the models will be named "MODEL1", "MODEL2", etc.  If you want to name your
#' model, pass it as a named list or named vector.
#' @param class An optional vector of classification (categorical) variable
#' names.  Classification variables are treated as factors and coded into the
#' model design matrix as indicator variables.  Pass a single variable
#' unquoted, or multiple variables as a quoted vector of names.  You may also
#' pass them unquoted using the \code{\link[common]{v}} function.
#' @param by An optional by group. If you specify a by group, the input
#'  data will be subset on the by variable(s) prior to performing the analysis.
#'  For multiple by variables, pass them as a quoted vector of variable names.
#'  You may also pass them unquoted using the \code{\link[common]{v}} function.
#' @param stats Optional statistics keywords.  Valid values are "ss1", "ss2",
#' "ss3", "clparm", "p", and "solution".  A single keyword may be passed with or
#' without quotes. Pass multiple keywords either as a quoted vector, or unquoted
#' vector using the \code{v()} function.  These statistics keywords largely
#' correspond to the options on the "model" statement in SAS.  See the
#' \strong{Statistics Keywords} section for details on the purpose and target of
#' each keyword.
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
#' "noprint", and "outstat".  The "alpha = " option will set the alpha
#' value for confidence limit statistics.  The default is 95% (alpha = 0.05).
#' The "noprint" option turns off the interactive report. For other options,
#' see the \strong{Options} section for explanations of each.
#' @param titles A vector of one or more titles to use for the report output.
#' @param plots Pass the desired plot(s) on this parameter. Default is NULL,
#' meaning no plots are desired.  If there are multiple model requests, you can
#' pass a single plot request which will apply to all models, or a list of plot
#' requests that aligns one-to-one for each model formula.
#' @param where An expression to filter the rows before statistics are calculated.
#' Use the \code{\link[base]{expression}} function to define the filter.
#' @return Normally, the requested analysis of variance statistics are shown
#' interactively in the viewer, and output results are returned as a data frame.
#' If you request "report" datasets, they will be returned as a list.
#' You may then access individual datasets from the list using dollar sign
#' ($) syntax.
#' The interactive report can be turned off using the "noprint" option.
#' The output dataset can be turned off using the "none" keyword on the
#' \code{output} parameter. If the output dataset is turned off, the function
#' will return a NULL.
#' @import fmtr
#' @import tibble
#' @seealso [proc_reg()]
#' @export
proc_glm <- function(data,
                     model,
                     class = NULL,
                     by = NULL,
                     stats = NULL,
                     output = NULL,
                     weight = NULL,
                     options = NULL,
                     titles = NULL,
                     plots = NULL,
                     where = NULL) {

  # Deal with single value unquoted parameter values
  weight <- resolve_arg(weight)
  class <- resolve_arg(class)
  by <- resolve_arg(by)
  stats <- resolve_arg(stats)
  options <- resolve_arg(options, type = c("integer", "double", "character", "NULL"))
  output <- resolve_arg(output)

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

  if (!is.null(class)) {
    if (!all(class %in% nms)) {
      stop(paste("Invalid class name: ", class[!class %in% nms], "\n"))
    }
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

  # Parameter checks for options
  if (!is.null(options)) {

    kopts <- c("alpha", "noprint", "outstat")

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

    sopts <- c("ss1", "ss2", "ss3", "clparm", "p", "solution")

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
  rptres <- NULL

  # Kill output request for report
  if (has_report(output)) {
    rptflg <- TRUE
  }

  if (has_view(options))
    view <- TRUE
  else
    view <- FALSE

  res <- NULL

  # Where subset
  if (!is.null(where)) {
    data <- subset_data(data, where)
  }

  # Get report if requested
  if (view == TRUE | rptflg) {
    rptres <- gen_report_glm(data,
                             model = model,
                             class = class,
                             by = by, stats = stats,
                             view = view,
                             titles = titles,
                             opts = options,
                             output = output,
                             weight = weight,
                             plots = plots)
  }

  # Get output datasets if requested
  if (has_output(output)) {
    res <- gen_output_glm(data,
                          model = model,
                          class = class,
                          by = by,
                          stats = stats,
                          output = output,
                          opts = options,
                          weight = weight)
  }

  # Add report to result if requested
  if (rptflg & !is.null(rptres)) {

    if (is.null(res))
      res <- rptres
    else {
      res <- list(out = res, report = rptres)
    }
  }

  # Log the glm function
  log_glm(data,
          model = model,
          class = class,
          by = by,
          stats = stats,
          output = output,
          weight = weight,
          view = view,
          titles = titles,
          options = options,
          plots = plots,
          where = where,
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


log_glm <- function(data,
                    model = NULL,
                    class = NULL,
                    by = NULL,
                    stats = NULL,
                    output = NULL,
                    weight = NULL,
                    view = TRUE,
                    titles = NULL,
                    options = NULL,
                    plots = NULL,
                    where = NULL,
                    outcnt = NULL) {

  ret <- c()

  indt <- paste0(rep(" ", 10), collapse = "")

  ret <- paste0("proc_glm: input data set ", nrow(data),
                " rows and ", ncol(data), " columns")

  if (!is.null(model))
    ret[length(ret) + 1] <- paste0(indt, "model: ",
                                   paste(model, collapse = " "))

  if (!is.null(class))
    ret[length(ret) + 1] <- paste0(indt, "class: ", paste(class, collapse = " "))

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

  if (!is.null(options))
    ret[length(ret) + 1]<- paste0(indt, "options: ", paste(options, collapse = " "))

  if (!is.null(where))
    ret[length(ret) + 1]<- paste0(indt, "where: ", as.character(where))

  if (!is.null(titles))
    ret[length(ret) + 1] <- paste0(indt, "titles: ", paste(titles, collapse = "\n"))

  if (!is.null(outcnt))
    ret[length(ret) + 1] <- paste0(indt, "output: ", outcnt, " datasets")

  log_logr(ret)

}


# Utilities -----------------------------------------------------------------

glm_lbls <- c(DF = "DF", SUMSQ = "Sum of Squares", MEANSQ = "Mean Square",
              FVAL = "F Value", PROBF = "Pr > F", RMSE = "Root MSE",
              DEPMEAN = "Dependent Mean", COEFVAR = "Coeff Var",
              RSQ = "R-Square", ADJRSQ = "Adj R-Sq", LEVELS = "Levels",
              VALUES = "Values", SOURCE = "Source", CLASS = "Class",
              TYPE = "Type")

get_output_specs_glm <- function(model, opts, output, report = FALSE) {

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

    nms <- names(mlist)
    mnum <- 1

    for (mdl in mlist) {

      if (length(nms) > 0 && !is.na(nms[mnum]) && nms[mnum] != "")
        vlbl <- nms[mnum]
      else
        vlbl <- paste0("MODEL", mnum)

      # Get dependent variable
      vr <- as.character(mdl)
      if (length(vr) > 2)
        vr <- vr[2]

      ret[[vlbl]] <- out_spec(var = vr, formula = mdl, report = report)

      mnum <- mnum + 1
    }
  }

  return(ret)
}


# Get p-value format used across glm tables
get_glm_pfmt <- function() {

  eval(str2lang('value(condition(is.na(x), "NA"),
                condition(x < .0001, "<.0001"),
                condition(TRUE, "%.4f"),
                log = FALSE)'))
}


# Lookup to translate sasLM column names to procs names
glm_lkp <- c("Df" = "DF", "Sum.Sq" = "SUMSQ", "Mean.Sq" = "MEANSQ",
             "F.value" = "FVAL", "Pr..F." = "PROBF", "Root.MSE" = "RMSE",
             "Coef.Var" = "COEFVAR", "R.square" = "RSQ", "Adj.R.sq" = "ADJRSQ")


# Determine which sums of squares types to produce.  SAS PROC GLM shows
# Type I and Type III by default.  Passing any of the "ss1", "ss2", or
# "ss3" keywords restricts the output to only those requested.
get_glm_ss_types <- function(stats = NULL) {

  reqs <- intersect(c("ss1", "ss2", "ss3"), tolower(stats))

  if (length(reqs) == 0)
    ret <- c("Type I", "Type III")
  else
    ret <- c(ss1 = "Type I", ss2 = "Type II", ss3 = "Type III")[reqs]

  names(ret) <- NULL

  return(ret)
}


#' @import sasLM
#' @import common
#' @import fmtr
get_glm_report <- function(data, var, model, class = NULL, opts = NULL,
                           weight = NULL, stats = NULL) {

  ret <- list()

  # Convert class variables to factors
  if (!is.null(class)) {
    for (cv in class)
      data[[cv]] <- as.factor(data[[cv]])
  }

  # Run the model
  glmod <- GLM(model, data)

  pfmt <- get_glm_pfmt()

  glm_fc <- fcat(DF = "%d", SUMSQ = "%.6f", MEANSQ = "%.6f", FVAL = "%.2f",
                 PROBF = pfmt, RMSE = "%.6f", DEPMEAN = "%.5f",
                 COEFVAR = "%.6f", RSQ = "%.6f", ADJRSQ = "%.6f",
                 LEVELS = "%d", log = FALSE)

  lkp <- glm_lkp
  lkp[paste0(make.names(var), ".Mean")] <- "DEPMEAN"

  # ClassLevels (shown before NObs in SAS PROC GLM)
  if (!is.null(class)) {
    clv <- c()
    cvl <- c()
    for (cv in class) {
      lv <- levels(data[[cv]])
      clv[length(clv) + 1] <- length(lv)
      cvl[length(cvl) + 1] <- paste(lv, collapse = " ")
    }

    cl <- data.frame(stub = class, LEVELS = clv, VALUES = cvl,
                     stringsAsFactors = FALSE)
    formats(cl) <- glm_fc
    labels(cl) <- c(glm_lbls, stub = "Class")
    ret[["ClassLevels"]] <- cl
  }

  # NObs
  ret[["NObs"]] <- get_obs(data, model)

  # OverallANOVA
  aov <- as.data.frame(unclass(glmod$ANOVA), stringsAsFactors = FALSE)
  aov <- data.frame(stub = c("Model", "Error", "Corrected Total"),
                    aov, stringsAsFactors = FALSE)
  rownames(aov) <- NULL
  names(aov) <- fapply(names(aov), lkp)
  formats(aov) <- glm_fc
  labels(aov) <- c(glm_lbls, stub = "Source")
  ret[["OverallANOVA"]] <- aov

  # FitStatistics
  fit <- as.data.frame(unclass(glmod$Fitness), stringsAsFactors = FALSE)
  rownames(fit) <- NULL
  names(fit) <- fapply(names(fit), lkp)
  fit <- fit[ , c("RSQ", "COEFVAR", "RMSE", "DEPMEAN")]
  formats(fit) <- glm_fc
  labels(fit) <- glm_lbls
  ret[["FitStatistics"]] <- fit

  # ModelANOVA (separate table per SS type, matching SAS PROC GLM)
  tps <- get_glm_ss_types(stats)
  tpcd <- c("Type I" = "1", "Type II" = "2", "Type III" = "3")
  for (tp in tps) {
    tmp <- as.data.frame(unclass(glmod[[tp]]), stringsAsFactors = FALSE)
    tmp <- data.frame(stub = rownames(glmod[[tp]]), tmp,
                      stringsAsFactors = FALSE)
    rownames(tmp) <- NULL
    names(tmp) <- fapply(names(tmp), lkp)
    formats(tmp) <- glm_fc

    # Label the sums of squares column with the SS type, e.g. "Type I SS"
    tlbls <- c(glm_lbls, stub = "Source")
    tlbls[["SUMSQ"]] <- paste0(tp, " SS")
    labels(tmp) <- tlbls

    ret[[paste0("ModelANOVA", tpcd[[tp]])]] <- tmp
  }

  return(ret)
}


#' @import sasLM
#' @import common
#' @import fmtr
get_glm_output <- function(data, var, model, modelname, class = NULL,
                           opts = NULL, stats = NULL, byvars = NULL,
                           weight = NULL) {

  # Convert class variables to factors
  if (!is.null(class)) {
    for (cv in class)
      data[[cv]] <- as.factor(data[[cv]])
  }

  # Run the model
  glmod <- GLM(model, data)

  # Build stacked SS table (Type I and Type III by default)
  tpcd <- c("Type I" = "1", "Type II" = "2", "Type III" = "3")
  tps <- get_glm_ss_types(stats)
  manova <- NULL
  for (tp in tps) {
    tmp <- as.data.frame(unclass(glmod[[tp]]), stringsAsFactors = FALSE)
    tmp <- data.frame(SOURCE = rownames(glmod[[tp]]), TYPE = tpcd[[tp]],
                      tmp, stringsAsFactors = FALSE)
    rownames(tmp) <- NULL

    if (is.null(manova))
      manova <- tmp
    else
      manova <- rbind(manova, tmp)
  }
  names(manova) <- fapply(names(manova), glm_lkp)

  ret <- data.frame(MODEL = modelname, DEPVAR = var, manova,
                    stringsAsFactors = FALSE)

  # Assign byvars
  if (!is.null(byvars)) {
    for (bnm in names(byvars)) {
      ret[[bnm]] <- as.character(byvars[bnm])
    }
    ret <- ret[ , c(names(byvars), setdiff(names(ret), names(byvars)))]
  }

  rownames(ret) <- NULL

  return(ret)
}


# Specifically for shaping results of get_glm_output()
shape_glm_data <- function(ds, shape) {

  ret <- ds

  if (!is.null(shape)) {

    bv <- c("MODEL", "DEPVAR", "SOURCE", "TYPE")
    bnms <- find.names(ds, "BY*")
    if (!is.null(bnms))
      bv <- c(bnms, bv)

    if (all(shape == "long")) {

      ret <- proc_transpose(ds, by = bv, name = "STAT", log = FALSE)

    } else if (all(shape == "stacked")) {

      ret <- proc_transpose(ds, by = bv, name = "STAT", log = FALSE)

      rnms <- names(ret)
      rnms[rnms %in% "COL1"] <- "VALUES"
      names(ret) <- rnms
    }
  }

  return(ret)
}


# Drivers -----------------------------------------------------------------

#' @import common
gen_report_glm <- function(data,
                           model = NULL,
                           class = NULL,
                           by = NULL,
                           stats = NULL,
                           opts = NULL,
                           output = NULL,
                           view = TRUE,
                           titles = NULL,
                           weight = NULL,
                           plots = NULL) {

  spcs <- get_output_specs_glm(model, opts, output, report = TRUE)

  nms <- names(spcs)

  byres <- list()

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

  mnum <- 0

  # Loop through models
  for (nm in nms) {

    outp <- spcs[[nm]]
    vnm <- outp$var
    mnum <- mnum + 1

    # Loop through by groups
    for (j in seq_len(length(dtlst))) {

      dt <- dtlst[[j]]
      bynm <- nm
      if (length(bylbls) > 0)
        bynm <- paste0(nm, ":", bylbls[j])

      byres[[bynm]] <- get_glm_report(dt, vnm, outp$formula, class = class,
                                      opts = opts, weight = weight,
                                      stats = stats)

      # Assign titles
      ttls <- c()
      ttls[1] <- paste0("Model: ", nm)
      ttls[2] <- paste0("Dependent Variable: ", vnm)
      if (length(bylbls) > 0)
        ttls[3] <- bylbls[j]

      attr(byres[[bynm]][[1]], "ttls") <- ttls
    }
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
        titles <- "The GLM Function"

      out <- output_report(ret, dir_name = dirname(vrfl),
                           file_name = basename(vrfl), out_type = "HTML",
                           titles = titles, margins = .5, viewer = TRUE,
                           pages = length(byres))

      show_viewer(out)
    }
  }

  if (has_option(output, "report")) {

    nmsret <- names(ret)

    if ("NObs" %in% nmsret)
      names(ret$NObs) <- sub("stub", "LABEL", names(ret$NObs), fixed = TRUE)

    if ("ClassLevels" %in% nmsret)
      names(ret$ClassLevels) <- sub("stub", "CLASS", names(ret$ClassLevels), fixed = TRUE)

    if ("OverallANOVA" %in% nmsret)
      names(ret$OverallANOVA) <- sub("stub", "SOURCE", names(ret$OverallANOVA), fixed = TRUE)

    for (mn in grep("^ModelANOVA", nmsret, value = TRUE))
      names(ret[[mn]]) <- sub("stub", "SOURCE", names(ret[[mn]]), fixed = TRUE)
  }

  return(ret)
}


#' @import fmtr
#' @import common
gen_output_glm <- function(data,
                           model = NULL,
                           class = NULL,
                           by = NULL,
                           stats = NULL,
                           output = NULL,
                           opts = NULL,
                           weight = NULL) {

  spcs <- get_output_specs_glm(model, opts, output, report = FALSE)

  res <- list()

  if (length(spcs) > 0) {

    bdat <- list(data)
    if (!is.null(by)) {
      bdat <- split(data, data[ , by, drop = FALSE], sep = "|", drop = TRUE)
    }
    bynms <- names(bdat)

    # Make up by variable names for output ds
    byn <- NULL
    if (!is.null(by)) {
      if (length(by) == 1)
        byn <- "BY"
      else
        byn <- paste0("BY", seq(1, length(by)))
    }

    tmpres <- NULL

    nms <- names(spcs)
    for (j in seq_len(length(bdat))) {

      dat <- bdat[[j]]

      bynm <- NULL
      if (!is.null(by)) {
        bynm <- strsplit(bynms[j], "|", fixed = TRUE)[[1]]
        names(bynm) <- byn
      }

      for (i in seq_len(length(spcs))) {

        outp <- spcs[[i]]
        nm <- nms[i]

        tmpby <- get_glm_output(dat, var = outp$var,
                                model = outp$formula, modelname = nm,
                                class = class, opts = opts, stats = stats,
                                byvars = bynm, weight = weight)

        if (is.null(tmpres))
          tmpres <- tmpby
        else
          tmpres <- perform_set(tmpres, tmpby)
      }
    }

    rownames(tmpres) <- NULL

    res[["ModelANOVA"]] <- tmpres
  }

  if (length(res) == 1) {
    res <- res[[1]]

    if (has_option(output, "long"))
      res <- shape_glm_data(res, "long")

    if (has_option(output, "stacked"))
      res <- shape_glm_data(res, "stacked")
  }

  return(res)
}



# @import sasLM
# get_lm_stats <- function() {
#
#
#
#
# }

# library(sasLM)
#
# BEdata = af(BEdata, c("SEQ", "SUBJ", "PRD", "TRT")) # Columns as factor
# formula1 = log(CMAX) ~ SEQ/SUBJ + PRD + TRT # Model
# GLM(formula1, BEdata) # ANOVA tables of Type I, II, III SS
# EMS(formula1, BEdata) # EMS table
# T3test(formula1, BEdata, Error="SEQ:SUBJ") # Hypothesis test
# ci0 = CIest(formula1, BEdata, "TRT", c(-1, 1), 0.90) # 90$ CI
# exp(ci0[, c("Estimate", "Lower CL", "Upper CL")]) # 90% CI of GMR






# General Linear Model ----------------------------------------------------



#' @title Calculates a General Linear Model
#' @encoding UTF-8
#' @description The \code{proc_glm} function performs a general linear model
#' analysis for one or more models.  Unlike \code{\link{proc_reg}}, the
#' \code{proc_glm} function accepts categorical predictors through the
#' \code{class} parameter, and produces Type I, Type II, and Type III sums
#' of squares.  The model(s) are passed on the \code{model} parameter, and the
#' input dataset is passed on the \code{data} parameter.  The \code{by}
#' parameter allows you to subset the data into groups and run the model on
#' each group.  The \code{weight} parameter lets you assign a weight to each
#' observation.  The \code{output} and \code{options} parameters provide
#' additional customization of the results.
#' @details
#' The \code{proc_glm} function is a general-purpose linear modeling function
#' built on top of the \code{\link[sasLM]{GLM}} function from the \strong{sasLM}
#' package.  It produces a dataset output by default, and, when working in
#' RStudio, also produces an interactive report.  Statistical output is
#' designed to match SAS.
#' @param data The input data frame for which to perform the analysis.
#' This parameter is required.
#' @param model A model for the analysis.  The model can be specified using
#' either R syntax (\code{y ~ a + b + a:b}) or SAS syntax (\code{"y = a b a*b"}).
#' To pass multiple models, use a list (R syntax) or a vector of strings
#' (SAS syntax).  By default, models are named "MODEL1", "MODEL2", etc.
#' @param class An optional vector of variable names to treat as categorical
#' (factor) predictors.  These variables will be converted to factors prior to
#' fitting the model.  Pass quoted, or unquoted using the
#' \code{\link[common]{v}} function.
#' @param order Controls the sort order of the class variable levels.  Valid
#' values are "internal", "formatted", "data", and "freq", matching the
#' \code{order} parameter on \code{\link{proc_freq}} and \code{\link{proc_ttest}}.
#' The default "internal" uses the natural order of the values: character values
#' sort alphabetically, numeric values sort by value, and an existing factor
#' keeps its level order.  "formatted" applies the variable's format and uses
#' the order defined by the format.  "data" uses order of appearance, and "freq"
#' sorts by descending frequency.  Pass one keyword per \code{class} variable, or
#' a single keyword to apply to all of them.
#'
#' Note that for \code{proc_glm} the level order is not merely cosmetic.  The
#' last level of a class variable is the reference level, so \code{order}
#' changes the Type III and "solution" results.  It also determines how
#' \code{contrast} and \code{estimate} coefficients are mapped onto levels: the
#' coefficients are read in level order, so the same coefficient vector means
#' different things under different \code{order} values.
#'
#' The "formatted" keyword goes further still.  If the format maps several
#' values onto one label, those values become a single level, and the model
#' loses the degrees of freedom that separated them.  This matches the SAS
#' practice of using a format to group a class variable, but it means
#' \code{order = "formatted"} can change the model and not just its
#' presentation.  Only a user-defined format created with
#' \code{\link[fmtr]{value}} carries a level order, so that is the only kind of
#' format "formatted" accepts.
#' @param by An optional by group.  If specified, the input data will be subset
#' on the by variable(s) prior to performing the analysis.
#' @param stats Optional statistics keywords, corresponding to the options on
#' the "model" statement in SAS.  Valid values are "ss1", "ss2", "ss3",
#' "solution", "est", "clparm", "p", "clm", "cli", "noint", "xpx", "inverse",
#' "e1", "e2", and "e3".
#' \itemize{
#' \item{\strong{solution}: Adds a parameter estimates table to the interactive
#' report.  "est" is an accepted alias.}
#' \item{\strong{clparm}: Adds confidence limits for the parameter estimates,
#' using the alpha value from the \code{options} parameter.}
#' \item{\strong{p}: Adds predicted values and residuals to the interactive
#' report.}
#' \item{\strong{clm}: Adds the standard error and confidence limits of the
#' mean predicted value to the predicted values table.  Implies "p".}
#' \item{\strong{cli}: Adds confidence limits for an individual predicted value
#' to the predicted values table.  These are wider than the "clm" limits
#' because they include the error variance.  Implies "p".}
#' \item{\strong{noint}: Fits the model without an intercept.  Every downstream
#' statistic reflects this, and the total sum of squares is no longer corrected
#' for the mean.}
#' \item{\strong{xpx}: Adds the augmented X'X crossproducts matrix, which
#' carries the dependent variable as an extra row and column.}
#' \item{\strong{inverse}: Adds the generalized inverse of the augmented X'X
#' matrix.  Its bottom right cell is the error sum of squares.}
#' \item{\strong{e1}, \strong{e2}, \strong{e3}: Add the Type I, Type II and
#' Type III estimable function tables.}
#' \item{\strong{ss1}, \strong{ss2}, \strong{ss3}: Select which sums of squares
#' to produce.  The default is Type I and Type III, as in SAS.}
#' }
#' @param output Whether or not to return datasets from the function.  Valid
#' values are "out", "none", and "report", plus the data shaping keywords
#' "long", "stacked", and "wide".  Default is "out".
#' @param weight The name of a variable to use as a weight for each observation.
#' @param lsmeans The name of one or more class variables for which to compute
#' least-squares means.  Each requested effect produces a least-squares means
#' table on the interactive report.  The effect(s) must also appear on the
#' \code{class} parameter.
#' @param contrast A named list of contrast specifications.  Each element name
#' is the contrast label, and each value is itself a named list mapping a model
#' effect to its coefficients over that effect's levels, in level order.  For
#' example, \code{contrast = list("F vs M" = list(Sex = c(1, -1)))} mirrors the
#' SAS statement \code{contrast 'F vs M' Sex 1 -1}.  Each contrast produces an
#' F-test row on the interactive report.
#'
#' Give an effect a matrix instead of a vector to reproduce the comma syntax of
#' a SAS \code{CONTRAST} statement: each row of the matrix is one row of the
#' contrast, and the result is a single test whose degrees of freedom equal the
#' number of linearly independent rows.  So
#' \code{list("Any diff" = list(AgeGroup = rbind(c(-1, 0, 1), c(0, -1, 1))))}
#' is the SAS statement
#' \code{contrast 'Any diff' AgeGroup -1 0 1, AgeGroup 0 -1 1;}.
#'
#' Interactions may be spelled either way: the SAS operator \code{"a*b"} is
#' accepted and normalized to the R operator \code{"a:b"}.  Any effect not named
#' gets a coefficient of zero, as does the intercept.  A contrast that is not
#' estimable is dropped with a warning rather than reported.
#' @param estimate A named list of estimate specifications, using the same
#' structure as the \code{contrast} parameter.  Each estimate produces a row on
#' the interactive report with the estimate, standard error, t value, p value,
#' and confidence limits.  Unlike \code{contrast}, \code{estimate} accepts only
#' single-row specifications, since it estimates one linear combination.
#' @param random The name of one or more class variables to treat as random
#' effects.  Produces a table of Type III expected mean squares on the
#' interactive report.  The effect(s) must also appear on the \code{class}
#' parameter.
#' @param options A vector of optional keywords.  Valid values are "alpha =",
#' "noprint", "ss1", "ss2", "ss3", "outstat", "singular =", and "zeta =".  The
#' "outstat" option requests an output dataset of the model sums of squares.
#' This is the default output dataset, so the option does not normally need to
#' be passed.  The "singular =" option sets the tolerance for detecting a linear
#' dependency among the design matrix columns, and "zeta =" sets the tolerance
#' of the estimability check applied to each \code{contrast} and
#' \code{estimate}.  Both default to 1e-8.
#' @param titles A vector of one or more titles to use for the report output.
#' @param where An expression to filter the rows before statistics are
#' calculated.  Use the \code{\link[base]{expression}} function.
#' @return Normally the requested statistics are shown interactively in the
#' viewer, and output results are returned as a data frame.  If "report"
#' datasets are requested they are returned as a list.
#' @import fmtr
#' @import tibble
#' @seealso [proc_reg()]
#' @export
proc_glm <- function(data,
                     model,
                     class = NULL,
                     order = NULL,
                     by = NULL,
                     stats = NULL,
                     output = NULL,
                     weight = NULL,
                     lsmeans = NULL,
                     contrast = NULL,
                     estimate = NULL,
                     random = NULL,
                     options = NULL,
                     titles = NULL,
                     where = NULL
) {

  # Deal with single value unquoted parameter values
  weight <- resolve_arg(weight)
  class <- resolve_arg(class)
  order <- resolve_arg(order)
  by <- resolve_arg(by)
  stats <- resolve_arg(stats)
  lsmeans <- resolve_arg(lsmeans)
  random <- resolve_arg(random)
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

  if (!is.null(lsmeans)) {
    if (!all(lsmeans %in% class)) {
      stop(paste("The lsmeans effect must be a class variable: ",
                 lsmeans[!lsmeans %in% class], "\n"))
    }
  }

  if (!is.null(random)) {
    if (!all(random %in% class)) {
      stop(paste("The random effect must be a class variable: ",
                 random[!random %in% class], "\n"))
    }
  }

  # Validate the shape of the coefficient specs, and reject a multi-row spec
  # on estimate, where it has no meaning.
  check_spec_list(contrast, "contrast")
  check_spec_list(estimate, "estimate")

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

    kopts <- c("alpha", "noprint", "ss1", "ss2", "ss3", "outstat",
               "singular", "zeta")

    # Deal with "alpha =" by using name instead of value
    nopts <- names(options)
    if (is.null(nopts) & length(options) > 0)
      nopts <- options
    mopts <- ifelse(nopts == "", options, nopts)

    if (!all(tolower(mopts) %in% kopts)) {
      stop(paste0("Invalid options keyword: ", mopts[!tolower(mopts) %in% kopts], "\n"))
    }
  }

  # Parameter checks for stats
  if (!is.null(stats)) {

    sopts <- c("ss1", "ss2", "ss3", "solution", "clparm", "p",
               "est", "clb", "alpha", "noint", "xpx", "inverse", "e1", "e2", "e3",
               "clm", "cli")

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
      stop("Variable specified for weight parameter not found in data.")
  }

  # Default order parameter
  if (is.null(order)) {
    order <- "internal"
  } else {
    order <- tolower(order)

    if (!all(order %in% c("data", "internal", "formatted", "freq"))) {
      bad <- order[!order %in% c("data", "internal", "formatted", "freq")]
      stop(paste0("Invalid value for 'order' parameter: ",
                  paste0("'", bad, "'", collapse = ", "), "\n",
                  "Valid values are: 'internal', 'formatted', 'data', and 'freq'"))
    }
  }

  rptflg <- FALSE
  rptres <- NULL

  # Kill output request for report so it doesn't confuse gen_output_glm
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

  # Deal with order.  This parallels proc_ttest() keyword for keyword, with one
  # necessary difference: proc_ttest() leaves "internal" alone because it does
  # its own grouping, but sasLM::GLM() would read an unfactored numeric class
  # variable as a continuous predictor and silently fit the wrong model (one
  # degree of freedom instead of one per level), so every class variable has to
  # end up a factor here.
  if (!is.null(order) & !is.null(class)) {

    if (length(order) != length(class)) {
      if (length(order) == 1) {

        order <- rep(order, length(class))
      } else {
        stop("Order keywords are misaligned with the number of class variables.")
      }
    }

    for (idx in seq_len(length(class))) {

      cl <- class[idx]
      odr <- order[idx]

      if (odr == "data") {

        data[[cl]] <- factor(data[[cl]], unique(data[[cl]]))

      } else if (odr == "formatted") {

        fmt <- attr(data[[cl]], "format")

        if (!is.null(fmt)) {
          attr(fmt, "as.factor") <- TRUE
          data[[cl]] <- fapply(data[[cl]], fmt)
        }

      } else if (odr == "freq") {

        ftbl <- table(data[[cl]])
        ftbl <- sort(ftbl, decreasing = TRUE)
        data[[cl]] <- factor(data[[cl]], names(ftbl))

      } else {
        # Internal is the default.  The factor conversion below handles it.
      }

      if (!is.factor(data[[cl]]))
        data[[cl]] <- factor(data[[cl]],
                             levels = as.character(sort(unique(data[[cl]]))))

      # Drop levels left empty by a where or NA subset so the degrees of
      # freedom match SAS.
      data[[cl]] <- droplevels(data[[cl]])
    }
  }

  # Get report if requested
  if (view == TRUE | rptflg) {
    rptres <- gen_report_glm(data,
                             model = model,
                             class = class,
                             by = by,
                             stats = stats,
                             view = view,
                             titles = titles,
                             opts = options,
                             output = output,
                             weight = weight,
                             lsmeans = lsmeans,
                             contrast = contrast,
                             estimate = estimate,
                             random = random)
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
    else
      res <- list(out = res, report = rptres)
  }

  # Log the glm function
  log_glm(data,
          model = model,
          class = class,
          order = order,
          by = by,
          stats = stats,
          output = output,
          weight = weight,
          lsmeans = lsmeans,
          random = random,
          contrast = contrast,
          estimate = estimate,
          view = view,
          titles = titles,
          options = options,
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
                    order = NULL,
                    by = NULL,
                    stats = NULL,
                    output = NULL,
                    weight = NULL,
                    lsmeans = NULL,
                    random = NULL,
                    contrast = NULL,
                    estimate = NULL,
                    view = TRUE,
                    titles = NULL,
                    options = NULL,
                    where = NULL,
                    outcnt = NULL) {

  ret <- c()
  indt <- paste0(rep(" ", 10), collapse = "")

  ret <- paste0("proc_glm: input data set ", nrow(data),
                " rows and ", ncol(data), " columns")

  if (!is.null(model))
    ret[length(ret) + 1] <- paste0(indt, "model: ", paste(model, collapse = " "))

  if (!is.null(class))
    ret[length(ret) + 1] <- paste0(indt, "class: ", paste(class, collapse = " "))

  if (!is.null(order))
    ret[length(ret) + 1] <- paste0(indt, "order: ", paste(order, collapse = " "))

  if (!is.null(by))
    ret[length(ret) + 1] <- paste0(indt, "by: ", paste(by, collapse = " "))

  if (!is.null(stats))
    ret[length(ret) + 1] <- paste0(indt, "stats: ", paste(stats, collapse = " "))

  if (!is.null(output))
    ret[length(ret) + 1] <- paste0(indt, "output: ", paste(output, collapse = " "))

  if (!is.null(weight))
    ret[length(ret) + 1] <- paste0(indt, "weight: ", paste(weight, collapse = " "))

  if (!is.null(lsmeans))
    ret[length(ret) + 1] <- paste0(indt, "lsmeans: ", paste(lsmeans, collapse = " "))

  if (!is.null(random))
    ret[length(ret) + 1] <- paste0(indt, "random: ", paste(random, collapse = " "))

  if (!is.null(contrast))
    ret[length(ret) + 1] <- paste0(indt, "contrast: ",
                                   paste(names(contrast), collapse = " "))

  if (!is.null(estimate))
    ret[length(ret) + 1] <- paste0(indt, "estimate: ",
                                   paste(names(estimate), collapse = " "))

  if (!is.null(view))
    ret[length(ret) + 1] <- paste0(indt, "view: ", paste(view, collapse = " "))

  if (!is.null(options))
    ret[length(ret) + 1] <- paste0(indt, "options: ", paste(options, collapse = " "))

  if (!is.null(where))
    ret[length(ret) + 1] <- paste0(indt, "where: ", as.character(where))

  if (!is.null(titles))
    ret[length(ret) + 1] <- paste0(indt, "titles: ", paste(titles, collapse = "\n"))

  if (!is.null(outcnt))
    ret[length(ret) + 1] <- paste0(indt, "output: ", outcnt, " datasets")

  log_logr(ret)
}


# Utilities -----------------------------------------------------------------

# Parse the model parameter into a list of out_spec objects, one per model.
# Mirrors get_output_specs_reg(), but also records the class variables on the
# spec so the downstream report/output functions know which terms are
# categorical.
get_output_specs_glm <- function(model, class = NULL, opts = NULL,
                                  output = NULL, report = FALSE,
                                  stats = NULL) {
  ret <- list()
  mlist <- list()

  if (length(model) > 0) {
    if ("formula" %in% class(model)) {
      mlist <- list(model)
    } else if ("character" %in% class(model)) {
      mlist <- get_formulas(model)
    } else if ("list" %in% class(model)) {
      if ("formula" %in% class(model[[1]]))
        mlist <- model
      else
        stop("Model list must contain a formula.")
    }
  }

  if (length(mlist) > 0) {

    nms <- names(mlist)
    mnum <- 1

    for (mdl in mlist) {

      # Concat model number when not named
      if (length(nms) > 0 && !is.na(nms[mnum]) && nms[mnum] != "")
        vlbl <- nms[mnum]
      else
        vlbl <- paste0("MODEL", mnum)

      # The noint keyword drops the intercept from the model itself, so every
      # downstream statistic reflects it.  SAS model syntax has no way to spell
      # "- 1", which is why this is a keyword rather than part of the formula.
      if (has_option(stats, "noint"))
        mdl <- stats::update(mdl, . ~ . - 1)

      # Get dependent variable
      vr <- as.character(mdl)
      if (length(vr) > 2)
        vr <- vr[2]

      ret[[vlbl]] <- out_spec(var = vr, formula = mdl, class = class,
                              report = report)

      mnum <- mnum + 1
    }
  }

  return(ret)
}


# Build the L coefficient matrix for one CONTRAST or ESTIMATE.  The spec is a
# named list mapping a model effect to its coefficients, in level order (SAS
# style: "contrast 'F vs M' Sex 1 -1").  Each row of the returned matrix is one
# row of the SAS CONTRAST statement, aligned to the columns of the model design
# matrix as required by sasLM's CONTR() and ESTM().  Coefficients for the
# intercept and for any effect the spec does not mention default to zero.
#
# SAS spells interactions "a*b" where R spells them "a:b", and the model
# parameter already accepts both, so the spec does too.
#' @import sasLM
build_glm_L <- function(formula, data, spec, label = "") {

  mm <- ModelMatrix(formula, data)
  cn <- colnames(mm$X)
  asg <- mm$assign
  tl <- attr(stats::terms(formula), "term.labels")

  nms <- gsub("*", ":", names(spec), fixed = TRUE)

  # Normalize every coefficient set to a matrix so that a single row and the
  # comma syntax of a multi-row SAS CONTRAST take the same path below.
  cfs <- list()

  for (idx in seq_along(spec)) {

    cf <- spec[[idx]]

    if (!is.numeric(cf))
      stop(paste0("Coefficients for effect '", nms[idx], "' on '", label,
                  "' must be numeric."))

    if (!is.matrix(cf))
      cf <- matrix(cf, nrow = 1)

    cfs[[nms[idx]]] <- cf
  }

  rws <- unique(vapply(cfs, nrow, integer(1)))

  if (length(rws) > 1)
    stop(paste0("All effects on '", label, "' must have the same number of ",
                "rows.  Use rbind() to give every effect a coefficient on ",
                "every row."))

  L <- matrix(0, nrow = rws, ncol = length(cn), dimnames = list(NULL, cn))

  for (eff in names(cfs)) {

    idx <- which(tl == eff)

    if (length(idx) == 0)
      stop(paste0("Effect '", eff, "' on '", label,
                  "' not found in the model."))

    cols <- which(asg == idx)

    coefs <- cfs[[eff]]

    if (ncol(coefs) != length(cols))
      stop(paste0("Effect '", eff, "' on '", label, "' expects ", length(cols),
                  " coefficients but got ", ncol(coefs), "."))

    L[, cols] <- coefs
  }

  return(L)
}


# Validate the contrast or estimate parameter: a named list of labels, each
# holding a named list of effect coefficients.  The estimate parameter
# additionally rejects a multi-row spec, because ESTM() estimates a single
# linear combination and a multi-row L has no meaning for it.
#' @noRd
check_spec_list <- function(specs, parm) {

  if (is.null(specs))
    return(invisible(NULL))

  msg <- paste0("The ", parm, " parameter must be a named list of coefficient ",
                "specs.  For example: ", parm,
                " = list(\"F vs M\" = list(Sex = c(1, -1)))")

  if (!is.list(specs) || length(specs) == 0 || is.null(names(specs)) ||
      any(names(specs) == ""))
    stop(msg)

  for (lbl in names(specs)) {

    sp <- specs[[lbl]]

    if (!is.list(sp) || length(sp) == 0 || is.null(names(sp)) ||
        any(names(sp) == ""))
      stop(msg)

    if (parm == "estimate") {
      for (cf in sp) {
        if (is.matrix(cf) && nrow(cf) > 1)
          stop(paste0("The estimate parameter accepts only single-row ",
                      "specifications.  Multiple rows define a multiple degree ",
                      "of freedom F test, which is meaningful only for ",
                      "contrast.  See label '", lbl, "'."))
      }
    }
  }

  return(invisible(NULL))
}


# Check that every row of an L matrix is estimable, the way SAS does before it
# will test a CONTRAST or compute an ESTIMATE.
#
# sasLM's CONTR() and ESTM() do no estimability checking of their own: handed a
# non-estimable L they return a perfectly ordinary-looking F or t statistic that
# means nothing.  SAS instead writes a note to the log and leaves the contrast
# out of the output entirely, which is what this reproduces.  For a multi-row
# contrast SAS rejects the whole contrast if any single row is not estimable.
#
# The tolerance is the SAS ZETA= option.
#' @import sasLM
check_estimable <- function(L, X, g2, label, zeta = 1e-8) {

  # g2 must be a generalized inverse of the unweighted X'X, built with
  # G2SWEEP().  g2inv() is not a substitute: it writes its result into the
  # leading rank-by-rank block, which is only correct when the singular columns
  # come last -- not the case for a model that mixes class effects with
  # continuous predictors.
  est <- estmb(L, X, g2, eps = zeta)

  if (!all(est)) {
    warning(paste0("'", label, "' is not estimable and was not computed."))
    return(FALSE)
  }

  return(TRUE)
}


# Determine which sum-of-squares types to produce for a given stats request.
# SAS PROC GLM prints both Type I and Type III when nothing is specified, so
# that is the default here.  Returns the sasLM component names.
get_ss_types <- function(stats) {

  types <- c()
  if (has_option(stats, "ss1")) types <- c(types, "Type I")
  if (has_option(stats, "ss2")) types <- c(types, "Type II")
  if (has_option(stats, "ss3")) types <- c(types, "Type III")

  if (length(types) == 0)
    types <- c("Type I", "Type III")

  return(types)
}


# Engine: build the GLM report tables for a single model + by-group.
# Parallels get_reg_report().  Runs sasLM::GLM() and reshapes the ANOVA, fit
# statistics, and requested Type I/II/III sum-of-squares components into the
# labeled and formatted tables used by the interactive report.
#' @import sasLM
#' @import common
#' @import fmtr
get_glm_report <- function(data, var, model, class = NULL, opts = NULL,
                           weight = NULL, stats = NULL, lsmeans = NULL,
                           contrast = NULL, estimate = NULL, random = NULL) {

  alph <- 1 - get_alpha(opts)

  hasSol <- has_option(stats, "solution") || has_option(stats, "est")
  hasCL  <- has_option(stats, "clparm") || has_option(stats, "clb")
  hasCLM <- has_option(stats, "clm")
  hasCLI <- has_option(stats, "cli")
  hasXPX <- has_option(stats, "xpx")
  hasINV <- has_option(stats, "inverse")
  hasE1  <- has_option(stats, "e1")
  hasE2  <- has_option(stats, "e2")
  hasE3  <- has_option(stats, "e3")

  # As in SAS, clm and cli imply the output statistics table that p produces.
  hasP   <- has_option(stats, "p") || hasCLM || hasCLI
  hasLS  <- !is.null(lsmeans)
  needBeta <- hasSol || hasCL || is.null(class)

  # The SAS SINGULAR= option: the tolerance for detecting linear dependency.
  sing <- get_singular(opts)

  if (!is.null(weight)) {
    glm <- GLM(model, data, conf.level = alph, Weights = data[[weight]],
               BETA = needBeta, Resid = hasP, EMEAN = hasLS)
  } else {
    glm <- GLM(model, data, conf.level = alph,
               BETA = needBeta, Resid = hasP, EMEAN = hasLS)
  }

  # One fit of the model, reused by the estimability check, the contrasts, the
  # estimates and the clm/cli limits.
  #
  # This is deliberately not sasLM's CONTR() and ESTM() wrappers.  Both of them
  # call REG() internally with no Weights argument, so in a weighted model they
  # silently return the unweighted statistics -- which would not even agree with
  # the weighted "solution" table printed alongside them.  Calling est() and
  # cSS() directly lets the weights through, and reusing one fit also avoids
  # refitting the model once per contrast.
  rx <- NULL
  mmX <- NULL
  g2u <- NULL

  if (hasCLM || hasCLI || !is.null(contrast) || !is.null(estimate)) {

    wts <- if (is.null(weight)) 1 else data[[weight]]

    rx <- REG(model, data, Weights = wts, summarize = FALSE)
    mmX <- ModelMatrix(model, data)$X

    # Estimability is a property of the row space of the design matrix, so it
    # does not depend on the weights.  In a weighted fit rx$g2 is the inverse of
    # X'WX, and checking L against that rejects contrasts that are perfectly
    # estimable, so the check gets its own unweighted inverse.
    g2u <- G2SWEEP(crossprod(mmX), eps = sing)
  }

  # Create p-val format. Built from a string to bypass CMD check notes on "x".
  pfmt <- eval(str2lang('value(condition(is.na(x), "NA"),
                condition(x < .0001, "<.0001"),
                condition(TRUE, "%.4f"),
                log = FALSE)'))

  # DEPMEAN uses 4 decimals to match SAS PROC GLM's dependent mean display
  # (PROC REG uses 5, so the format is intentionally not shared with reg_fc).
  glm_fc <- fcat(DF = "%d", SUMSQ = "%.5f", MEANSQ = "%.5f", FVAL = "%.2f",
                 PROBF = pfmt, RMSE = "%.5f", DEPMEAN = "%.4f", COEFVAR = "%.5f",
                 RSQ = "%.6f", ADJRSQ = "%.6f", log = FALSE)

  lkp <- c("Df" = "DF", "Sum.Sq" = "SUMSQ", "Mean.Sq" = "MEANSQ",
           "F.value" = "FVAL", "Pr..F." = "PROBF")

  glbls <- c(DF = "DF", SUMSQ = "Sum of Squares", MEANSQ = "Mean Square",
             FVAL = "F Value", PROBF = "Pr > F", RMSE = "Root MSE",
             DEPMEAN = paste(var, "Mean"), COEFVAR = "Coeff Var",
             RSQ = "R-Square", ADJRSQ = "Adj R-Sq", stub = "Source")

  ret <- list()

  # Class Level Information (SAS prints this before the observation counts)
  if (!is.null(class)) {
    vdat <- get_valid_obs(data, model)
    lvls <- c()
    vals <- c()
    for (cv in class) {
      fx <- droplevels(factor(vdat[[cv]]))
      lvls[length(lvls) + 1] <- nlevels(fx)
      vals[length(vals) + 1] <- paste(levels(fx), collapse = " ")
    }
    cli <- data.frame(stub = class, LEVELS = lvls, VALUES = vals,
                      stringsAsFactors = FALSE)
    labels(cli) <- list(stub = "Class", LEVELS = "Levels", VALUES = "Values")
    ret[["ClassLevels"]] <- cli
  }

  # NObs
  ret[["NObs"]] <- get_obs(data, model)

  # Overall model ANOVA.  Without an intercept the total is not corrected for
  # the mean, and SAS labels that row "Uncorrected Total".  The sum of squares
  # sasLM returns is already the uncorrected one; only the label changes.
  aov <- as.data.frame(unclass(glm$ANOVA), stringsAsFactors = FALSE)
  ttl <- ifelse(has_option(stats, "noint"), "Uncorrected Total",
                "Corrected Total")
  aov <- data.frame(stub = c("Model", "Error", ttl),
                    aov, stringsAsFactors = FALSE)
  rownames(aov) <- NULL
  names(aov) <- fapply(names(aov), lkp)
  formats(aov) <- glm_fc
  labels(aov) <- glbls
  ret[["ANOVA"]] <- aov

  # Fit statistics (R-Square, Coeff Var, Root MSE, Dependent Mean)
  fit <- as.data.frame(unclass(glm$Fitness), stringsAsFactors = FALSE)
  fitr <- data.frame(RSQ = fit[[4]], COEFVAR = fit[[3]], RMSE = fit[[1]],
                     DEPMEAN = fit[[2]], stringsAsFactors = FALSE)
  formats(fitr) <- glm_fc
  labels(fitr) <- glbls
  ret[["FitStatistics"]] <- fitr

  # Type I / II / III sum-of-squares tables
  sslbl <- c("Type I" = "Type I SS", "Type II" = "Type II SS",
             "Type III" = "Type III SS")

  for (ty in get_ss_types(stats)) {
    ss <- as.data.frame(unclass(glm[[ty]]), stringsAsFactors = FALSE)
    ss <- data.frame(stub = rownames(glm[[ty]]), ss, stringsAsFactors = FALSE)
    rownames(ss) <- NULL
    names(ss) <- fapply(names(ss), lkp)
    formats(ss) <- glm_fc
    gl2 <- glbls
    gl2[["SUMSQ"]] <- sslbl[[ty]]
    labels(ss) <- gl2
    ret[[gsub(" ", "", ty)]] <- ss   # "TypeI", "TypeII", "TypeIII"
  }

  # Parameter estimates (solution) and optional confidence limits (clparm)
  if (needBeta) {

    pe <- as.data.frame(unclass(glm$Parameter), stringsAsFactors = FALSE)
    pe <- data.frame(stub = rownames(glm$Parameter), pe, stringsAsFactors = FALSE)
    rownames(pe) <- NULL

    penm <- c("Estimate" = "EST", "Estimable" = "ESTIMABLE",
              "Std..Error" = "STDERR", "Df" = "DF",
              "t.value" = "T", "Pr...t.." = "PROBT")
    names(pe) <- fapply(names(pe), penm)

    # SAS labels the intercept row "Intercept", not "(Intercept)"
    pe$stub <- sub("(Intercept)", "Intercept", pe$stub, fixed = TRUE)

    # SAS marks non-uniquely-estimable parameters with a "B"; sasLM flags them
    # with Estimable == 0.  The zeroed reference level additionally has a zero
    # standard error and blank t / p / CL.
    if ("ESTIMABLE" %in% names(pe))
      biased <- pe$ESTIMABLE == 0
    else
      biased <- rep(FALSE, nrow(pe))
    zero <- pe$STDERR == 0
    pe$BIASED <- ifelse(biased, "B", "")

    pe$T[zero] <- NA
    pe$PROBT[zero] <- NA

    # Confidence limits for clparm, driven by alpha= (conf.level)
    if (hasCL) {
      av <- get_alpha(opts)
      tcrit <- qt(1 - av / 2, pe$DF)
      pe$LCLM <- ifelse(zero, NA, pe$EST - tcrit * pe$STDERR)
      pe$UCLM <- ifelse(zero, NA, pe$EST + tcrit * pe$STDERR)
    }

    pe$ESTIMABLE <- NULL

    pe_fc <- fcat(EST = "%.8f", STDERR = "%.8f", DF = "%d", "T" = "%.2f",
                  PROBT = pfmt, LCLM = "%.8f", UCLM = "%.8f", log = FALSE)

    pctl <- (1 - get_alpha(opts)) * 100
    pe_lbls <- list(stub = "Parameter", BIASED = "", EST = "Estimate",
                    STDERR = "Standard Error", DF = "DF", "T" = "t Value",
                    PROBT = "Pr > |t|",
                    LCLM = paste0("Lower ", pctl, "% CL"),
                    UCLM = paste0("Upper ", pctl, "% CL"))

    # Order columns: Parameter, Estimate, B-marker, StdErr, t, Pr, [CL]
    cols <- c("stub", "EST", "BIASED", "STDERR", "T", "PROBT")
    if (hasCL)
      cols <- c(cols, "LCLM", "UCLM")
    pe <- pe[, cols]

    formats(pe) <- pe_fc
    labels(pe) <- pe_lbls

    ret[["ParameterEstimates"]] <- pe
  }

  # Predicted values and residuals (p keyword).  Parallels get_reg_report().
  if (hasP) {

    preg <- glm$Fitted
    rreg <- glm$Residual
    vdat <- get_valid_obs(data, model)

    idcol <- seq_len(length(preg))
    nmscol <- suppressWarnings(as.numeric(rownames(vdat)))
    if (!is.null(nmscol) && all(!is.na(nmscol)))
      idcol <- nmscol

    if (length(idcol) == nrow(vdat)) {

      st <- data.frame(stub = idcol, DEPVAL = vdat[[var]],
                       PREVAL = preg, RESID = rreg, stringsAsFactors = FALSE)
      stlbl <- list(stub = "Obs", DEPVAL = "Observed",
                    PREVAL = "Predicted Value", RESID = "Residual")
      stfmt <- list(DEPVAL = "%.4f", PREVAL = "%.4f", RESID = "%.4f")

      # Confidence limits for the mean (clm) and for an individual predicted
      # value (cli).  ESTM() on the rows of the design matrix gives the fitted
      # value and the standard error of the mean, which reproduces
      # predict.lm(interval = "confidence").  The individual limits add the
      # error variance to that standard error, reproducing
      # predict.lm(interval = "prediction").
      if (hasCLM || hasCLI) {

        pctl <- (1 - get_alpha(opts)) * 100
        es <- est(mmX, mmX, rx, conf.level = alph)

        if (hasCLM) {
          st$STDMEAN <- es[, "Std. Error"]
          st$LCLM <- es[, "Lower CL"]
          st$UCLM <- es[, "Upper CL"]
          stlbl <- c(stlbl, list(STDMEAN = "Std Error Mean Predict",
                                 LCLM = paste0("Lower ", pctl, "% CL Mean"),
                                 UCLM = paste0("Upper ", pctl, "% CL Mean")))
          stfmt <- c(stfmt, list(STDMEAN = "%.4f", LCLM = "%.4f",
                                 UCLM = "%.4f"))
        }

        if (hasCLI) {
          dfr <- glm$ANOVA[2, 1]
          mse <- glm$ANOVA[2, 3]

          # A weight is the relative precision of the observation, so the error
          # variance of observation i is MSE / w[i].  Leaving the weight out
          # would widen the interval for the precisely measured observations.
          wv  <- if (is.null(weight)) 1 else vdat[[weight]]

          sei <- sqrt(es[, "Std. Error"] ^ 2 + mse / wv)
          tv  <- stats::qt(1 - (1 - alph) / 2, dfr)

          st$LCLI <- es[, "Estimate"] - tv * sei
          st$UCLI <- es[, "Estimate"] + tv * sei
          stlbl <- c(stlbl, list(LCLI = paste0("Lower ", pctl, "% CL Predict"),
                                 UCLI = paste0("Upper ", pctl, "% CL Predict")))
          stfmt <- c(stfmt, list(LCLI = "%.4f", UCLI = "%.4f"))
        }
      }

      labels(st) <- stlbl
      formats(st) <- stfmt
      ret[["OutputStatistics"]] <- st

      # SAS prints five residual summary statistics under the p option.
      n <- length(rreg)
      sumsq <- sum(rreg ^ 2)
      errss <- glm$ANOVA[2, 2]                       # RESIDUALS Sum Sq
      foa <- sum(rreg[-1] * rreg[-n]) / sumsq        # first order autocorrelation
      dw  <- sum(diff(rreg) ^ 2) / sumsq             # Durbin-Watson D

      resi <- data.frame(stub = c("Sum of Residuals",
                                  "Sum of Squared Residuals",
                                  "Sum of Squared Residuals - Error SS",
                                  "First Order Autocorrelation",
                                  "Durbin-Watson D"),
                         VALUE = c(round(sum(rreg), 5), sumsq,
                                   sumsq - errss, foa, dw),
                         stringsAsFactors = FALSE)
      labels(resi) <- list(VALUE = "Value")
      formats(resi) <- list(VALUE = "%.6f")
      ret[["ResidualStatistics"]] <- resi

    } else {
      warning("There was a problem creating the predicted values and residuals.")
    }
  }

  # Least-squares means (lsmeans effect).  sasLM's EMEAN returns one row per
  # model parameter named paste0(class, level); the rows for a requested effect
  # are the class name concatenated with each of its levels.
  if (hasLS) {

    em <- as.data.frame(unclass(glm$`Expected Mean`), stringsAsFactors = FALSE)
    em$stub <- rownames(glm$`Expected Mean`)
    vdat <- get_valid_obs(data, model)

    for (eff in lsmeans) {

      elvls <- levels(droplevels(factor(vdat[[eff]])))
      rn <- paste0(eff, elvls)

      erows <- em[match(rn, em$stub), ]

      # EMEAN returns the standard error, the confidence limits and the error
      # degrees of freedom alongside the mean itself.  The t test of the mean
      # against zero is the one piece SAS prints that is not in the table.
      tval <- erows$LSmean / erows$SE

      ls <- data.frame(stub = elvls,
                       LSMEAN = erows$LSmean,
                       STDERR = erows$SE,
                       PROBT = 2 * stats::pt(-abs(tval), erows$Df),
                       LCLM = erows$LowerCL,
                       UCLM = erows$UpperCL,
                       stringsAsFactors = FALSE)

      pctl <- (1 - get_alpha(opts)) * 100

      labels(ls) <- list(stub = eff,
                         LSMEAN = paste(var, "LSMEAN"),
                         STDERR = "Standard Error",
                         PROBT = "Pr > |t|",
                         LCLM = paste0("Lower ", pctl, "% CL"),
                         UCLM = paste0("Upper ", pctl, "% CL"))
      formats(ls) <- list(LSMEAN = "%.7f", STDERR = "%.7f", PROBT = pfmt,
                          LCLM = "%.7f", UCLM = "%.7f")

      ret[[paste0("LSMeans.", eff)]] <- ls
    }
  }

  # The augmented X'X crossproducts matrix (xpx) and its generalized inverse
  # (inverse).  "Augmented" means the dependent variable is carried as an extra
  # row and column, so the bottom right cell of the inverse is the error sum of
  # squares, the way SAS prints it.
  if (hasXPX || hasINV) {

    vdat <- get_valid_obs(data, model)
    aug <- cbind(ModelMatrix(model, data)$X, vdat[[var]])
    colnames(aug)[ncol(aug)] <- var

    if (!is.null(weight))
      aug <- aug * sqrt(vdat[[weight]])

    xpx <- crossprod(aug)

    if (hasXPX)
      ret[["XPX"]] <- matrix_table(xpx, "Parameter")

    # G2SWEEP(Augmented = TRUE), not g2inv(): g2inv() writes its result into the
    # leading rank-by-rank block, which is only correct when the singular
    # columns come last.  G2SWEEP keeps every column in place, carries the
    # dimnames, and puts the error sum of squares in the bottom right cell, the
    # way SAS prints it.
    if (hasINV) {

      inv <- G2SWEEP(xpx, Augmented = TRUE, eps = sing)

      # G2SWEEP leaves the augmented row holding -beta while the augmented
      # column holds beta.  SAS prints this matrix symmetric, with beta on both
      # sides, so mirror the column onto the row.
      inv[nrow(inv), ] <- inv[, ncol(inv)]

      ret[["Inverse"]] <- matrix_table(inv, "Parameter")
    }
  }

  # Type I, II and III estimable functions (e1, e2, e3).  Each row of the
  # returned matrix is the general form of an estimable function for one
  # parameter of the model.
  if (hasE1 || hasE2 || hasE3) {

    mm <- ModelMatrix(model, data)

    if (hasE1)
      ret[["TypeIEstimableFunctions"]] <-
        matrix_table(e1(crossprod(mm$X), eps = sing), "Effect")

    if (hasE2)
      ret[["TypeIIEstimableFunctions"]] <- matrix_table(e2(mm, eps = sing),
                                                        "Effect")

    if (hasE3)
      ret[["TypeIIIEstimableFunctions"]] <- matrix_table(e3(mm, eps = sing),
                                                         "Effect")
  }

  # Contrasts: an F-test per labeled contrast, via sasLM::CONTR.
  if (!is.null(contrast)) {

    zeta <- get_zeta(opts)

    crows <- NULL
    for (lbl in names(contrast)) {

      L <- build_glm_L(model, data, contrast[[lbl]], lbl)

      # A non-estimable contrast is dropped, the way SAS drops it.
      if (!check_estimable(L, mmX, g2u, lbl, zeta))
        next

      # cSS() takes the whole matrix and returns one row, with degrees of
      # freedom equal to the number of linearly independent rows of L.
      cr <- cSS(L, rx)
      crows <- rbind(crows,
                     data.frame(stub = lbl, DF = cr[1, 1], SUMSQ = cr[1, 2],
                                MEANSQ = cr[1, 3], FVAL = cr[1, 4],
                                PROBF = cr[1, 5], stringsAsFactors = FALSE))
    }

    if (!is.null(crows)) {
      rownames(crows) <- NULL

      formats(crows) <- list(DF = "%d", SUMSQ = "%.6f", MEANSQ = "%.6f",
                             FVAL = "%.2f", PROBF = pfmt)
      labels(crows) <- list(stub = "Contrast", DF = "DF", SUMSQ = "Contrast SS",
                            MEANSQ = "Mean Square", FVAL = "F Value",
                            PROBF = "Pr > F")
      ret[["Contrasts"]] <- crows
    }
  }

  # Estimates: a linear-combination estimate per label, via sasLM::ESTM.
  # Confidence limits are only added when clparm is requested, matching SAS
  # (the ESTIMATE statement shows CL only with the clparm option).
  if (!is.null(estimate)) {

    zeta <- get_zeta(opts)
    pctl <- (1 - get_alpha(opts)) * 100

    erows <- NULL
    for (lbl in names(estimate)) {

      L <- build_glm_L(model, data, estimate[[lbl]], lbl)

      if (!check_estimable(L, mmX, g2u, lbl, zeta))
        next

      es <- est(L, mmX, rx, conf.level = alph)
      row <- data.frame(stub = lbl, EST = es[1, "Estimate"],
                        STDERR = es[1, "Std. Error"], DF = es[1, "Df"],
                        "T" = es[1, "t value"], PROBT = es[1, "Pr(>|t|)"],
                        stringsAsFactors = FALSE, check.names = FALSE)
      if (hasCL) {
        row$LCLM <- es[1, "Lower CL"]
        row$UCLM <- es[1, "Upper CL"]
      }
      erows <- rbind(erows, row)
    }

    if (!is.null(erows)) {
      rownames(erows) <- NULL

      efmt <- list(EST = "%.7f", STDERR = "%.7f", DF = "%d",
                   "T" = "%.2f", PROBT = pfmt)
      elbl <- list(stub = "Parameter", EST = "Estimate",
                   STDERR = "Standard Error", DF = "DF", "T" = "t Value",
                   PROBT = "Pr > |t|")
      if (hasCL) {
        efmt <- c(efmt, list(LCLM = "%.7f", UCLM = "%.7f"))
        elbl <- c(elbl, list(LCLM = paste0("Lower ", pctl, "% CL"),
                             UCLM = paste0("Upper ", pctl, "% CL")))
      }
      formats(erows) <- efmt
      labels(erows) <- elbl
      ret[["Estimates"]] <- erows
    }
  }

  # Random effects: the Type III expected mean squares table, via sasLM::EMS.
  # SAS RANDOM prints, for each source, the expected mean square as a linear
  # combination of the variance components.
  if (!is.null(random)) {

    ems <- EMS(model, data, Type = 3)
    src <- rownames(ems)
    qcol <- colnames(ems)

    # Build the SAS-style expected mean square expression for each source.
    exprs <- character(length(src))
    for (i in seq_along(src)) {
      terms_i <- c("Var(Error)")
      for (j in seq_along(qcol)) {
        co <- ems[i, j]
        if (!is.na(co) && co != 0) {
          comp <- qcol[j]
          if (comp %in% random) {
            # Random effect: variance component with its coefficient.
            cf <- if (isTRUE(all.equal(co, 1))) "" else paste0(round(co, 4), " ")
            terms_i[length(terms_i) + 1] <- paste0(cf, "Var(", comp, ")")
          } else {
            # Fixed effect: SAS writes a bare quadratic form, no coefficient.
            terms_i[length(terms_i) + 1] <- paste0("Q(", comp, ")")
          }
        }
      }
      exprs[i] <- paste(terms_i, collapse = " + ")
    }

    emsdf <- data.frame(stub = src, EMS = exprs, stringsAsFactors = FALSE)
    labels(emsdf) <- list(stub = "Source",
                          EMS = "Type III Expected Mean Square")
    ret[["RandomEffects"]] <- emsdf
  }

  return(ret)
}


# Engine: build the programmatic GLM output data frame for one model + by-group.
# Parallels get_reg_output().  Produces one tidy row per model source per
# requested sum-of-squares type.
#' @import sasLM
#' @import common
#' @import fmtr
get_glm_output <- function(data, var, model, modelname, class = NULL,
                           opts = NULL, stats = NULL, byvars = NULL,
                           weight = NULL) {

  alph <- 1 - get_alpha(opts)

  if (!is.null(weight)) {
    glm <- GLM(model, data, conf.level = alph, Weights = data[[weight]])
  } else {
    glm <- GLM(model, data, conf.level = alph)
  }

  tycode <- c("Type I" = "SS1", "Type II" = "SS2", "Type III" = "SS3")

  ret <- data.frame(MODEL = modelname,
                    DEPVAR = var,
                    SOURCE = "ERROR",
                    TYPE = "ERROR",
                    DF = glm$ANOVA[2, 1],
                    SS = glm$ANOVA[2, 2],
                    MEANSQ = glm$ANOVA[2, 3],
                    FVAL = glm$ANOVA[2, 4],
                    PROBF = glm$ANOVA[2, 5],
                    stringsAsFactors = FALSE)

  for (ty in get_ss_types(stats)) {

    mat <- glm[[ty]]
    ss <- as.data.frame(unclass(mat), stringsAsFactors = FALSE)
    names(ss) <- c("DF", "SS", "MEANSQ", "FVAL", "PROBF")
    rownames(ss) <- NULL

    df1 <- data.frame(MODEL = modelname,
                      DEPVAR = var,
                      SOURCE = rownames(mat),
                      TYPE = tycode[[ty]],
                      ss,
                      stringsAsFactors = FALSE)
    rownames(df1) <- NULL

    ret <- rbind(ret, df1)
  }

  # Prepend by variable columns (BY, BY1, BY2, ...)
  if (!is.null(byvars)) {
    for (bnm in rev(names(byvars))) {
      ret <- cbind(stats::setNames(
        data.frame(as.character(byvars[bnm]), stringsAsFactors = FALSE), bnm),
        ret)
    }
  }

  rownames(ret) <- NULL

  return(ret)
}


# Drivers -------------------------------------------------------------------

# Loops by-groups x models and assembles the interactive HTML report.
# Parallels gen_report_reg().
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
                           lsmeans = NULL,
                           contrast = NULL,
                           estimate = NULL,
                           random = NULL) {

  spcs <- get_output_specs_glm(model, class = class, opts = opts,
                               output = output, report = TRUE, stats = stats)

  nms <- names(spcs)

  byres <- list()

  # Build by-group labels and split the data
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
          if (!is.na(bylbls[k]))
            lv <- bylbls[k]
        }

        cma <- if (l == length(by)) "" else ", "

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

      dt <- dtlst[[j]]
      bynm <- nm
      if (length(bylbls) > 0)
        bynm <- paste0(nm, ":", bylbls[j])

      byres[[bynm]] <- get_glm_report(dt, vnm, outp$formula, class = class,
                                      opts = opts, weight = weight,
                                      stats = stats, lsmeans = lsmeans,
                                      contrast = contrast, estimate = estimate,
                                      random = random)

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
        titles <- "The GLM Procedure"

      nmsret <- names(ret)

      if ("ANOVA" %in% nmsret) {
        spn <- span(1, ncol(ret$ANOVA), label = "Analysis of Variance", level = 1)
        attr(ret$ANOVA, "spans") <- list(spn)
      }

      out <- output_report(ret, dir_name = dirname(vrfl),
                           file_name = basename(vrfl), out_type = "HTML",
                           titles = titles, margins = .5, viewer = TRUE,
                           pages = length(byres))

      show_viewer(out)
    }
  }

  # Rename stub columns for returned report datasets
  if (has_option(output, "report")) {

    nmsret <- names(ret)

    if ("NObs" %in% nmsret)
      names(ret$NObs) <- sub("stub", "LABEL", names(ret$NObs), fixed = TRUE)

    if ("ClassLevels" %in% nmsret)
      names(ret$ClassLevels) <- sub("stub", "CLASS", names(ret$ClassLevels), fixed = TRUE)

    if ("ANOVA" %in% nmsret)
      names(ret$ANOVA) <- sub("stub", "SOURCE", names(ret$ANOVA), fixed = TRUE)

    for (tnm in c("TypeI", "TypeII", "TypeIII")) {
      if (tnm %in% nmsret)
        names(ret[[tnm]]) <- sub("stub", "SOURCE", names(ret[[tnm]]), fixed = TRUE)
    }

    if ("ParameterEstimates" %in% nmsret)
      names(ret$ParameterEstimates) <- sub("stub", "PARM",
                                           names(ret$ParameterEstimates),
                                           fixed = TRUE)

    if ("OutputStatistics" %in% nmsret)
      names(ret$OutputStatistics) <- sub("stub", "OBS",
                                         names(ret$OutputStatistics), fixed = TRUE)

    if ("ResidualStatistics" %in% nmsret)
      names(ret$ResidualStatistics) <- sub("stub", "LABEL",
                                           names(ret$ResidualStatistics), fixed = TRUE)

    for (lnm in nmsret[startsWith(nmsret, "LSMeans.")])
      names(ret[[lnm]]) <- sub("stub", "LEVEL", names(ret[[lnm]]), fixed = TRUE)

    if ("Contrasts" %in% nmsret)
      names(ret$Contrasts) <- sub("stub", "CONTRAST", names(ret$Contrasts),
                                  fixed = TRUE)

    if ("Estimates" %in% nmsret)
      names(ret$Estimates) <- sub("stub", "PARM", names(ret$Estimates),
                                  fixed = TRUE)

    if ("RandomEffects" %in% nmsret)
      names(ret$RandomEffects) <- sub("stub", "SOURCE", names(ret$RandomEffects),
                                      fixed = TRUE)
  }

  return(ret)
}


# Loops by-groups x models and assembles the programmatic output data frame.
# Parallels gen_output_reg().
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

  spcs <- get_output_specs_glm(model, class = class, opts = opts,
                               output = output, report = FALSE, stats = stats)

  res <- NULL

  if (length(spcs) > 0) {

    # Split by by-groups
    bdat <- list(data)
    if (!is.null(by))
      bdat <- split(data, data[ , by, drop = FALSE], sep = "|", drop = TRUE)

    bynms <- names(bdat)

    # Make up by variable names for output ds
    byn <- NULL
    if (!is.null(by)) {
      if (length(by) == 1)
        byn <- "BY"
      else
        byn <- paste0("BY", seq(1, length(by)))
    }

    nms <- names(spcs)

    for (j in seq_len(length(bdat))) {

      dat <- bdat[[j]]

      # Deal with by variable values
      bynm <- NULL
      if (!is.null(bynms) & !is.null(byn)) {
        bynm <- strsplit(bynms[j], "|", fixed = TRUE)[[1]]
        names(bynm) <- byn
      }

      for (i in seq_len(length(spcs))) {

        outp <- spcs[[i]]
        nm <- nms[i]

        tmpby <- get_glm_output(dat, var = outp$var, model = outp$formula,
                                modelname = nm, class = class, opts = opts,
                                stats = stats, byvars = bynm, weight = weight)

        if (is.null(res))
          res <- tmpby
        else
          res <- perform_set(res, tmpby)
      }
    }
  }

  rownames(res) <- NULL

  # Data shaping
  if (has_option(output, "long"))
    res <- shape_glm_data(res, "long")
  else if (has_option(output, "stacked"))
    res <- shape_glm_data(res, "stacked")

  return(res)
}


# Shapes the results of gen_output_glm(). Wide (default) keeps statistics in
# columns.  Long transposes statistics into rows keyed per source; stacked
# collapses everything to a single value column.  Parallels shape_reg_data().
shape_glm_data <- function(ds, shape) {

  ret <- ds

  if (!is.null(shape)) {

    bnms <- find.names(ds, "BY*")

    if (all(shape == "long")) {

      bv <- c(bnms, "MODEL", "DEPVAR", "SOURCE", "TYPE")
      ret <- proc_transpose(ds, by = bv, name = "STAT", log = FALSE)

    } else if (all(shape == "stacked")) {

      bv <- c(bnms, "MODEL", "DEPVAR", "SOURCE", "TYPE")
      ret <- proc_transpose(ds, by = bv, name = "STAT", log = FALSE)

      rnms <- names(ret)
      rnms[rnms %in% "COL1"] <- "VALUES"
      names(ret) <- rnms
    }
  }

  return(ret)
}

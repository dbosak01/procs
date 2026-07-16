

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
#'
#' A model may be specified using R model syntax or SAS model syntax.  To use
#' SAS syntax, the model statement must be quoted.
#'
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
#' @param by An optional by group.  If specified, the input data will be subset
#' on the by variable(s) prior to performing the analysis.
#' @param stats Optional statistics keywords.  Valid values are "ss1", "ss2",
#' "ss3", "solution", "clparm", "p", "est", and "clb".  The "solution" keyword
#' adds a parameter estimates table to the interactive report ("est" is an
#' accepted alias).  The "clparm" keyword adds confidence limits for the
#' estimates, using the alpha value from the \code{options} parameter ("clb" is
#' an accepted alias).  The "p" keyword adds predicted values and residuals to
#' the interactive report.
#' @param output Whether or not to return datasets from the function.  Valid
#' values are "out", "none", and "report", plus the data shaping keywords
#' "long", "stacked", and "wide".  Default is "out".
#' @param weight The name of a variable to use as a weight for each observation.
#' @param lsmeans The name of one or more class variables for which to compute
#' least-squares means.  Each requested effect produces a least-squares means
#' table on the interactive report.  The effect(s) must also appear on the
#' \code{class} parameter.
#' @param options A vector of optional keywords.  Valid values are "alpha =",
#' "noprint", "ss1", "ss2", "ss3", and "outstat".  The "outstat" option requests
#' an output dataset of the model sums of squares.  This is the default output
#' dataset, so the option does not normally need to be passed.
#' @param titles A vector of one or more titles to use for the report output.
#' @param where An expression to filter the rows before statistics are
#' calculated.  Use the \code{\link[base]{expression}} function.
#' @return Normally the requested statistics are shown interactively in the
#' viewer, and output results are returned as a data frame.  If "report"
#' datasets are requested they are returned as a list.
#' @import fmtr
#' @import tibble
#' @seealso [proc_reg()], [proc_anova()]
#' @export
proc_glm <- function(data,
                     model,
                     class = NULL,
                     by = NULL,
                     stats = NULL,
                     output = NULL,
                     weight = NULL,
                     lsmeans = NULL,
                     options = NULL,
                     titles = NULL,
                     where = NULL
) {

  # Deal with single value unquoted parameter values
  weight <- resolve_arg(weight)
  class <- resolve_arg(class)
  by <- resolve_arg(by)
  stats <- resolve_arg(stats)
  lsmeans <- resolve_arg(lsmeans)
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

    kopts <- c("alpha", "noprint", "ss1", "ss2", "ss3", "outstat")

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
               "est", "clb", "alpha")

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

  # Convert class variables to factors before fitting
  data <- make_factors(data, class)

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
                             lsmeans = lsmeans)
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
          by = by,
          stats = stats,
          output = output,
          weight = weight,
          lsmeans = lsmeans,
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
                    by = NULL,
                    stats = NULL,
                    output = NULL,
                    weight = NULL,
                    lsmeans = NULL,
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
                                  output = NULL, report = FALSE) {
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


# Convert the named `class` columns of `data` to factors before the model is
# fit.  sasLM::GLM requires categorical predictors to be factors in order to
# generate the correct design matrix and Type I/II/III sums of squares.
#
# Level ordering is deliberately rebuilt with factor() so that levels are sorted
# in ascending order.  This mirrors the SAS PROC GLM default (ORDER=FORMATTED),
# which controls the reference level and therefore the Type III / solution
# results.  droplevels() removes any empty levels left over after a where or NA
# subset so downstream degrees of freedom match SAS.
make_factors <- function(data, class = NULL) {

  if (!is.null(class)) {

    for (nm in class) {
      if (nm %in% names(data)) {
        data[[nm]] <- droplevels(factor(data[[nm]]))
      }
    }
  }

  return(data)
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
                           weight = NULL, stats = NULL, lsmeans = NULL) {

  alph <- 1 - get_alpha(opts)

  # solution/clparm request the parameter estimates (SAS keywords); "est"/"clb"
  # are accepted as aliases for consistency with proc_reg.
  hasSol <- has_option(stats, "solution") || has_option(stats, "est")
  hasCL  <- has_option(stats, "clparm") || has_option(stats, "clb")
  hasP   <- has_option(stats, "p")
  hasLS  <- !is.null(lsmeans)
  needBeta <- hasSol || hasCL

  if (!is.null(weight)) {
    glm <- GLM(model, data, conf.level = alph, Weights = data[[weight]],
               BETA = needBeta, Resid = hasP, EMEAN = hasLS)
  } else {
    glm <- GLM(model, data, conf.level = alph,
               BETA = needBeta, Resid = hasP, EMEAN = hasLS)
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

  # Overall model ANOVA
  aov <- as.data.frame(unclass(glm$ANOVA), stringsAsFactors = FALSE)
  aov <- data.frame(stub = c("Model", "Error", "Corrected Total"),
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
    biased <- pe$ESTIMABLE == 0
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
      labels(st) <- list(stub = "Obs", DEPVAL = "Observed",
                         PREVAL = "Predicted Value", RESID = "Residual")
      formats(st) <- list(DEPVAL = "%.4f", PREVAL = "%.4f", RESID = "%.4f")
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

    pctl <- (1 - get_alpha(opts)) * 100

    for (eff in lsmeans) {

      elvls <- levels(droplevels(factor(vdat[[eff]])))
      rn <- paste0(eff, elvls)

      erows <- em[match(rn, em$stub), ]

      # H0: LSMEAN = 0 test, matching SAS
      tval <- erows$LSmean / erows$SE
      probt <- 2 * pt(-abs(tval), erows$Df)

      ls <- data.frame(stub = elvls,
                       LSMEAN = erows$LSmean,
                       STDERR = erows$SE,
                       PROBT = probt,
                       LCLM = erows$LowerCL,
                       UCLM = erows$UpperCL,
                       stringsAsFactors = FALSE)

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

  ret <- NULL
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

    if (is.null(ret))
      ret <- df1
    else
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
                           lsmeans = NULL) {

  spcs <- get_output_specs_glm(model, class = class, opts = opts,
                               output = output, report = TRUE)

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
                                      stats = stats, lsmeans = lsmeans)

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
                               output = output, report = FALSE)

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

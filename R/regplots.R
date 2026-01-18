

# Regplots Main Function --------------------------------------------------

# RESIDUALBYPREDICTED: Residuals vs. Predicted values.
# RSTUDENTBYPREDICTED: Externally Studentized Residuals (RStudent) vs. Predicted values.
# RSTUDENTBYLEVERAGE: Externally Studentized Residuals vs. Leverage.
# QQ (or QQPLOT): Normal Quantile-Quantile (Q-Q) plot of residuals.
# OBSERVEDBYPREDICTED: Dependent variable (Observed) vs. Predicted values.
# COOKSD: Cook’s D statistic vs. Observation number.
# RESIDUALHISTOGRAM: Histogram of residuals.
# RFPLOT: Residual-Fit (RF) spread plot.

# ALL: Requests all plots relevant to the current analysis and options.
# FITPLOT: Produces a scatter plot of the dependent variable against the regressor, including the fitted line and confidence/prediction bands. This is only available for models with a single regressor.
# RESIDUALS or RESIDUALPLOT: Produces a panel of residual plots against each independent variable in the model.
# PARTIAL (or PARTIALPLOT): Requests partial regression residual plots for each regressor. This requires the PARTIAL option to be specified in the MODEL statement.
# CRITERIONPANEL: When using model selection (like SELECTION=STEPWISE), this displays how criteria like AIC, SBC, or R-square change across different model sizes.


# panel = TRUE/FALSE
# type = diagnostics,  residualbypredicted, RStudentbypredicted, rstudentbyleverage,
# qq/qqplot, observedbypredicted, cooksd, residualhistogram, rfplot, residuals/residualplot, fitplot
# stats = default aic sbc
# all = obs, edf, rsquare, sse, coefvar, bic, gmsep, pc, sp, parms, mse, adjrsquare, depmean, aic, cp, jp, sbc


#' @title Request Regression Plots
#' @description A function to request regression plots on a call to
#' \code{\link{proc_reg}}. The function allows you to specify the type of
#' regression plots to produce. It produces a combined diagnostics panel
#' by default.  You may also specify individual plots to create by setting
#' the "panel" parameter to FALSE, and passing a vector of plot names
#' on the "type" parameter.
#' @details Any requested
#' plots will be displayed on interactive reports only.
#' Plots are created as jpeg files, and stored in a temp directory.  Those
#' temporary files are then referenced by the interactive report to display
#' the graphic.
#'
#' If desired, you may
#' output the report objects and pass to \code{\link{proc_print}}. To do this,
#' set \code{output = report} on the call to \code{\link{proc_freq}}, and pass
#' the entire list to \code{\link{proc_print}}.
#' @section Plots:
#' The \code{plots} parameter allows you to request several types of regression
#' plots. Below are the types of plots that are supported.  The list shows
#' the plot type keyword needed to request the plot, and a brief description:
#' \itemize{
#' \item{\strong{diagnostics}:  A fit diagnostics panel that contains 8 different
#' types of plots and a table of statistics.
#' }
#' \item{\strong{residuals}: Produces a panel of residual plots against
#'  each independent variable in the model.
#' }
#' \item{\strong{fitplot}:  Produces a scatter plot of the dependent variable
#' against the regressor, including the fitted line and confidence/prediction bands.
#' This is only available for models with a single regressor.
#' }
#' \item{\strong{qqplot}: Normal Quantile-Quantile (Q-Q) plot of residuals.
#' }
#' \item{\strong{rfplot}: Residual-Fit (RF) spread plot.
#' }
#' \item{\strong{residualbypredicted}: Residuals vs. Predicted values.
#' }
#' \item{\strong{rstudentbypredicted}: Externally Studentized Residuals (RStudent) vs. Predicted values.
#' }
#' \item{\strong{rstudentbyleverage}: Externally Studentized Residuals vs. Leverage.
#' }
#' \item{\strong{cooksd}: Cook’s D statistic vs. Observation number.
#' }
#' \item{\strong{residualhistogram}: Histogram of residuals,
#' with a normal and kernel curve overlay.
#' }
#' \item{\strong{observedbypredicted}: Dependent variable (Observed) vs. Predicted values.
#' }
#' \item{\strong{residualboxplot}: A boxplot of residuals.
#' }
#' }
#' The above plots may be requested in different ways: as a vector of keywords,
#' or as a call to the \code{\link{regplot}} function.  The keyword approach will
#' produce plots with default parameters. A call to \code{\link{regplot}} will
#' give you control over some parameters to the charts.  See the \code{\link{regplot}}
#' function for further details.
#'
#' @section Statistics:
#' \itemize{
#' \item{\strong{adjrsq}: Adjusted R-square.
#' }
#' \item{\strong{aic}: Akaike's information criterion.
#' }
#' \item{\strong{coeffvar}: Coefficient of variation.
#' }
#' \item{\strong{depmean}: Mean of dependent.
#' }
#' \item{\strong{default}: A set of default statistics.
#' }
#' \item{\strong{edf}: Error degrees of freedom.
#' }
#' \item{\strong{mse}: Mean squared error.
#' }
#' \item{\strong{nobs}: Number of observations used.
#' }
#' \item{\strong{nparm}: Number of parameters in the model (including the intercept).
#' }
#' \item{\strong{rsquare}: The R-square statistic.
#' }
#' \item{\strong{sse}: Error sum of squares.
#' }
#' }
#'
#' @param type The type(s) of plot to create. Multiple types should be passed
#' as a vector of strings.  Valid values are "diagnostics", "residualbypredicted",
#' "rstudentbypredicted", "rstudentbyleverage", "qqplot", "observedbypredicted",
#' "cooksd", "residualhistogram", "residualboxplot",
#' "rfplot", "residuals", and "fitplot".  The default value is
#' a vector with "diagnostics", "residuals", and "fitplot".  The "diagnostics"
#' keyword produces a single combined chart with 8 different plots and a
#' selection of statistics in a small table.  The statistics can be controlled
#' by the \code{stats} parameter.
#' @param panel Whether or not to display the diagnostics plots combined into
#' in a single panel.  Default is TRUE.  A value of FALSE will create
#' individual plots instead.  This parameter is equivalent to the
#' "unpack" keyword in SAS.
#' @param stats The statistics to display on the diagnostics panel. Valid values
#' are: "adjrsq", "aic", "coeffvar", "depmean", "default", "edf", "mse", "nobs",
#' "nparm", "rsquare", and "sse".  The
#' default value is "default", which produces the following statistics:
#' "nobs", "nparm", "edf", "mse", "rsquare", and "adjrsq". You may also pass
#' the value "none" if you do not want any statistics shown on the chart.
#' @param label Whether or not to label values automatically. Valid values
#' are TRUE or FALSE.  Default is FALSE. If TRUE, this options will assign
#' labels to outlier values on some charts. Only some individual charts are labelled,
#' not the panel diagnostics chart.
#' @param id If the \code{label} parameter is TRUE, this parameter determines
#' which value is assigned to the label.  By default, the row number will
#' be assigned.  You may also assign a column name from the input dataset
#' to use as the label value.
#' @examples
#' library(procs)
#'
#' # Turn off printing for CRAN checks
#' # Set to TRUE to run in local environment
#' options("procs.print" = FALSE)
#'
#'
#' # Example 1: Regression statistics with default plots
#' res <- proc_reg(iris, model = "Sepal.Length = Petal.Length",
#'                  output = report,
#'                  plots = regplot,
#'                  titles = "Iris Regression Statistics")
#'
#' # View results
#' res
#'
#' # Example 2: Regression statistics with custom plot strings and by variable
#' res <- proc_reg(iris, model = "Sepal.Length = Petal.Length",
#'                  output = report,
#'                  by = Species,
#'                  plots = v(diagnostics, residuals, residualhistogram, cooksd),
#'                  titles = "Iris Regression Statistics")
#'
#' # View results
#' res
#'
#' # Example 3: Regression statistics with multiple models, same plot
#' res <- proc_reg(iris, model = c("Sepal.Length = Petal.Length",
#'                                 "Sepal.Length = Sepal.Width",
#'                                 "Sepal.Length = Petal.Width"),
#'                  output = report,
#'                  plots = "diagnostics",
#'                  titles = "Iris Regression Statistics")
#'
#' # View results
#' res
#'
#' # Example 4: Regression statistics with multiple models, different plot strings
#' res <- proc_reg(iris, model = c("Sepal.Length = Petal.Length",
#'                                 "Sepal.Length = Sepal.Width",
#'                                 "Sepal.Length = Petal.Width"),
#'                  output = report,
#'                  plots = list("diagnostics",
#'                               "cooksd",
#'                               "residualhistogram"),
#'                  titles = "Iris Regression Statistics")
#'
#' # View results
#' res
#'
#' # Example 5: Regression statistics with multiple models, different plot functions
#' res <- proc_reg(iris, model = c("Sepal.Length = Petal.Length",
#'                                 "Sepal.Length = Sepal.Width",
#'                                 "Sepal.Length = Petal.Width"),
#'                  output = report,
#'                  plots = list(regplot(type = "diagnostics"),
#'                               regplot(type = "cooksd",
#'                                       label = TRUE),
#'                               regplot(type = "fitplot",
#'                                       label = TRUE,
#'                                       stats = c("nobs", "mse", "rsquare"))),
#'                  titles = "Iris Regression Statistics")
#'
#' # View results
#' res
#' @export
regplot <- function(type = c("diagnostics", "residuals", "fitplot"), panel = TRUE,
                    stats = "default", label = FALSE, id = NULL) {

  # Non-standard evaluation

  otype <- deparse(substitute(type, env = environment()))
  type <- tryCatch({if (typeof(type) %in% c("character", "NULL")) type else otype},
                   error = function(cond) {otype})

  ostats <- deparse(substitute(stats, env = environment()))
  stats <- tryCatch({if (typeof(stats) %in% c("character", "NULL")) stats else ostats},
                     error = function(cond) {ostats})


  # Parameter Checks
  vldvals <- c("diagnostics", "residuals", "fitplot", 'qqplot', 'rfplot',
               "residualbypredicted", "rstudentbypredicted", "rstudentbyleverage",
               "cooksd", "residualhistogram", "observedbypredicted", "residualboxplot")
  if (any(!type %in% vldvals)) {

    ivd <- type[!type %in% vldvals]
    stop(paste0("Parameter value for 'type' invalid: ", paste0("'", ivd, "'", collapse = ", "),
                "\nValid values are: ",
                "'diagnostics', 'residuals', 'fitplot', 'qqplot', 'rfplot', ",
                "'residualbypredicted', 'rstudentbypredicted', 'rstudentbyleverage', ",
                "'cooksd', 'residualhistogram', 'observedbypredicted', 'residualboxplot'."
                ))
  }

  if (!panel %in% c(TRUE, FALSE)) {
    stop("Parameter value for 'panel' invalid. Valid values are TRUE or FALSE.")
  }

  # sse, bic, gmsep, pc, sp, aic, cp, jp, sbc
  statlst <-  c("adjrsq", "aic", "coeffvar", "depmean", "default", "edf", "mse",
                "nobs", "none", "nparm", "rsquare", "sse")
  if (!all(stats %in% statlst)) {
    stop(paste0("Parameter value for 'stats' invalid. Valid values are ",
                paste0("'", statlst, "'", collapse = ", "), "."))
  }

  # Create object
  ret <- structure(list(), class = c("regplot", "list"))

  ret$stats <- tolower(stats)
  ret$panel <- panel
  ret$type <- tolower(type)
  ret$label <- label
  ret$id <- id

  return(ret)
}



# Plot Rendering ----------------------------------------------------------


#' @noRd
render_regplot <- function (dat, res, mdl, plt, alph) {

  ret <- NULL

  if (is.character(plt)) {
    if (all(plt == "regplot")) {
      plt <- regplot()
    } else {
      plt <- regplot(type = plt)
    }
  }

  if ("regplot" %in% class(plt)) {

    typs <- plt$type

    # "Unpack" logic
    if ("diagnostics" %in% typs & plt$panel == FALSE) {

      pos <- match("diagnostics", typs)

      vins <- c("residualbypredicted", "rstudentbypredicted", "rstudentbyleverage",
                "qqplot", "observedbypredicted", "cooksd", "residualhistogram", "rfplot")

      nv <- append(typs, vins, after = pos)

      typs <- nv[-pos]
    }

    ret <- list()

    # "diagnostics", "residuals", "fitplot", 'qqplot', 'rfplot',
    # "residualbypredicted", "rstudentbypredicted", "rstudentbyleverage",
    # "cooksd", "residualhistogram", "observedbypredicted"

    for (tp in typs) {

      if (tp == "diagnostics") {
        ret[["diagnostics"]] <- render_diagnostics(dat, res, mdl, plt, alph)
      } else if (tp == "residuals") {  # Can be more than 1
        resplts <- render_residuals(dat, res, mdl)
        if ("plot_spec" %in% class(resplts)) {
          ret[["residuals"]] <- resplts
        } else {
          for (idx in seq_len(length(resplts))) {
            ret[[paste0("residuals", idx)]] <- resplts[[idx]]
          }
        }
      } else if (tp == "fitplot") {
        ret[["fitplot"]] <- render_fitplot(dat, res, mdl, plt, alph)
      } else if (tp == "qqplot") {
        ret[["qqplot"]] <- render_qqplot(dat, res, mdl)
      } else if (tp == "rfplot") {
        ret[["rfplot"]] <- render_rfplot(dat, res, mdl)
      } else if (tp == "cooksd") {
        ret[["cooksd"]] <- render_cooksd(dat, res, mdl, plt)
      } else if (tp == "residualbypredicted") {
        ret[["residualbypredicted"]] <- render_residualbypredicted(dat, res, mdl)
      } else if (tp == "rstudentbypredicted") {
        ret[["rstudentbypredicted"]] <- render_rstudentbypredicted(dat, res, mdl, plt)
      } else if (tp == "rstudentbyleverage") {
        ret[["rstudentbyleverage"]] <- render_rstudentbyleverage(dat, res, mdl, plt)
      } else if (tp == "observedbypredicted") {
        ret[["observedbypredicted"]] <- render_observedbypredicted(dat, res, mdl, plt)
      } else if (tp == "residualhistogram") {
        ret[["residualhistogram"]] <- render_residualhistogram(dat, res, mdl, plt)
      }else if (tp == "residualboxplot") {
        ret[["residualboxplot"]] <- render_residualboxplot(dat, res, mdl, plt)
      }
    }

    if (length(ret) == 0) {
      ret <- NULL
    }
  }

  return(ret)
}



# Diagnostics Panel -------------------------------------------------------

# Deal with "none" stats parameter.  Adding extra box plot.
#' @noRd
render_diagnostics <- function(dat, res, mdl, plt, alph) {

  op <- par("mar")
  om <- par("oma")
  ofig <- par("fig")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 6  # Height in inches
  wdi <- 6  # Width in inches

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Set margins
  par(mfrow = c(3, 3),
      mar = c(2, 2, 1, 0) + 0.1,
      oma = c(0, 0, 3, .75))


  #******************************
  # 1) Residual By Predicted
  #******************************

  # Set margins
  par(mar = c(4, 4, 0, 0) + 0.1)

  # Prepare data
  rdt <- res$OutputStatistics$RESID
  pdt <- res$OutputStatistics$PREVAL

  # Get scales
  xscl <- get_scale(pdt, .05)
  yscl <- get_zero_scale(rdt, .05)

  # Generate plot
  plot(pdt, rdt,
       main = "",
       xlab = "",
       ylab = "",
       pch  = 1,          # open circles
       cex = 1.3,
       col  = "#05379B",
       xlim = xscl,
       ylim = yscl,
       axes = FALSE
  )

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # Generate zero line
  abline(h = 0, col = "grey60", lwd = 1)

  # Add labels
  mtext("Predicted Value", side = 1, line = par("mar")[1] - 2, cex = .9)
  mtext("Residual", side = 2, line = par("mar")[2] - 2, cex = .9)

  # Frame
  box(col = "grey70", lwd = 1)


  #******************************
  # 2) RStudent By Predicted
  #******************************

  # Prepare data
  fit <- lm(mdl, data = dat)
  rdt <- rstudent(fit)
  pdt <- fitted(fit)

  # Set margins
  par(mar = c(4, 4, 0, 0) + 0.1)

  # Get scales
  xscl <- get_scale(pdt, .05)
  yscl <- get_zero_scale(rdt, .1)

  # Generate plot
  plot(pdt, rdt,
       main = "",
       xlab = "",
       ylab = "",
       pch  = 1,          # open circles
       cex = 1.3,
       col  = "#05379B",
       xlim = xscl,
       ylim = yscl,
       axes = FALSE
  )

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # Generate reference lines
  abline(h =  2, col = "grey60", lwd = 1)
  abline(h = -2, col = "grey60", lwd = 1)

  # Add bottom label
  mtext("Predicted Value", side = 1, line = par("mar")[1] - 2, cex = .9)
  mtext("RStudent", side = 2, line = par("mar")[2] - 2, cex = .9)

  # Frame
  box(col = "grey70", lwd = 1)

  #******************************
  # 3) RStudent By Leverage
  #******************************

  # Prepare data
  fit <- lm(mdl, data = dat)
  rdt <- rstudent(fit)
  ldt <- hatvalues(fit)

  n <- length(ldt)
  p <- length(coef(fit))      # includes intercept

  # PROC REG-style cutoff
  cut <- 2 * p / n

  # Set margins
  par(mar = c(4, 4, 0, 0) + 0.1)

  # Get scales
  xscl <- c(min(ldt) * .9, max(ldt, cut) * 1.10)
  yscl <- get_zero_scale(rdt, .1)

  # Define colors
  nc <- "#05379B"   # Normal color

  # Generate plot
  plot(ldt, rdt,
       main = "",
       xlab = "",
       ylab = "",
       pch  = 1,          # open circles
       cex = 1.3,
       col  = nc,
       xlim = xscl,
       ylim = yscl,
       axes = FALSE
  )

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # Generate reference lines
  abline(h =  2, col = "grey60", lwd = 1)
  abline(h = -2, col = "grey60", lwd = 1)
  abline(v = cut, col = "grey60", lwd = 1)  # Cutoff

  # Add labels
  mtext("Leverage", side = 1, line = par("mar")[1] - 2, cex = .9)
  mtext("RStudent", side = 2, line = par("mar")[2] - 2, cex = .9)

  # Frame
  box(col = "grey70", lwd = 1)

  #******************************
  # 4) QQPlot
  #******************************

  # Prepare data
  rdt <- res$OutputStatistics$RESID

  # Set margins
  par(mar = c(4, 4, 0, 0) + 0.1)

  # Get y scale
  mx <- max(abs(rdt)) * 1.125  #1.0025
  scl <- c(-mx, mx)

  # Draw plot
  qqnorm(rdt,
         main = "",
         xlab = "",
         ylab = "",
         pch  = 1,          # open circles
         col  = "#05379B",
         cex = 1.3,
         ylim = scl,
         axes = FALSE)

  # Add diagonal line
  usr <- par("usr")
  segments(usr[1], usr[3], usr[2], usr[4], col = "grey60")

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # X Axis label
  mtext("Quantile", side = 1, line = par("mar")[1] - 2, cex = .9)
  mtext("Residual", side = 2, line = par("mar")[2] - 2, cex = .9)

  # Frame
  box(col = "grey70", lwd = 1)

  #******************************
  # 5) Observed by Predicted
  #******************************

  # Prepare data
  odt <- dat[[dvr]]
  pdt <- res$OutputStatistics$PREVAL

  # Set margins
  par(mar = c(4, 4, 0, 0) + 0.1)

  # Get scales
  xscl <- get_scale(pdt, 0)
  yscl <- get_scale(odt, 0)
  scl <- c(min(xscl[1], yscl[1]), max(xscl[2], yscl[2]))

  # Generate plot
  plot(pdt, odt,
       main = "",
       xlab = "",
       ylab = "",
       pch  = 1,          # open circles
       cex = 1.3,
       col  = "#05379B",
       xlim = scl,
       ylim = scl,
       axes = FALSE
  )

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", tck = -0.015, mgp = c(3, .5, 0))
  axis(side = 2, las = 1, col.ticks = "grey55", tck = -0.015, mgp = c(3, .5, 0))

  # Generate diagonal line
  usr <- par("usr")
  segments(usr[1], usr[3], usr[2], usr[4], col = "grey60")

  # Add labels
  mtext("Predicted Value", side = 1, line = par("mar")[1] - 2, cex = .9)
  mtext(dvr, side = 2, line = par("mar")[1] - 2, cex = .9)

  # Frame
  box(col = "grey70", lwd = 1)

  #******************************
  # 6) Cook's D
  #******************************

  # Set margins
  par(mar = c(4, 5, 0, 0) + 0.1)

  # Calculate statistics
  fit <- lm(mdl, data = dat)
  cd <- cooks.distance(fit)
  xcd <- seq_along(cd)
  n  <- length(cd)
  p  <- length(coef(fit))   # includes intercept

  ## Calculate cutoff
  cutoff <- 4 / n

  plot(xcd, cd,
       type = "h",            # vertical spikes
       lwd  = 1,
       col  = "#05379B",
       xlab = "",
       ylab = "",
       main = "",
       ylim = c(0, max(cd, cutoff) * 1.10),
       axes = FALSE)

  ## Open circles at the top of each spike
  points(seq_along(cd), cd,
         pch = 1,
         col = "#05379B", cex = 1.25)

  ## Reference line (PROC REG default)
  abline(h = cutoff, col = "grey60", lwd = 1)


  # Add custom axes
  axis(side = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Generate zero line
  abline(h = 0, col = "#05379B", lwd = 1)

  # Add labels
  mtext("Observation", side = 1, line = par("mar")[1] - 2, cex = .9)
  mtext("Cook's D", side = 2, line = par("mar")[1] - 1, cex = .9)

  # Frame
  box(col = "grey70", lwd = 1)

  #******************************
  # 7) Residual Histogram
  #******************************

  # Prepare data
  rdt <- res$OutputStatistics$RESID

  # Set margins
  par(mar = c(4, 4, 0, 0) + 0.1)

  # Calculate stats
  n   <- length(rdt)
  mu  <- mean(rdt)
  sdx <- sd(rdt)

  # Histogram (Percent scale)
  h <- hist(rdt,
            breaks = "Sturges",
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Use calculated breaks to get scale
  scl <- get_reg_xlim(h$breaks, mu, sdx, pad_frac = 0, max_sigma = 4)

  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = "",
       xlab = "",
       ylab = "",
       xlim = scl,
       ylim = c(0, max(h$counts) * 1.05),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  x <- seq(min(rdt) * 2, max(rdt) * 2, length.out = 300)

  y_norm <- dnorm(x, mean = mu, sd = sdx)
  y_norm <- y_norm / max(y_norm) * max(h$counts)

  lines(x, y_norm, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  dens <- density(rdt)

  y_kern <- dens$y / max(dens$y) * max(h$counts)

  lines(dens$x, y_kern, col = "orangered2", lwd = 2)

  # Add x axis label
  mtext("Residual", side = 1, line = par("mar")[1] - 2, cex = .9)
  mtext("Percent", side = 2, line = par("mar")[1] - 2, cex = .9)

  # Calculate x axis ticks
  df <- abs(h$mids[1] - h$mids[2])
  te <- (df * seq(1, 10)) + h$mids[length(h$mids)]
  be <- (-(df * seq(10, 1))) + h$mids[1]
  df <-  c(be, h$mids, te)
  xtks <- df[df > scl[1] & df < scl[2]]

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", at = xtks, labels = TRUE,
       mgp = c(3, .5, 0), tck = -0.015, cex.axis = .75)
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Frame
  box(col = "grey70", lwd = 1)

  #******************************
  # 8) RF Plot
  #******************************

  # Prepare data
  rdt <- res$OutputStatistics$RESID
  fdt <- res$OutputStatistics$PREVAL
  # dt <- dat[[ivr]]

  # Get length of predicted values
  n <- length(fdt)

  ## Proportion less (SAS-style)
  p <- (seq_len(n) - 0.5) / n

  ## Left panel: Fit Mean
  fit_centered <- sort(fdt - mean(fdt))

  ## Right panel: Residuals
  res_sorted <- sort(rdt)

  # Get y scale
  smin <- min(fit_centered)
  if (min(res_sorted) < smin) {
    smin <- min(res_sorted)
  }

  smax <- max(fit_centered)
  if (min(res_sorted) > smax) {
    smax <- max(res_sorted)
  }

  # Finalize y scale
  yscl <- c(smin, smax) * 1.05

  # Set margins
  par(mar = c(4, 4, 1.5, 0) + 0.1,
      fig = c(.334, .53, 0, .334), new = TRUE)  # Can place in specific spot

  ## Left: Fit Mean
  plot(p, fit_centered,
       pch = 1,
       col = "#05379B",
       xlab = "",
       ylab = "",
       ylim = yscl,
       main = "Fit-Mean",
       font.main = 1,
       cex.main = 1.4,
       axes = FALSE)

  box(col = "grey70", lwd = 1)

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", col = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", las = 1, col = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  ## Plot layout
  par(mar = c(4, .2, 1.5, 0) + 0.1,
      fig = c(.53, .667, 0, .334), new = TRUE)

  ## Right: Residual
  plot(p, res_sorted,
       pch = 1,
       col = "#05379B",
       xlab = "",
       ylab = "",
       ylim = yscl,
       main = "Residual",
       font.main = 1,
       cex.main = 1.4,
       axes = FALSE)

  box(col = "grey70", lwd = 1)

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", col = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  ## Add Labels
  mtext("             Proportion Less",
        side = 1, outer = TRUE,
        line = par("oma")[1] - 2, cex = .9)

  #******************************
  # 9) Statistics or Boxplot
  #******************************

  if (!"none" %in% plt$stats) {

    # Set margins and figure placement
    par(mar = c(4, 5, 0, 2) + 0.1,
        fig = c(.667, 1, 0, .334), new = TRUE)

    # Initialize empty plot
    # to create drawing area.
    plot.new()

    # Get needed statistics as named vector
    stats <- collect_stats(res, plt)

    if ("aic" %in% plt$stats) {
      aicv <- round(extractAIC(fit)[2], 4)
      stats <- append(stats, aicv)
      names(stats)[length(stats)] <- "AIC"
    }

    # Set parameters
    box_col = "white"
    border_col = "grey70"
    pad_y = 0.03            # vertical padding as fraction of y-range
    pad_x = 0.03

    # Get environment settings
    usr <- par("usr")  # c(x1, x2, y1, y2)
    x_rng <- diff(usr[1:2])
    y_rng <- diff(usr[3:4])

    # x positions (user sets; relative to right edge)
    x_left  <- 0
    x_right <- 1

    # Determine line spacing
    line_h <- strheight("Mg")         # approx line height in user units
    gap_h  <- 0.8 * line_h           # gap between lines
    inner_h <- length(stats) * line_h + (length(stats) - 1) * gap_h

    # box padding in user units
    py <- pad_y * y_rng
    px <- pad_x * x_rng

    box_h <- inner_h + 2 * py

    # center vertically in plot area
    y_mid <- mean(usr[3:4])
    y_bot <- y_mid - box_h / 2
    y_top <- y_mid + box_h / 2

    # draw box
    rect(x_left, y_bot - (py/2), x_right, y_top, col = box_col, border = border_col)

    # y positions for each row (top to bottom)
    y_start <- y_top - py - line_h / 2
    y_pos <- y_start - (seq_along(stats) - 1) * (line_h + gap_h)

    # left column: names
    text(x_left + px, y_pos,
         labels = names(stats),
         adj = c(0, 0.5), cex= 1.25)

    # right column: values
    text(x_right - px, y_pos,
         labels = unname(stats),
         adj = c(1, 0.5), cex= 1.25)

  } else {  # Boxplot

    # Prepare data
    rdt <- res$OutputStatistics$RESID

    # Set margins
    par(mar = c(4, 5, 0, 0) + 0.1,
        fig = c(.667, 1, 0, .334), new = TRUE)

    # Get scales
    yscl <- get_zero_scale(rdt, .05)

    # Calculate SAS compatible quantile
    q <- quantile(rdt, probs = c(0, .25, .5, .75, 1), type = 2)

    # Prepare to pass data
    bp <- list(
      stats = matrix(q, ncol = 1),
      n = length(rdt),
      conf = NULL,
      out = numeric(0)
    )

    # Box plot
    bxp(bp,
        main = "",
        ylab = "",
        boxfill = "#CAD5E5",
        border = "grey40",
        outline = FALSE,
        lty = 1,
        ylim = yscl,
        whiskcol = "#05379B",
        staplecol = "#05379B",
        medcol = "#05379B",
        medlwd = 1,
        boxwex = .65,
        axes = FALSE)


    # Mean diamond
    points(1, mean(rdt),
           pch = 5,          # diamond
           col = "#05379B",
           cex = 1.4)

    # Add custom axis
    aval <- axis(side = 2, las = 1, col.ticks = "grey55",
                 mgp = c(3, .5, 0), tck = -0.015)

    # Label
    mtext("Residual", side = 2, line = par("mar")[1] - 1, cex = .9)

    # Frame
    box(col = "grey70", lwd = 1)

  }


  #******************************
  # Finish up
  #******************************

  # Set title
  mtext(paste0("Fit Diagnostics for ", dvr), side = 3, cex = 1.2,
        outer = TRUE, font = 2, line = par("oma")[3] - 2.25)

  # Frame
  box("outer", col = "grey70", lwd = 1)

  # Restore margins
  par(mar = op,
      oma = om,
      mfrow = c(1, 1),
      fig = ofig,
      new = FALSE)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}



# Individual Plots --------------------------------------------------------


# What if there are multiple independent variables?
# Need to show separate plot for each independent variable, all in
# same panel.
#' @noRd
render_residuals <- function(dat, res, mdl) {

  op <- par("mar")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Max per panel
  pnlmx <- 6

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Set titles
  ttl <- "Residuals for "
  if (length(ivr) > 1) {
    ttl <- "Residual by Regressors for "
  }

  # Determine number of panels and panel layout
  if (length(ivr) == 1) {
    pnls <- 1
    rws <- c(1, 1)
  } else if (length(ivr) == 2) {
    pnls <- 1
    rws <- c(1, 2)
    hti <- 3.5
  } else if (length(ivr) %in% c(3, 4)) {
    pnls <- 1
    rws <- c(2, 2)
  } else if (length(ivr) %in% c(5, 6)) {
    pnls <- 1
    rws <- c(2, 3)
  } else if (length(ivr) > pnlmx) {
    pnls <- ceiling(length(ivr) / pnlmx)
    rws <- c(2, 3)
  }

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Set up object to return
  ret <- list()

  # Get residuals
  rdt <- res$OutputStatistics$RESID

  # Split independent variables by panel
  if (length(ivr) > pnlmx) {
    fct <- ceiling(seq_along(ivr) / pnlmx)
    ivrs <- split(ivr, fct)
  } else {
    ivrs <- list(ivr)
  }

  for (pnl in seq(1, pnls)) {

    # Create temp file path
    pth <- tempfile(fileext = ".jpg")

    # Output to image file
    # All output types accept jpeg
    # So start with that
    jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

    # Set up margins and grid
    par(oma = c(0, 4, 2.5, .75) + 0.1,
        mar = c(4, 1, 0, 0),
        mfrow = rws)

    # Flag for first chart
    firstChart <- TRUE

    # Chart counter
    chtcnt <- 0

    for (iv in ivrs[[pnl]]) {

      # Increment counter
      chtcnt <- chtcnt + 1

      # Get data for independent variable
      dt <- dat[[iv]]

      # Get y scale
      mx <- max(abs(rdt)) * 1.05
      scl <- c(-mx, mx)

      # Generate plot
      plot(dt, rdt,
           main = "",
           xlab = "",
           ylab = "",
           pch  = 1,          # open circles
           cex = 1.3,
           col  = "#05379B",
           ylim = scl,
           axes = FALSE
           )

      # Add custom axes
      axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
      if (firstChart) {
        axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
      }

      # Put x axis label
      mtext(iv, side = 1, line = par("mar")[1] - 1.5)

      # Generate zero line
      abline(h = 0, col = "grey60", lwd = 1)

      # Frame
      box(col = "grey70", lwd = 1)

      # Set flag
      if (chtcnt %% par("mfrow")[2] == 0) {
        firstChart <- TRUE
      } else {
        firstChart <- FALSE
      }


    }

    # Title
    mtext(paste0(ttl, dvr), side = 3,
          font = 2, cex = 1.2, outer = TRUE, line = par("oma")[3] - 2)

    # Y Axis lable
    mtext("Residual", side = 2, outer = TRUE, line = par("oma")[2] - 1.5)

    # Draw line around entire chart
    box("outer", col = "grey70", lwd = 1)

    # Close device context
    dev.off()

    # Put plot in reporter plot object
    ret[[length(ret) + 1]] <- create_plot(pth, height = hti, width = wdi)

  }

  # Restore margins
  par(mar = op, mfrow = c(1, 1))

  if (length(ret) == 1) {
    ret <- ret[[1]]
  }

  return(ret)

}


render_residualbypredicted <- function(dat, res, mdl) {

  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Prepare data
  rdt <- res$OutputStatistics$RESID
  pdt <- res$OutputStatistics$PREVAL

  # Set margins
  par(mar = c(4, 5, 2, .75) + 0.1)

  # Get scales
  xscl <- get_scale(pdt, .05)
  yscl <- get_zero_scale(rdt, .05)

  # Generate plot
  plot(pdt, rdt,
       main = paste0("Residual by Predicted for ", dvr),
       xlab = "",
       ylab = "Residual",
       pch  = 1,          # open circles
       cex = 1.3,
       col  = "#05379B",
       xlim = xscl,
       ylim = yscl,
       axes = FALSE
  )

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # Generate zero line
  abline(h = 0, col = "grey60", lwd = 1)

  # Add bottom label
  mtext("Predicted Value", side = 1, line = par("mar")[1] - 2)

  # Frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  # Restore margins
  par(mar = op)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}


render_rstudentbypredicted <- function(dat, res, mdl, plt) {

  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Prepare data
  fit <- lm(mdl, data = dat)
  rdt <- rstudent(fit)
  pdt <- fitted(fit)

  # Set margins
  par(mar = c(4, 5, 2, .75) + 0.1)

  # Get scales
  xscl <- get_scale(pdt, .05)
  yscl <- get_zero_scale(rdt, .1)

  # Generate plot
  plot(pdt, rdt,
       main = paste0("RStudent by Predicted for ", dvr),
       xlab = "",
       ylab = "RStudent",
       pch  = 1,          # open circles
       cex = 1.3,
       col  = "#05379B",
       xlim = xscl,
       ylim = yscl,
       axes = FALSE
  )

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # Generate reference lines
  abline(h =  2, col = "grey60", lwd = 1)
  abline(h = -2, col = "grey60", lwd = 1)

  # Add bottom label
  mtext("Predicted Value", side = 1, line = par("mar")[1] - 2)

  # Frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  # Add labels
  if (plt$label) {

    # Identify points that need labels
    lblids <- ifelse(rdt > 2 | rdt < -2, TRUE, FALSE)
    if (!is.null(plt$id)) {
      if (!plt$id %in% names(dat)) {
        stop("Label ID variable '", plt$id, "' not found in input dataset.")
      }

      lbls <- dat[[plt$id]][lblids]
    } else {
      lbls <- seq(1, length(rdt))[lblids]
    }

    # Filter data
    lrdt <- rdt[lblids]
    lpdt <- pdt[lblids]

    # Filter colors
    # lcol <- cols[cols != nc]

    # Assign labels
    text(lpdt, lrdt, labels = lbls, pos = 3)
  }

  # Restore margins
  par(mar = op)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}



render_rstudentbyleverage <- function(dat, res, mdl, plt) {

  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Prepare data
  fit <- lm(mdl, data = dat)
  rdt <- rstudent(fit)
  ldt <- hatvalues(fit)

  n <- length(ldt)
  p <- length(coef(fit))      # includes intercept

  # PROC REG-style cutoff
  cut <- 2 * p / n

  # Set margins
  par(mar = c(6, 5, 2, .75) + 0.1)

  # Get scales
  xscl <- c(min(ldt) * .9, max(ldt, cut) * 1.10)
  yscl <- get_zero_scale(rdt, .1)

  # Define colors
  nc <- "#05379B"   # Normal color
  lvc <- "#01665E"  # Leverage color
  otc <- "#A23A2E"  # Outlier color
  olc <- "#543005"  # Outlier and leverage

  # Assign colors
  cols <- rep(nc, length(ldt))
  cols <- ifelse(ldt > cut, lvc, cols)
  cols <- ifelse(rdt > 2 | rdt < -2, otc, cols)
  cols <- ifelse((rdt > 2 | rdt < -2) & ldt > cut, olc, cols)

  # Generate plot
  plot(ldt, rdt,
       main = paste0("Outlier and Leverage Diagnostics for ", dvr),
       xlab = "",
       ylab = "RStudent",
       pch  = 1,          # open circles
       cex = 1.3,
       col  = cols,
       xlim = xscl,
       ylim = yscl,
       axes = FALSE
  )

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # Generate reference lines
  abline(h =  2, col = "grey60", lwd = 1)
  abline(h = -2, col = "grey60", lwd = 1)
  abline(v = cut, col = "grey60", lwd = 1)  # Cutoff

  # Add bottom label
  mtext("Leverage", side = 1, line = par("mar")[1] - 4)

  # Add labels
  if (plt$label) {

    # Identify points that need labels
    lblids <- ifelse((rdt > 2 | rdt < -2) | ldt > cut, TRUE, FALSE)
    if (!is.null(plt$id)) {
      if (!plt$id %in% names(dat)) {
        stop("Label ID variable '", plt$id, "' not found in input dataset.")
      }

      lbls <- dat[[plt$id]][lblids]
    } else {
      lbls <- seq(1, length(rdt))[lblids]
    }

    # Filter data
    lrdt <- rdt[lblids]
    lldt <- ldt[lblids]

    # Filter colors
    lcol <- cols[cols != nc]

    # Assign labels
    text(lldt, lrdt, labels = lbls, pos = 3, col = lcol)
  }

  # Frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  # Legend (bottom, SAS-style)
  legend("bottom",
         legend = c("Outlier ",
                    "Leverage",
                    "Outlier and Leverage"),
         pch = c(1, 1, 1),
         pt.cex = 1.3,
         col = c(otc,
                 lvc,
                 olc),
         horiz = TRUE,
         bty = "o",
         box.col = "grey", # Grey border to match SAS
         x.intersp = 1,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         text.width = NA,  # Compute label widths dynamically
         inset = c(0, -0.23),
         xpd = TRUE)

  # Restore margins
  par(mar = op)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}

# Kill if more than 1 independent variable.
# Kill stats if "none" parameter is passed.
#' @noRd
render_fitplot <- function(dat, res, mdl, plt, alph) {

  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  if (length(ivr) == 1) {

    # Output to image file
    # All output types accept jpeg
    # So start with that
    jpeg(pth, width = wd, height = ht, quality = 100,  units = "px")

    # Set Confidence limits
    alph1 <- 1 - alph
    alph2 <- alph1 * 100

    # Fit model
    fit <- lm(mdl, data = dat)

    # Prediction grid
    xnew <- seq(min(dat[[ivr]]), max(dat[[ivr]]), length.out = 200)

    # Create data frame
    df1 <- data.frame(xnew)
    names(df1) <- ivr

    conf <- predict(fit,
                    newdata = df1,
                    interval = "confidence", level = alph1)

    pred <- predict(fit,
                    newdata = df1,
                    interval = "prediction", level = alph1)

    # Set margins
    if ("none" %in% plt$stats) {
      par(mar = c(5, 4.5, 2, .75) + 0.1)
    } else {
      par(mar = c(5, 4.5, 2, 8.5) + 0.1)
    }

    # Plot
    plot(dat[[ivr]], dat[[dvr]],
         main = paste0("Fit Plot for ", dvr),
         xlab = "",
         ylab = dvr,
         pch  = 1,
         cex = 1.23,
         col  = "#05379B",
         ylim = range(pred),
         axes = FALSE)

    # X Axis label
    mtext(ivr, side = 1, line = par("mar")[1] - 3.25)

    # Add custom axes
    axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
    axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

    # 95% confidence band (shaded)
    polygon(c(xnew, rev(xnew)),
            c(conf[, "lwr"], rev(conf[, "upr"])),
            col = adjustcolor("steelblue", alpha.f = 0.30),
            border = NA)

    # Fitted line
    lines(xnew, conf[, "fit"],
          col = "steelblue4",
          lwd = 2)

    # 95% prediction limits (dashed)
    lines(xnew, pred[, "lwr"],
          col = "steelblue3",
          lty = 2)

    lines(xnew, pred[, "upr"],
          col = "steelblue3",
          lty = 2)

    # Legend (bottom, SAS-style)
    legend("bottom",
           legend = c("Fit ",
                      paste0(alph2, "% Confidence Limits  "),
                      paste0(alph2, "% Prediction Limits")),
           lwd = c(2, NA, 1),
           lty = c(1, NA, 2),
           pch = c(NA, 15, NA),
           pt.cex = 2,
           col = c("steelblue4",
                   adjustcolor("steelblue", 0.30),
                   "steelblue3"),
           horiz = TRUE,
           bty = "o",
           box.col = "grey", # Grey border to match SAS
           x.intersp = .7,  # Spacing between group label and box
           y.intersp = 0,  # Spacing between content and borders
           text.width = NA,  # Compute label widths dynamically
           inset = c(0, -0.19),
           xpd = TRUE)

    # Frame
    box(col = "grey70", lwd = 1)
    box("figure", col = "grey70", lwd = 1)

    if (!"none" %in% plt$stats) {
      # Gather statistics for box
      sts <- collect_stats(res, plt)

      if ("aic" %in% plt$stats) {
        aicv <- round(extractAIC(fit)[2], 4)
        sts <- append(sts, aicv)
        names(sts)[length(sts)] <- "AIC"
      }

      # Create box
      draw_stats_box(sts, x_frac = c(0.015, 0.3))
    }

    # Restore margins
    par(mar = op)

    # Close device context
    dev.off()

    # Put plot in reporter plot object
    ret <- create_plot(pth, height = hti, width = wdi)

  } else {

    ret <- NULL
  }

  return(ret)

}

#' @noRd
render_residualhistogram <- function(dat, res, mdl, plt) {

  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Prepare data
  rdt <- res$OutputStatistics$RESID
  # dt <- dat[[ivr]]

  # Set margins
  par(mar = c(4, 5, 2, .75) + 0.1)

  # Calculate stats
  n   <- length(rdt)
  mu  <- mean(rdt)
  sdx <- sd(rdt)

  # Histogram (Percent scale)
  h <- hist(rdt,
            breaks = "Sturges",
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Use calculated breaks to get scale
  scl <- get_reg_xlim(h$breaks, mu, sdx, pad_frac = 0, max_sigma = 4)

  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = paste0("Distribution of Residuals for ", dvr),
       xlab = "",
       ylab = "Percent",
       xlim = scl,
       ylim = c(0, max(h$counts) * 1.05),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  x <- seq(min(rdt) * 2, max(rdt) * 2, length.out = 300)

  y_norm <- dnorm(x, mean = mu, sd = sdx)
  y_norm <- y_norm / max(y_norm) * max(h$counts)

  lines(x, y_norm, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  dens <- density(rdt)

  y_kern <- dens$y / max(dens$y) * max(h$counts)

  lines(dens$x, y_kern, col = "orangered2", lwd = 2)

  # Add x axis label
  mtext("Residual", side = 1, line = par("mar")[1] - 2)

  # Calculate x axis ticks
  df <- abs(h$mids[1] - h$mids[2])
  te <- (df * seq(1, 10)) + h$mids[length(h$mids)]
  be <- (-(df * seq(10, 1))) + h$mids[1]
  df <-  c(be, h$mids, te)
  xtks <- df[df > scl[1] & df < scl[2]]

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", at = xtks, labels = TRUE,
       mgp = c(3, .5, 0), tck = -0.015, cex.axis = .75)
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  # Legend
  mpos <- legend("topright",
         legend = c("Normal", "Kernel"),
         col = c("steelblue4", "orangered2"),
         lwd = 2,
         box.col = "grey80",
         bty = "n",
         cex = .9,
         inset = c(.01, .01),
         x.intersp = .5,
         y.intersp = c(0.5, 1.8)
         )

  rect(mpos$rect$left, mpos$rect$top - (mpos$rect$h - .5),
       mpos$rect$left + mpos$rect$w, mpos$rect$top,
       border = "grey80"
       )

  # Restore margins
  par(mar = op)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}


#' @noRd
render_qqplot <- function(dat, res, mdl) {

  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Prepare data
  rdt <- res$OutputStatistics$RESID

  # Set margins
  par(mar = c(4, 5, 2, .75) + 0.1)

  # Get y scale
 mx <- max(abs(rdt)) * 1.125  #1.0025
 scl <- c(-mx, mx)

 # Draw plot
  qqnorm(rdt,
         main = paste0("Q-Q Plot of Residuals for ", dvr),
         xlab = "",
         ylab = "Residual",
         pch  = 1,          # open circles
         col  = "#05379B",
         cex = 1.3,
         ylim = scl,
         axes = FALSE)

  # Add diagonal line
  usr <- par("usr")
  segments(usr[1], usr[3], usr[2], usr[4], col = "grey60")

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # X Axis label
  mtext("Quantile", side = 1, line = par("mar")[1] - 2)

  # Frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  par(mar = op)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}


render_rfplot <- function(dat, res, mdl) {

  # res <- proc_reg(cls,
  #                 model = "Weight = Height",
  #                 output = report,
  #                 plots = regplot(type = "qqplot"),
  #                 stats = p)


  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Prepare data
  rdt <- res$OutputStatistics$RESID
  fdt <- res$OutputStatistics$PREVAL
  # dt <- dat[[ivr]]

  # Set margins
  par(mar = c(5, 5, 2, .75) + 0.1)

  ## Proportion less (SAS-style)
  n <- length(fdt)
  p <- (seq_len(n) - 0.5) / n

  ## Left panel: Fit Mean
  fit_centered <- sort(fdt - mean(fdt))

  ## Right panel: Residuals
  res_sorted <- sort(rdt)

  # Get y scale
  smin <- min(fit_centered)
  if (min(res_sorted) < smin) {
    smin <- min(res_sorted)
  }

  smax <- max(fit_centered)
  if (min(res_sorted) > smax) {
    smax <- max(res_sorted)
  }

  # Finalize y scale
  yscl <- c(smin, smax) * 1.05

  ## Plot layout
  par(mfrow = c(1, 2),
      mar = c(2, 0, 2, .75) + 0.1,
      oma = c(2, 4, 2, .25))

  ## Left: Fit Mean
  plot(p, fit_centered,
       pch = 1,
       col = "#05379B",
       xlab = "",
       ylab = "",
       ylim = yscl,
       main = "Fit-Mean",
       font.main = 1,
       cex.main = 1,
       axes = FALSE)

  box(col = "grey70", lwd = 1)

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", col = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", las = 1, col = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  ## Right: Residual
  plot(p, res_sorted,
       pch = 1,
       col = "#05379B",
       xlab = "",
       ylab = "",
       ylim = yscl,
       main = "Residual",
       font.main = 1,
       cex.main = 1,
       axes = FALSE)

  box(col = "grey70", lwd = 1)

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", col = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  ## Overall title
  mtext(paste0("Residual-Fit Spread Plot for ", dvr),
        outer = TRUE, cex = 1.25, font = 2)

  # X Axis Label
  mtext("Proportion Less", side = 1, outer = TRUE, cex = 1.1)


  # Frame
  box("outer", col = "grey70", lwd = 1)

  # Restore margins
  par(mar = op, mfrow = c(1, 1), oma = c(0, 0, 0, 0))

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

}


#' @noRd
render_cooksd <- function(dat, res, mdl, plt) {

  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Set margins
  par(mar = c(4, 5, 2, .75) + 0.1)

  # Calculate statistics
  fit <- lm(mdl, data = dat)
  cd <- cooks.distance(fit)
  xcd <- seq_along(cd)
  n  <- length(cd)
  p  <- length(coef(fit))   # includes intercept

  ## Calculate cutoff
  cutoff <- 4 / n

  plot(xcd, cd,
       type = "h",            # vertical spikes
       lwd  = 1,
       col  = "#05379B",
       xlab = "",
       ylab = "Cook's D",
       main = paste0("Cook's D for ", dvr),
       ylim = c(0, max(cd, cutoff) * 1.10),
       axes = FALSE)

  ## Open circles at the top of each spike
  points(seq_along(cd), cd,
         pch = 1,
         col = "#05379B", cex = 1.25)

  ## Reference line (PROC REG default)
  abline(h = cutoff, col = "grey60", lwd = 1)


  # Add custom axes
  axis(side = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Generate zero line
  abline(h = 0, col = "#05379B", lwd = 1)

  mtext("Observation", side = 1, line = par("mar")[1] - 2)

  # Frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  # Add labels
  if (plt$label) {

    # Identify points that need labels
    lblids <- ifelse(cd > cutoff, TRUE, FALSE)
    if (!is.null(plt$id)) {
      if (!plt$id %in% names(dat)) {
        stop("Label ID variable '", plt$id, "' not found in input dataset.")
      }

      lbls <- dat[[plt$id]][lblids]
    } else {
      lbls <- seq(1, length(cd))[lblids]
    }

    # Filter data
    lcd <- cd[lblids]
    lxcd <- xcd[lblids]

    # Filter colors
    # lcol <- cols[cols != nc]

    # Assign labels
    text(lxcd, lcd, labels = lbls, pos = 3)
  }

  # Restore margins
  par(mar = op)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}


render_observedbypredicted <- function(dat, res, mdl, plt) {

  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 4.5   # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Prepare data
  odt <- dat[[dvr]]
  pdt <- res$OutputStatistics$PREVAL

  # Set margins
  par(mar = c(4, 5, 2, 1.05) + 0.1)

  # Get scales
  # Appears to take min of predicted and observed
  # and max of predicted and observed, and uses
  # those for both x and y scales.  Scales have to be the same
  # or the dots won't line up properly.  Much trial and error
  # to figure this out.
  xscl <- get_scale(pdt, 0)
  yscl <- get_scale(odt, 0)
  scl <- c(min(xscl[1], yscl[1]), max(xscl[2], yscl[2]))

  # Generate plot
  plot(pdt, odt,
       main = paste0("Observed by Predicted for ", dvr),
       xlab = "",
       ylab = dvr,
       pch  = 1,          # open circles
       cex = 1.3,
       col  = "#05379B",
       xlim = scl,
       ylim = scl,
       axes = FALSE
  )

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", tck = -0.015, mgp = c(3, .5, 0))
  axis(side = 2, las = 1, col.ticks = "grey55", tck = -0.015, mgp = c(3, .5, 0))

  # Generate diagonal line
  # abline() doesn't work at all. Not sure why. Have to use segments().
  usr <- par("usr")
  segments(usr[1], usr[3], usr[2], usr[4], col = "grey60")

  # Add bottom label
  mtext("Predicted Value", side = 1, line = par("mar")[1] - 2)

  # Frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  # Would be nice to get outlier labels in here,
  # but I don't know how they are identified.
  # There are no limits on this chart.

  # Restore margins
  par(mar = op)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}

render_residualboxplot <- function(dat, res, mdl, plt) {

  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Prepare data
  rdt <- res$OutputStatistics$RESID
  pdt <- res$OutputStatistics$PREVAL

  # Set margins
  par(mar = c(.75, 5, 2, .75) + 0.1)

  # Get scales
  yscl <- get_zero_scale(rdt, .05)

  # Calculate SAS compatible quantile
  q <- quantile(rdt, probs = c(0, .25, .5, .75, 1), type = 2)

  # Prepare to pass data
  bp <- list(
    stats = matrix(q, ncol = 1),
    n = length(rdt),
    conf = NULL,
    out = numeric(0)
  )

  # Box plot
  bxp(bp,
      main = paste0("Residuals for ", dvr),
      ylab = "Residual",
      boxfill = "#CAD5E5",
      border = "grey40",
      outline = FALSE,
      lty = 1,
      ylim = yscl,
      whiskcol = "#05379B",
      staplecol = "#05379B",
      medcol = "#05379B",
      medlwd = 1,
      boxwex = .65,
      axes = FALSE)


  # Mean diamond
  points(1, mean(rdt),
         pch = 5,          # diamond
         col = "#05379B",
         cex = 1.3)

  # Add custom axis
  aval <- axis(side = 2, las = 1, col.ticks = "grey55",
               mgp = c(3, .5, 0), tck = -0.015)

  ## Optional: horizontal gridlines
  abline(h = aval, col = "grey90", lwd = 1)

  # Frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  # Restore margins
  par(mar = op)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}



# Utilities ---------------------------------------------------------------


#' @noRd
collect_stats <- function(res, plt) {


  # mres <- proc_reg(cls, Weight ~ Height, stats = "clb", output = report)

  ret <- c()

  if ("default" %in% plt$stats) {
    ret <- c(
      "Observations"  = res$NObs$NOBS[1],
      "Parameters"    = nrow(res$ParameterEstimates),
      "Error DF"      = res$ANOVA$DF[2],
      "MSE"           = roundup(res$ANOVA$MEANSQ[2], 2),
      "R-Square"      = roundup(res$FitStatistics$RSQ[1], 4),
      "Adj R-Square"  = roundup(res$FitStatistics$ADJRSQ[1], 4)
    )

  }

  # Apperently got from AI and these are wrong
  # obs, edf, rsquare, parms, mse, adjrsquare,
  # coefvar, depmean
  # sse, bic, gmsep, pc, sp, aic, cp, jp, sbc

  # From SAS error message
  # Documentation is not actually that good on how to do this.
  # ADJRSQ, AIC, BIC, COEFFVAR, CP, DEFAULT, DEPMEAN, EDF, GMSEP, JP,
  # MSE, NOBS, NPARM, PC, RSQUARE, SBC, SP, SSE

  if ("nobs" %in% plt$stats) {
    ret[length(ret) + 1] <- res$NObs$NOBS[1]
    names(ret)[length(ret)] <- "Observations"
  }

  if ("nparm" %in% plt$stats) {
    ret[length(ret) + 1] <- nrow(res$ParameterEstimates)
    names(ret)[length(ret)] <- "Parameters"
  }

  if ("edf" %in% plt$stats) {
    ret[length(ret) + 1] <- res$ANOVA$DF[2]
    names(ret)[length(ret)] <- "Error DF"
  }

  if ("mse" %in% plt$stats) {
    ret[length(ret) + 1] <- roundup(res$ANOVA$MEANSQ[2], 2)
    names(ret)[length(ret)] <- "MSE"
  }

  if ("rsquare" %in% plt$stats) {
    ret[length(ret) + 1] <- roundup(res$FitStatistics$RSQ[1], 4)
    names(ret)[length(ret)] <- "R-Square"
  }

  if ("adjrsq" %in% plt$stats) {
    ret[length(ret) + 1] <- roundup(res$FitStatistics$ADJRSQ[1], 4)
    names(ret)[length(ret)] <- "Adj R-Square"
  }

  if ("depmean" %in% plt$stats) {
    ret[length(ret) + 1] <- roundup(res$FitStatistics$DEPMEAN, 2)
    names(ret)[length(ret)] <- "Dep Mean"
  }

  if ("coeffvar" %in% plt$stats) {
    ret[length(ret) + 1] <- roundup(res$FitStatistics$COEFVAR, 4)
    names(ret)[length(ret)] <- "Coeff Var"
  }

  if ("sse" %in% plt$stats) {

    ret[length(ret) + 1] <- roundup(res$ANOVA$SUMSQ[2], 1)
    names(ret)[length(ret)] <- "SSE"
  }


  return(ret)
}

#' @noRd
draw_stats_box <- function(stats,
                           x_frac = c(0.03, 0.30),   # from right edge of plot (in x-range fractions)
                           cex = 0.85,
                           family = "sans",
                           box_col = "white",
                           border_col = "grey70",
                           pad_y = 0.01,            # vertical padding as fraction of y-range
                           pad_x = 0.01) {           # horizontal padding as fraction of x-range
  stopifnot(is.numeric(stats), !is.null(names(stats)))

  # allow drawing in the margin
  op <- par(xpd = NA)
  on.exit(par(op), add = TRUE)

  usr <- par("usr")  # c(x1, x2, y1, y2)
  x_rng <- diff(usr[1:2])
  y_rng <- diff(usr[3:4])

  # x positions (user sets; relative to right edge)
  x_left  <- usr[2] + x_frac[1] * x_rng
  x_right <- usr[2] + x_frac[2] * x_rng

  # compute line height in user units (based on current cex)
  old_cex <- par("cex")
  old_family <- par("family")
  par(cex = cex, family = family)
  on.exit(par(cex = old_cex, family = old_family), add = TRUE)

  line_h <- strheight("Mg")         # approx line height in user units
  gap_h  <- 0.35 * line_h           # gap between lines
  inner_h <- length(stats) * line_h + (length(stats) - 1) * gap_h

  # box padding in user units
  py <- pad_y * y_rng
  px <- pad_x * x_rng

  box_h <- inner_h + 2 * py

  # center vertically in plot area
  y_mid <- mean(usr[3:4])
  y_bot <- y_mid - box_h / 2
  y_top <- y_mid + box_h / 2

  # draw box
  rect(x_left, y_bot, x_right, y_top, col = box_col, border = border_col)

  # y positions for each row (top to bottom)
  y_start <- y_top - py - line_h / 2
  y_pos <- y_start - (seq_along(stats) - 1) * (line_h + gap_h)

  # left column: names
  text(x_left + px, y_pos,
       labels = names(stats),
       adj = c(0, 0.5))

  # right column: values
  text(x_right - px, y_pos,
       labels = unname(stats),
       adj = c(1, 0.5))
}

get_scale <- function(vct, pct = .1) {

  rng <- range(vct)

  mn <- rng[1] * (1 - pct)
  mx <- rng[2] * (1 + pct)

  ret <- c(mn, mx)

  return(ret)

}


get_zero_scale <- function(vct, pct = .1) {

  rng <- range(vct)

  mn <- abs(rng[1] * (1 + pct))
  mx <- abs(rng[2] * (1 + pct))

  tmx <- max(mn, mx)

  ret <- c(-tmx, tmx)

  return(ret)
}



# This function was mostly generated by AI and I don't fully understand it.
# But it does seem to work.
get_reg_xlim <- function(brks, mu, sdx,
                         pad_frac = 0.05,
                         normal_tail_frac = 0.001,  # normal at edges <= this * peak
                         max_sigma = 6) {

  # Get range of calculated breaks
  rng  <- range(brks)

  # Add adding on bin range
  pad0 <- pad_frac * diff(rng)
  xlim <- c(rng[1] - pad0, rng[2] + pad0)

  # Force symmetry around zero (expand, never shrink)
  m <- max(abs(xlim))
  xlim <- c(-m, m)

  if (!is.finite(sdx) || sdx <= 0) return(xlim)

  # Calculate peak of normal curve
  peak   <- stats::dnorm(mu, mean = mu, sd = sdx)
  target <- normal_tail_frac * peak

  # For symmetric limits, the "worst" edge is the one closer to mu.
  # Ensure BOTH edges are far enough into the tails.
  k <- 0
  while ((stats::dnorm(xlim[1], mu, sdx) > target ||
          stats::dnorm(xlim[2], mu, sdx) > target) &&
         k < max_sigma) {
    xlim <- xlim + c(-0.5 * sdx, 0.5 * sdx)  # symmetric expansion
    k <- k + 0.5
  }

  ret <- xlim - c(-0.5 * sdx, 0.5 * sdx)  # xlim[seq(2, length(xlim) - 1)]

  return(ret)
}

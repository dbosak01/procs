

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
# type = diagnostics/panel,  residuals, RStudent, leverage, quantile, qqplot, cooksd, distribution, proportion/spread, stats, fitplot
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
#' @param type The type(s) of plot to create. Multiple types should be passed
#' as a vector of strings.  Valid values are "diagnostics", "residuals",
#' "rstudent", "leverage", "quantile", "qqplot", "cooksd", "distribution",
#' "proportion", "fitplot".  The default value is
#' a vector with "diagnostics", "residuals", and "fitplot".  The "diagnostics"
#' keyword produces a single combined chart with 8 different plots and a
#' selection of statistics in a small table.
#' @param panel Whether or not to display the diagnostics plots combined into
#' in a single panel.  Default is TRUE.  A value of FALSE will create
#' individual plots instead.  This parameter is equivalent to the
#' "unpack" keyword in SAS.
#' @param stats The statistics to display on the diagnostics panel.  The
#' default value is "default", which produces the following statistics:
#'
#' @examples
#' library(procs)
#'
#' # Turn off printing for CRAN checks
#' # Set to TRUE to run in local environment
#' options("procs.print" = FALSE)
#'
#' # Prepare sample data
#' dt <- as.data.frame(HairEyeColor, stringsAsFactors = FALSE)
#'
#' # Example 1: Frequency statistics with default plots
#' res <- proc_freq(dt, tables = v(Hair, Eye, Hair * Eye),
#'                  weight = Freq,
#'                  output = report,
#'                  plots = freqplot,
#'                  titles = "Hair and Eye Frequency Statistics")
#'
#' # View results
#' res
#'
#' # Example 2: Frequency statistics with custom plots
#' res <- proc_freq(dt, tables = v(Hair, Eye, Hair * Eye),
#'                  weight = Freq,
#'                  output = report,
#'                  plots = list(freqplot(type = "barchart",
#'                                        orient = "horizontal"),
#'                               freqplot(type = "dotplot",
#'                                        scale = "percent"),
#'                               freqplot(type = "barchart",
#'                                        twoway = "cluster"),
#'                  titles = "Hair and Eye Frequency Statistics"))
#'
#' # View results
#' res
#'
#' @export
regplot <- function(type = c("diagnostics", "residuals", "fitplot"), panel = TRUE, stats = "default") {

  # Non-standard evaluation

  otype <- deparse(substitute(type, env = environment()))
  type <- tryCatch({if (typeof(type) %in% c("character", "NULL")) type else otype},
                   error = function(cond) {otype})

  ostats <- deparse(substitute(stats, env = environment()))
  stats <- tryCatch({if (typeof(stats) %in% c("character", "NULL")) stats else ostats},
                     error = function(cond) {ostats})


  # Parameter Checks
  if (!all(type %in% c("diagnostics", "residuals", "fitplot", 'qqplot', 'spreadplot'))) {
    stop(paste0("Parameter value for 'type' invalid. Valid values are ",
                "'diagnostics', 'residuals', 'fitplot', 'qqplot', 'spreadplot'."))
  }

  if (!panel %in% c(TRUE, FALSE)) {
    stop("Parameter value for 'panel' invalid. Valid values are TRUE or FALSE.")
  }

  if (!all(stats %in% c("default"))) {
    stop(paste0("Parameter value for 'stats' invalid. Valid values are 'default' ",
                "."))
  }

  # Create object
  ret <- structure(list(), class = c("regplot", "list"))

  ret$stats <- tolower(stats)
  ret$panel <- panel
  ret$type <- tolower(type)

  return(ret)
}



# Plot Rendering ----------------------------------------------------------


#' @noRd
render_regplot <- function (dat, res, mdl, plt) {

  ret <- NULL

  if (is.character(plt)) {
    if (plt == "regplot") {
      plt <- regplot()
    }
  }


  if ("regplot" %in% class(plt)) {

    typs <- plt$type

    # "Unpack" logic
    if ("diagnostics" %in% typs & plt$panel == FALSE) {

      pos <- match("diagnostics", typs)

      vins <- c("residuals", "qqplot", "spreadplot")

      nv <- append(typs, vins, after = pos)

      typs <- nv[-pos]
    }

    ret <- list()

    for (tp in typs) {

      if (tp == "diagnostics") {
        ret[["diagnostics"]] <- render_diagnostics(dat, res, mdl)
      } else if (tp == "residuals") {
        ret[["residuals"]] <- render_residuals(dat, res, mdl)
      } else if (tp == "fitplot") {
        ret[["fitplot"]] <- render_fitplot(dat, res, mdl, plt)
      } else if (tp == "qqplot") {
        ret[["qqplot"]] <- render_qqplot(dat, res, mdl)
      } else if (tp == "spreadplot") {
        ret[["spreadplot"]] <- render_spreadplot(dat, res, mdl)
      }
    }

    if (length(ret) == 0) {
      ret <- NULL
    }
  }

  return(ret)
}


#' @noRd
render_diagnostics <- function(dat, res, mdl) {


}


# What is there are multiple independent variables?
#' @noRd
render_residuals <- function(dat, res, mdl) {

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
  dt <- dat[[ivr]]

  # Set margins
  par(mar = c(5, 5, 2, .75) + 0.1)

  # Get y scale
  # mx <- max(rdt) * 1.05
  # scl <- c(-mx, mx)
  scl <- range(rdt) * 1.1

  # Generate plot
  plot(dt, rdt,
       main = paste("Residuals for ", dvr),
       xlab = ivr,
       ylab = "Residual",

       pch  = 1,          # open circles
       cex = 1.3,
       col  = "#05379B",
       ylim = scl,
       axes = FALSE
       )

  # Add custom axes
  axis(side = 1, col.ticks = "grey55")
  axis(side = 2, las = 1, col.ticks = "grey55")

  # Generate zero line
  abline(h = 0, col = "grey60", lwd = 1)

  # # frame
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


# Kill if more than 1 independent variable
#' @noRd
render_fitplot <- function(dat, res, mdl, plt) {

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

    # Fit model
    fit <- lm(mdl, data = dat)

    # Prediction grid
    xnew <- seq(min(dat[[ivr]]), max(dat[[ivr]]), length.out = 200)

    # Create data frame
    df1 <- data.frame(xnew)
    names(df1) <- ivr

    conf <- predict(fit,
                    newdata = df1,
                    interval = "confidence")

    pred <- predict(fit,
                    newdata = df1,
                    interval = "prediction")

    # Plot
    par(mar = c(5, 4.5, 2, 8.5) + 0.1)

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
                      "95% Confidence Limits  ",
                      "95% Prediction Limits"),
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

    # # frame
    box(col = "grey70", lwd = 1)
    box("figure", col = "grey70", lwd = 1)

    # Gather statistics for box
    sts <- collect_stats(res, plt)

    # Create box
    draw_stats_box(sts, x_frac = c(0.015, 0.3))

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

#' @noRd
render_distribution <- function() {


}

#' @noRd
render_rstudent <- function() {


}

#' @noRd
render_leverage <- function() {


}

#' @noRd
render_quantile <- function() {


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
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  vrs <- get_vars(mdl)
  dvr <- vrs$dvar
  ivr <- vrs$ivar

  # Prepare data
  rdt <- res$OutputStatistics$RESID
  dt <- dat[[ivr]]

  # Set margins
  par(mar = c(5, 5, 2, .75) + 0.1)

  # Get y scale
 # mx <- max(rdt) * 1.1
 # scl <- c(-mx, mx)
  scl <- range(rdt) * 1.1
  # Generate plot
  # plot(dt, rdt,
  #      main = paste("Residuals for ", dvr),
  #      xlab = ivr,
  #      ylab = "Residual",
  #
  #      pch  = 1,          # open circles
  #      cex = 1.3,
  #      col  = "#05379B",
  #      ylim = scl,
  #      axes = FALSE
  # )


  # res <- resid(fit)

  par(mar = c(5, 5, 2, 2) + 0.1)

  qqnorm(rdt,
         main = paste0("Q-Q Plot of Residuals for ", dvr),
         xlab = "Quantile",
         ylab = "Residual",
         pch  = 1,          # open circles
         col  = "#05379B",
         cex = 1.2,
         ylim = scl,
         axes = FALSE)

  qqline(rdt, col = "grey60", lwd = 1, qtype = 3)



  # Add custom axes
  axis(side = 1, col.ticks = "grey55")
  axis(side = 2, las = 1, col.ticks = "grey55")

  # Generate zero line
  # abline(h = 0, col = "grey60", lwd = 1)

  # # frame
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


render_spreadplot <- function(dat, res, mdl) {

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
  dt <- dat[[ivr]]

  # Set margins
  par(mar = c(5, 5, 2, .75) + 0.1)


  ## Assumes:
  ## fit <- lm(Weight ~ Height, data = dat)

  # fitted_vals <- fitted(fit)
  # resid_vals  <- resid(fit)

  n <- length(fdt)

  ## Proportion less (SAS-style)
  p <- (seq_len(n) - 0.5) / n

  ## Left panel: Fit − Mean
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

  ## Left: Fit − Mean
  plot(p, fit_centered,
       pch = 1,
       col = "blue3",
       xlab = "",
       ylab = "",
       ylim = yscl,
       main = "Fit–Mean",
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
       col = "blue3",
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

  mtext("Proportion Less", side = 1, outer = TRUE, cex = 1.1)


  # # frame
  box("outer", col = "grey70", lwd = 1)

  # Restore margins
  par(mar = op, mfrow = c(1, 1), oma = c(0, 0, 0, 0))

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

}




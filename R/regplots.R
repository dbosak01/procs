

# Regplots Main Function --------------------------------------------------



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
  if (!all(type %in% c("diagnostics", "residuals", "fitplot"))) {
    stop(paste0("Parameter value for 'type' invalid. Valid values are ",
                "'diagnostics', 'residuals', or 'fitplot'."))
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

    ret <- list()

    for (tp in plt$type) {

      if (tp == "diagnostics") {
        ret[["diagnostics"]] <- render_diagnostics(dat, res, mdl)
      } else if (tp == "residuals") {
        ret[["residuals"]] <- render_residuals(dat, res, mdl)
      } else if (tp == "fitplot") {
        ret[["fitplot"]] <- render_fitplot(dat, res, mdl)
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
  mx <- max(rdt) * 1.05
  scl <- c(-mx, mx)

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
render_fitplot <- function(dat, res, mdl) {

  # mres <- proc_reg(cls, Weight ~ Height, stats = "clb", output = report)

  # lfit(

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
    jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

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
    par(mar = c(6, 5, 2, 8) + 0.1)

    plot(dat[[ivr]], dat[[dvr]],
         main = paste0("Fit Plot for ", dvr),
         xlab = "",
         ylab = dvr,
         pch  = 1,
         col  = "#05379B",
         ylim = range(pred),
         axes = FALSE)

    # X Axis label
    mtext(ivr, side = 1, line = par("mar")[1] - 3.5)

    # Add custom axes
    axis(side = 1, col.ticks = "grey55")
    axis(side = 2, las = 1, col.ticks = "grey55")

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
           inset = c(0, -0.25),
           xpd = TRUE)

    # # frame
    box(col = "grey70", lwd = 1)
    box("figure", col = "grey70", lwd = 1)

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
render_qqplot <- function() {



}


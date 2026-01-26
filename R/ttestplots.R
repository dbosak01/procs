

# TTestplots Main Function --------------------------------------------------


# AGREEMENT, ALL, BOX, HISTOGRAM, INTERVAL, NONE, PROFILES, QQ, SUMMARY
# agreement, boxplot, histogram, interval, profiles, qqplot, summary

#' @title Request T-Test Plots
#' @description A function to request T-test plots on a call to
#' \code{\link{proc_ttest}}. The function allows you to specify the type of
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
#' as a vector of strings.  Valid values are "agreement", "boxplot", "histogram",
#' "interval", "profiles", "qqplot", "summary".  The default value is
#' a vector with "summary" and "qqplot".  The "summary"
#' keyword produces a single combined chart with 2 different plots: a histogram
#' and a box plot.
#' @param panel Whether or not to display the summary plot combined into
#' in a single panel.  Default is TRUE.  A value of FALSE will create
#' individual plots instead.  This parameter is equivalent to the
#' "unpack" keyword in SAS.
#' @param showh0 Whether or not to show the h0 line on a single-variable T-test.
#' Default is FALSE.
#' @examples
#' library(procs)
#' @export
ttestplot <- function(type = c("summary", "qqplot"), panel = TRUE, showh0 = FALSE) {

  # Non-standard evaluation
  otype <- deparse(substitute(type, env = environment()))
  type <- tryCatch({if (typeof(type) %in% c("character", "NULL")) type else otype},
                   error = function(cond) {otype})


  # agreement, boxplot, histogram, interval, profiles, qqplot, summary

  # Parameter Checks
  vldvals <- c("agreement", "boxplot", "histogram", 'interval', 'profiles',
               "qqplot", "summary")
  if (any(!type %in% vldvals)) {

    ivd <- type[!type %in% vldvals]
    stop(paste0("Parameter value for 'type' invalid: ", paste0("'", ivd, "'", collapse = ", "),
                "\nValid values are: ",
                "'agreement', 'boxplot', 'histogram', 'interval', 'profiles', ",
                "'qqplot', 'summary'."
    ))
  }

  if (!panel %in% c(TRUE, FALSE)) {
    stop("Parameter value for 'panel' invalid. Valid values are TRUE or FALSE.")
  }

  if (!showh0 %in% c(TRUE, FALSE)) {
    stop("Parameter value for 'showh0' invalid. Valid values are TRUE or FALSE.")
  }


  # Create object
  ret <- structure(list(), class = c("ttestplot", "list"))

  ret$type <- tolower(type)
  ret$panel <- panel
  ret$showh0 <- showh0
  ret$alph <- 0.95
  ret$h0 <- 0

  return(ret)
}



# Plot Rendering ----------------------------------------------------------


#' @noRd
render_ttestplot <- function (dat, var, plt, class) {

  ret <- NULL

  if (is.character(plt)) {
    if (all(plt == "ttestplot")) {
      plt <- ttestplot()
    } else {
      plt <- ttestplot(type = plt)
    }
  }

  if ("ttestplot" %in% class(plt)) {

    typs <- plt$type

    # "Unpack" logic
    if ("summary" %in% typs & plt$panel == FALSE) {

      # Find and replace
      pos <- match("summary", typs)

      vins <- c("histogram", "boxplot")

      nv <- append(typs, vins, after = pos)

      typs <- nv[-pos]
    }

    ret <- list()

    # agreement, boxplot, histogram, interval, profiles, qqplot, summary

    for (tp in typs) {

      if (tp == "summary") {
        ret[["summary"]] <- render_summary(dat, var, plt)
      } else if (tp == "histogram") {
        ret[["histogram"]] <- render_histogram(dat, var, plt)
      } else if (tp == "boxplot") {
        if (is.null(class)) {
          ret[["boxplot"]] <- render_boxplot1(dat, var, plt)
        } else {
          ret[["boxplot"]] <- render_boxplot2(dat, var, plt, class)
        }
      } else if (tp == "qqplot") {
        ret[["qqplot"]] <- render_tqqplot(dat, var, plt)
      } else if (tp == "interval") {
        ret[["interval"]] <- render_interval(dat, var, plt)
      } else if (tp == "profiles") {
        ret[["profiles"]] <- render_profiles(dat, var, plt)
      } else if (tp == "agreement") {
        ret[["agreement"]] <- render_agreement(dat, var, plt)
      }
    }

    if (length(ret) == 0) {
      ret <- NULL
    }
  }

  return(ret)
}


# Summary Plot ------------------------------------------------------------


# unpack: TRUE or FALSE
#' @noRd
render_summary <- function(dat, var, plt) {


  op <- par("mar")
  of <- par("fig")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines
  alph <- (1 - plt$alph) * 100  # Percentage
  alpha <- plt$alph             # Actual value

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Get analysis variables
  # vrs <- get_vars(mdl)
  # dvr <- vrs$dvar
  # ivr <- vrs$ivar

  # Prepare data
  rdt <- dat[[var]]
  # dt <- dat[[ivr]]

  #**********************************
  #* Histogram
  #**********************************

  # Set margins
  par(mar = c(0, 5, 3, .75) + 0.1,
      fig = c(0, 1, .3, 1))

  # Calculate stats
  n   <- length(rdt)
  mu  <- mean(rdt)
  sdx <- sd(rdt)

  # Use calculated breaks to get scale
  scl <- range(rdt)
  scl <- c(scl[1] * .8, scl[2] * 1.2)

  # Calculate breaks and y scale
  h <- hist(rdt,
            breaks = "Sturges",
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Initial chart to draw vertical lines
  hist(rdt,
       breaks = "Sturges",
       main = "",
       ylab = "",
       xlim = scl,
       ylim = c(0, max(h$counts) * 1.05),
       plot = TRUE,
       axes = FALSE)

  # Get axis tick marks
  aval <- axTicks(side = 1)

  # Orientation lines
  abline(v = aval, col = "grey90", lwd = 1)

  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = "",
       xlab = "",
       ylab = "",
       xlim = scl,
       ylim = c(0, max(h$counts) * 1.05),
       axes = FALSE,
       add = TRUE)

  # Title
  mtext(paste0("Distribution of ", var), side = 3,
        line = par("mar")[3] - 1.5,
        font = 2, cex = 1.25)

  # Subtitle
  mtext(paste0("With ", alph, "% Confidence Interval for Mean"), side = 3,
        line = par("mar")[3] - 2.5,
        font = 1)

  # Y Label
  mtext("Percent", side = 2, line = par("mar")[2] - 2)


  # Normal curve overlay (scaled to percent)
  x <- seq(min(rdt) * 2, max(rdt) * 2, length.out = 300)
  x <- seq(scl[1], scl[2], length.out = 300)

  y_norm <- dnorm(x, mean = mu, sd = sdx)
  y_norm <- y_norm / max(y_norm) * max(h$counts)

  lines(x, y_norm, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  dens <- density(rdt)

  y_kern <- dens$y / max(dens$y) * max(h$counts)

  lines(dens$x, y_kern, col = "orangered2", lwd = 2)


  # Add custom Y axis
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Frame
  box(col = "grey70", lwd = 1)
  box("outer", col = "grey70", lwd = 1)

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

  # Custom Legend border
  rect(mpos$rect$left, mpos$rect$top - (mpos$rect$h - .5),
       mpos$rect$left + mpos$rect$w, mpos$rect$top,
       border = "grey80"
  )

  #**********************************
  #* Boxplot
  #**********************************

  par(mar = c(4, 5, 0, .75) + 0.1,
      fig = c(0, 1, 0, .3), new = TRUE)

  # Get data
  dt <- dat[[var]]

  # Get scales
  xscl <- get_scale(dt, .05)  # Not used?

  # Calculations
  n  <- length(dt)
  mu <- mean(dt)
  sdx <- sd(dt)

  ## 95% CI for mean (SAS uses t-based CI)
  tcrit <- qt(1 - alpha / 2, df = n - 1)

  # Calculate confidence interval
  ci <- mu + c(-1, 1) * tcrit * sdx / sqrt(n)


  ## Draw empty plot first (for layering)
  plot(dt, rep(1, n),
       type = "n",
       xlab = "",
       ylab = "",
       yaxt = "n",
       xlim = scl,
       main = "",
       axes = FALSE) #With 95% Confidence Interval for Mean")


  # X Label
  mtext(var, side = 1,
        line = par("mar")[1] - 2,
        font = 1)

  ## Shaded CI band
  usr <- par("usr")
  rect(ci[1], usr[3], ci[2], usr[4],
       col = "#B3D2D0",
       border = NA)

  # Draw axis
  aval <- axis(side = 1, las = 1, col.ticks = "grey55",
               mgp = c(3, .5, 0), tck = -0.015)

  # Orientation lines
  abline(v = aval, col = "grey90", lwd = 1)

  # Calculate SAS compatible quantile
  q <- quantile(dt, probs = c(0, .25, .5, .75, 1), type = 2)

  # Prepare to pass data
  bp <- list(
    stats = matrix(q, ncol = 1),
    n = length(dt),
    conf = NULL,
    out = numeric(0)
  )

  # Boxplot (horizontal)
  bxp(bp,
      horizontal = TRUE,
      add = TRUE,
      boxfill = "#CAD5E5",
      border = "grey40",
      lty = 1,
      whiskcol = "#05379B",
      staplecol = "#05379B",
      medcol = "#05379B",
      medlwd = 1,
      boxwex = .65,
      outline = FALSE,
      axes = FALSE)  # Already drawn above

  ## Mean diamond
  points(mu, 1,
         pch = 5,
         col = "#05379B",
         lwd = 1,
         cex = 1.25)

  # Create legend
  legend("topright",
         legend = paste0(alph, "% Confidence"),
         fill = "#B3D2D0",
         border = "grey60",
         box.col = "grey80",
         inset = c(.01, .02),
         x.intersp = .5,
         y.intersp = c(0.5, 1.8),
         cex = .9,
         bty = "o")

  # Frame
  box(col = "grey70", lwd = 1)

  #**********************************
  #* Clean up
  #**********************************

  # Restore margins
  par(mar = op, fig = of)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)


}


# Individual Plots --------------------------------------------------------

# agreement, boxplot, histogram, interval, profiles, qqplot, summary

#' @noRd
render_histogram <- function(dat, var, plt) {


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
  # vrs <- get_vars(mdl)
  # dvr <- vrs$dvar
  # ivr <- vrs$ivar

  # Prepare data
  rdt <- dat[[var]]
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
  # scl <- get_reg_xlim(h$breaks, mu, sdx, pad_frac = 0, max_sigma = 4)
  # mx <- max(abs(rdt))
  scl <- range(rdt)
  scl <- c(scl[1] * .8, scl[2] * 1.2)

  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = paste0("Distribution of ", var),
       xlab = "",
       ylab = "Percent",
       xlim = scl,
       ylim = c(0, max(h$counts) * 1.05),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  x <- seq(min(rdt) * 2, max(rdt) * 2, length.out = 300)
  x <- seq(scl[1], scl[2], length.out = 300)

  y_norm <- dnorm(x, mean = mu, sd = sdx)
  y_norm <- y_norm / max(y_norm) * max(h$counts)

  lines(x, y_norm, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  dens <- density(rdt)

  y_kern <- dens$y / max(dens$y) * max(h$counts)

  lines(dens$x, y_kern, col = "orangered2", lwd = 2)

  # Add x axis label
  mtext(var, side = 1, line = par("mar")[1] - 2)

  # Calculate x axis ticks
  # df <- abs(h$mids[1] - h$mids[2])
  # te <- (df * seq(1, 10)) + h$mids[length(h$mids)]
  # be <- (-(df * seq(10, 1))) + h$mids[1]
  # df <-  c(be, h$mids, te)
  # xtks <- df[df > scl[1] & df < scl[2]]

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", # at = xtks, labels = TRUE,
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


# Need to deal with outliers
#' @noRd
render_boxplot1 <- function(dat, var, plt) {


  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 2.25 # 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines
  alph <- plt$alph * 100
  alpha <- 1 - plt$alph

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Prepare data
  dt <- dat[[var]]

  # Set margins
  par(mar = c(3, 1, 3, .75) + 0.1)

  # Get scales
  xscl <- get_scale(dt, .05)


  n  <- length(dt)
  mu <- mean(dt)
  sdx <- sd(dt)

  ## 95% CI for mean (SAS uses t-based CI)
  tcrit <- qt(1 - alpha / 2, df = n - 1)

  # Calculate confidence interval
  ci <- mu + c(-1, 1) * tcrit * sdx / sqrt(n)


  ## Draw empty plot first (for layering)
  plot(dt, rep(1, n),
       type = "n",
       xlab = "",
       ylab = "",
       yaxt = "n",
       main = "",
       axes = FALSE) #With 95% Confidence Interval for Mean")

  # Title
  mtext(paste0("Distribution of ", var), side = 3,
        line = par("mar")[3] - 1.5,
        font = 2, cex = 1.25)

  # Subtitle
  mtext(paste0("With ", alph, "% Confidence Interval for Mean"), side = 3,
        line = par("mar")[3] - 2.5,
        font = 1)

  # X Label
  mtext(var, side = 1,
        line = par("mar")[1] - 1.5,
        font = 1)

  ## Shaded CI band
  usr <- par("usr")
  rect(ci[1], usr[3], ci[2], usr[4],
       col = "#B3D2D0",
       border = NA)

  # Draw axis
  aval <- axis(side = 1, las = 1, col.ticks = "grey55",
                       mgp = c(3, .5, 0), tck = -0.015)

  # Orientation lines
  abline(v = aval, col = "grey90", lwd = 1)

  # Calculate SAS compatible quantile
  q <- quantile(dt, probs = c(0, .25, .5, .75, 1), type = 2)

  # Prepare to pass data
  bp <- list(
    stats = matrix(q, ncol = 1),
    n = length(dt),
    conf = NULL,
    out = numeric(0)
  )

  # Boxplot (horizontal)
  bxp(bp,
          horizontal = TRUE,
          add = TRUE,
          boxfill = "#CAD5E5",
          border = "grey40",
          lty = 1,
          whiskcol = "#05379B",
          staplecol = "#05379B",
          medcol = "#05379B",
          medlwd = 1,
          boxwex = .65,
          outline = FALSE,
          axes = FALSE)  # Already drawn above

  ## Mean diamond
  points(mu, 1,
         pch = 5,
         col = "#05379B",
         lwd = 1,
         cex = 1.25)

  # Create legend
  legend("topright",
         legend = paste0(alph, "% Confidence"),
         fill = "#B3D2D0",
         border = "grey60",
         box.col = "grey80",
         inset = c(.01, .02),
         x.intersp = .5,
         y.intersp = c(0.5, 1.8),
         cex = .9,
         bty = "o")


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


# Need to deal with outliers
#' @noRd
render_boxplot2 <- function(dat, var, plt, class) {

  # browser()
  op <- par("mar")
  om <- par("oma")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 3.25 # 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines
  alph <- plt$alph * 100
  alpha <- 1 - plt$alph

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Convert to data frame just in case it is a tibble
  dat <- as.data.frame(dat)

  # Add rownumbers
  rownames(dat) <- seq(1, nrow(dat))

  # Get filter variable and values
  fvr <- "" # Filter variable
  fvls <- c() # Filter values
  if (!is.null(class)) {
    fvr <- class
    dat <- sort(dat, by = fvr)
    fvls <- unique(dat[[class]])
  } else {
    stop("Something is wrong")
  }

  # Set margins
  par(mar = c(4, 4, 2, .75) + 0.1)

  # Prepare data
  dt <- dat[[var]]
  rwnms <- rownames(dat)
  dt1 <- dat[dat[[fvr]] == fvls[1], var]
  nms1 <- rwnms[dat[[fvr]] == fvls[1]]
  dt2 <- dat[dat[[fvr]] == fvls[2], var]
  nms2 <- rwnms[dat[[fvr]] == fvls[2]]

  # Get x scale
  xscl <- get_scale(dt, .05)

  # Calculate
  n1  <- length(dt1)
  mu1 <- mean(dt1)
  sdx1 <- sd(dt1)
  n2  <- length(dt2)
  mu2 <- mean(dt2)
  sdx2 <- sd(dt2)

  # Draw empty plot first, so we can put ablines() behind the boxes
  plot(dt, rep(1, length(dt)),
       ylim = c(.5, 2.5),
       type = "n",
       xlab = "",
       ylab = "",
       yaxt = "n",
       main = "",
       axes = FALSE) #With 95% Confidence Interval for Mean")

  # Title
  mtext(paste0("Distribution of ", var), side = 3,
        line = par("mar")[3] - 1.5,
        font = 2, cex = 1.25)


  # X Label
  mtext(var, side = 1,
        line = par("mar")[1] - 2,
        font = 1)

  # Y Label
  mtext(fvr, side = 2,
        line = par("mar")[2] - 2,
        font = 1)


  # Draw X axis
  aval <- axis(side = 1, las = 1, col.ticks = "grey55",
               mgp = c(3, .5, 0), tck = -0.015)

  # # Draw Y axis
  axis(side = 2, las = 1, col.ticks = "grey55", labels = fvls,
       at = c(2, 1), mgp = c(3, .5, 0), tck = -0.015)


  # Orientation lines
  abline(v = aval, col = "grey90", lwd = 1)

  # Boxplot.stats(data1) can prepare the boxplot data more easily,
  # but has no parameter to use SAS-style quantiles. So need to calculate
  # everything manually.

  # bp <- boxplot_stats2(dt1, dt2, fvls)

  # Calculate quantile
  q1 <- quantile(dt1, probs = c(0, .25, .5, .75, 1), type = 2, na.rm = TRUE)
  q2 <- quantile(dt2, probs = c(0, .25, .5, .75, 1), type = 2, na.rm = TRUE)

  # Calculate Inter-Quartile Range
  iqr1 <- IQR(dt1, type = 2, na.rm = TRUE) * 1.5
  iqr2 <- IQR(dt2, type = 2, na.rm = TRUE) * 1.5

  # Calculate outlier limits
  lm1 <- c(q1[2] - iqr1, q1[4] + iqr1)
  lm2 <- c(q2[2] - iqr2, q2[4] + iqr2)

  # Determine if there are outliers
  ol1 <- dt1 <= lm1[1] | dt1 >= lm1[2]
  out1 <- dt1[ol1]
  ol2 <- dt2 <= lm2[1] | dt2 >= lm2[2]
  out2 <- dt2[ol2]

  # Change min and max to remove outliers
  if (length(out1) > 0) {
    mn1 <- min(dt1[dt1 > lm1[1]])
    mx1 <- max(dt1[dt1 < lm1[2]])
    q1[1] <- mn1
    q1[5] <- mx1
    onm1 <- seq(1, length(dt1))[ol1]   # Obs within class: This matches SAS
    # onm1 <- nms1[ol1]                # Original obs #: This seems more correct
  }

  # Change min and max to remove outliers
  if (length(out2) > 0) {
    mn2 <- min(dt1[dt2 > lm2[1]])
    mx2 <- max(dt1[dt2 < lm2[2]])
    q2[1] <- mn2
    q2[5] <- mx2
    onm2 <- seq(1, length(dt2))[ol2]   # Obs within class: This matches SAS
    # onm2 <- nms2[ol2]                # Original obs #: This seems more correct
  }

  # Prepare to pass data
  bp <- list(
    stats = cbind(q2, q1),
    n = cbind(length(dt), length(dt)),
    conf = NULL,
    out = numeric(0),
    names = fvls
  )

  # Boxplot (horizontal)
  bxp(bp,
      horizontal = TRUE,
      add = TRUE,
      boxfill = "#CAD5E5",
      border = "grey40",
      lty = 1,
      whiskcol = "#05379B",
      staplecol = "#05379B",
      medcol = "#05379B",
      medlwd = 1,
      show.names = TRUE,
      boxwex = .425,
      outline = FALSE,
      axes = FALSE)  # Already drawn above

  ## Mean diamond
  points(c(mu1, mu2), c(2, 1),
         pch = 5,
         col = "#05379B",
         lwd = 1,
         cex = 1.25)

  # Outliers for first class value
  if (length(out1) > 0) {
    points(out1, rep(2, length(out1)),
           pch = 1,
           col = "grey20",
           lwd = 1.6,
           cex = 1.25)

    # Add labels
    text(out1, rep(2, length(out1)),
         labels = onm1,
         cex = .9,
         pos = 1)
  }

  # Outliers for second class value
  if (length(out2) > 0) {
    # Plot outlier points
    points(out2, rep(1, length(out2)),
           pch = 1,
           col = "grey20",
           lwd = 1.6,
           cex = 1.25)
    # Add labels
    text(out2, rep(1, length(out2)),
         labels = onm2,
         cex = .9,
         pos = 1)
  }


  # Frame
  box(col = "grey70", lwd = 1)
  box("outer", col = "grey70", lwd = 1)

  # Restore margins
  par(mar = op, oma = om, mfrow = c(1, 1))

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}

# type: pergroup or period
#' @noRd
render_interval <- function(dat, var, plt) {


  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 2.75 # 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines
  alph <- plt$alph * 100
  alpha <- 1 - plt$alph

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Prepare data
  dt <- dat[[var]]

  # Set margins
  par(mar = c(3, 1, 3, .75) + 0.1)

  # Calculate basic parameters
  n  <- length(dt)
  mu <- mean(dt)
  sdx <- sd(dt)

  ## 95% CI for mean (SAS uses t-based CI)
  tcrit <- qt(1 - alpha / 2, df = n - 1)

  # Calculate confidence interval
  ci <- mu + c(-1, 1) * tcrit * sdx / sqrt(n)

  # Get scales
  xscl <- get_scale(ci, .001)

  # Create plot
  plot(ci, rep(1, length(ci)),
       type = "n",
       yaxt = "n",
       ylab = "",
       xlab = "",
       xlim = xscl,
       main = "",
       axes = FALSE)

  # Title
  mtext(paste0("Mean of ", var), side = 3,
        line = par("mar")[3] - 1.5,
        font = 2, cex = 1.25)

  # Subtitle
  mtext(paste0("With ", alph, "% Confidence Interval"), side = 3,
        line = par("mar")[3] - 2.5,
        font = 1)

  # X Label
  mtext(var, side = 1,
        line = par("mar")[1] - 1.5,
        font = 1)

  # Draw axis
  aval <- axis(side = 1, las = 1, col.ticks = "grey55",
               mgp = c(3, .5, 0), tck = -0.015)

  ## CI line
  segments(ci[1], 1, ci[2], 1,
           col = "firebrick3",
           lwd = 2)

  ## CI end caps
  segments(ci[1], 0.95, ci[1], 1.05,
           col = "firebrick3",
           lwd = 2)

  segments(ci[2], 0.95, ci[2], 1.05,
           col = "firebrick3",
           lwd = 2)

  ## Mean diamond
  points(mu, 1,
         pch = 5,
         col = "#05379B",
         cex = 1.25)

  # Create legend
  legend("topright",
         legend = "Mean",
         pch = 5,
         col = "#05379B",
         box.col = "grey80",
         inset = c(.01, .02),
         x.intersp = .5,
         y.intersp = c(0.5, 1.8),
         cex = .9,
         bty = "o")

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

#' @noRd
render_tqqplot <- function(dat, var, plt) {


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
  # vrs <- get_vars(mdl)
  # dvr <- vrs$dvar
  # ivr <- vrs$ivar

  # Prepare data
  rdt <- dat[[var]]

  # Set margins
  par(mar = c(4, 5, 2, .75) + 0.1)

  # Get y scale
  # mx <- max(abs(rdt)) * 1.125  #1.0025
  # scl <- c(-mx, mx)

  # Draw plot
  qqnorm(rdt,
         main = paste0("Q-Q Plot of ", var),
         xlab = "",
         ylab = var,
         pch  = 1,          # open circles
         col  = "#05379B",
         cex = 1.3,
        # ylim = scl,
         axes = FALSE)

  # Add diagonal line
  # This line is a problem.  Right now just putting it diagonal
  # between the corners.  All qqline() functions are worse.
  usr <- par("usr")
  segments(usr[1], usr[3], usr[2], usr[4], col = "grey60")
  # qqline(rdt, col = "grey60", lwd = 1, qtype = 3)

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

#' @noRd
render_profiles <- function(dat, var, plt) {



}

#' @noRd
render_agreement <- function(dat, var, plt) {



}



# Utilities ---------------------------------------------------------------


boxplot_stats1 <- function(dt1, dt2, fvls) {

  # Calculate quantile
  q1 <- quantile(dt1, probs = c(0, .25, .5, .75, 1), type = 2, na.rm = TRUE)
  q2 <- quantile(dt2, probs = c(0, .25, .5, .75, 1), type = 2, na.rm = TRUE)

  # Calculate Inter-Quartile Range
  iqr1 <- IQR(dt1, type = 2, na.rm = TRUE) * 1.5
  iqr2 <- IQR(dt2, type = 2, na.rm = TRUE) * 1.5

  # Calculate outlier limits
  lm1 <- c(q1[2] - iqr1, q1[4] + iqr1)
  lm2 <- c(q2[2] - iqr2, q2[4] + iqr2)

  # Determine if there are outliers
  ol1 <- dt1 <= lm1[1] | dt1 >= lm1[2]
  out1 <- dt1[ol1]
  ol2 <- dt2 <= lm2[1] | dt2 >= lm2[2]
  out2 <- dt2[ol2]

  # Change min and max to remove outliers
  if (length(out1) > 0) {
    mn1 <- min(dt1[dt1 > lm1[1]])
    mx1 <- max(dt1[dt1 < lm1[2]])
    q1[1] <- mn1
    q1[5] <- mx1
    onm1 <- seq(1, length(dt1))[ol1]   # Obs within class: This matches SAS
    # onm1 <- nms1[ol1]                # Original obs #: This seems more correct
  }

  # Change min and max to remove outliers
  if (length(out2) > 0) {
    mn2 <- min(dt1[dt2 > lm2[1]])
    mx2 <- max(dt1[dt2 < lm2[2]])
    q2[1] <- mn2
    q2[5] <- mx2
    onm2 <- seq(1, length(dt2))[ol2]   # Obs within class: This matches SAS
    # onm2 <- nms2[ol2]                # Original obs #: This seems more correct
  }

  # Get length
  lndt <- length(dt1) + length(dt2)

  # Prepare to pass data
  bp <- list(
    stats = cbind(q2, q1),
    n = cbind(lndt, lndt),
    conf = NULL,
    out = out1,
    names = fvls
  )

  return(bp)

}

boxplot_stats2 <- function(dt1, dt2, fvls) {

  # Calculate quantile
  q1 <- quantile(dt1, probs = c(0, .25, .5, .75, 1), type = 2, na.rm = TRUE)
  q2 <- quantile(dt2, probs = c(0, .25, .5, .75, 1), type = 2, na.rm = TRUE)

  # Calculate Inter-Quartile Range
  iqr1 <- IQR(dt1, type = 2, na.rm = TRUE) * 1.5
  iqr2 <- IQR(dt2, type = 2, na.rm = TRUE) * 1.5

  # Calculate outlier limits
  lm1 <- c(q1[2] - iqr1, q1[4] + iqr1)
  lm2 <- c(q2[2] - iqr2, q2[4] + iqr2)

  # Determine if there are outliers
  ol1 <- dt1 <= lm1[1] | dt1 >= lm1[2]
  out1 <- dt1[ol1]
  ol2 <- dt2 <= lm2[1] | dt2 >= lm2[2]
  out2 <- dt2[ol2]

  # Change min and max to remove outliers
  if (length(out1) > 0) {
    mn1 <- min(dt1[dt1 > lm1[1]])
    mx1 <- max(dt1[dt1 < lm1[2]])
    q1[1] <- mn1
    q1[5] <- mx1
    onm1 <- seq(1, length(dt1))[ol1]   # Obs within class: This matches SAS
    # onm1 <- nms1[ol1]                # Original obs #: This seems more correct
  }

  # Change min and max to remove outliers
  if (length(out2) > 0) {
    mn2 <- min(dt1[dt2 > lm2[1]])
    mx2 <- max(dt1[dt2 < lm2[2]])
    q2[1] <- mn2
    q2[5] <- mx2
    onm2 <- seq(1, length(dt2))[ol2]   # Obs within class: This matches SAS
    # onm2 <- nms2[ol2]                # Original obs #: This seems more correct
  }

  # Get length
  lndt <- length(dt1) + length(dt2)

  # Prepare to pass data
  bp <- list(
    stats = cbind(q2, q1),
    n = cbind(lndt, lndt),
    conf = NULL,
    out = numeric(0),
    names = fvls,
    out1 = out1,
    out2 = out2
  )

  return(bp)

}


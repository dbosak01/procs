

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
#' The \code{plots} parameter allows you to request several types of T-Test
#' plots. Below are the types of plots that are supported.  The list shows
#' the plot type keyword needed to request the plot, and a brief description:
#' \itemize{
#' \item{\strong{agreement}:  An agreement plot for the input data.  Only
#' available for paired comparisons.
#' }
#' \item{\strong{boxplot}: Displays a box and whisker plot for group comparisons,
#' including confidence intervals (if appropriate).
#' }
#' \item{\strong{histogram}: A histogram with normal curve and kernel density
#' overlays.
#' }
#' \item{\strong{interval}: Visualizes the confidence intervals for means.
#' }
#' \item{\strong{profiles}: A line plot mapping responses
#' between analysis variables. Available only for paired analysis.
#' }
#' \item{\strong{qqplot}: A quantile-quantile plot used to assess the assumption
#' of normality.
#' }
#' \item{\strong{summary}: Combines the histogram and boxplot charts
#' onto the same panel.
#' }
#' }
#' The above plots may be requested as a vector of keywords to the \code{plots}
#' parameter on \code{\link{proc_ttest}}, or as vector of values
#' to the \code{type} parameter on \code{ttestplots}.
#'
#' Note that, when passed as a vector of keywords, only the requested plots
#' will be produced.  That is to say, the "only" keyword in SAS is implied
#' at all times for \code{\link{proc_ttest}}.
#'
#' @param type The type(s) of plot to create. Multiple types should be passed
#' as a vector of strings.  Valid values are "agreement", "boxplot", "histogram",
#' "interval", "profiles", "qqplot", "summary".  The default value is
#' a vector with "summary" and "qqplot".
#' @param panel Whether or not to display the summary plot combined into
#' in a single panel.  Default is TRUE.  A value of FALSE will create
#' individual plots instead.  This parameter is equivalent to the
#' "unpack" keyword in SAS.
#' @param showh0 Whether or not to show the h0 line on a single-variable T-test.
#' Default is FALSE.
#' @examples
#' library(procs)
#' @export
ttestplot <- function(type = "default", panel = TRUE, showh0 = FALSE) {

  # Non-standard evaluation
  otype <- deparse(substitute(type, env = environment()))
  type <- tryCatch({if (typeof(type) %in% c("character", "NULL")) type else otype},
                   error = function(cond) {otype})


  # agreement, boxplot, histogram, interval, profiles, qqplot, summary

  # Parameter Checks
  vldvals <- c("agreement", "boxplot", "histogram", 'interval', 'profiles',
               "qqplot", "summary", "default", "all")
  if (any(!type %in% vldvals)) {

    ivd <- type[!type %in% vldvals]
    stop(paste0("Parameter value for 'type' invalid: ", paste0("'", ivd, "'", collapse = ", "),
                "\nValid values are: ",
                "'agreement', 'boxplot', 'histogram', 'interval', 'profiles', ",
                "'qqplot', 'summary', 'default', 'all'."
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
render_ttestplot <- function (dat, var, plt, class, res) {

  ret <- NULL

  if ("ttestplot" %in% class(plt)) {

    if (all(plt$type == "default")) {
      if (!is.null(plt$varlbl)) {
        plt$type <- c("summary", "profiles", "agreement", "qqplot")
      } else {
        plt$type <- c("summary", "qqplot")
      }
    }

    if (all(plt$type == "all")) {
      # agreement, boxplot, histogram, interval, profiles, qqplot, summary
      if (!is.null(plt$varlbl)) {
        plt$type <- c("summary", "histogram", "boxplot", "interval", "profiles", "agreement", "qqplot")
      } else {
        plt$type <- c("summary", "histogram", "boxplot", "interval", "qqplot")
      }
    }

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
        if (is.null(class)) {
          ret[["summary"]] <- render_summary1(dat, var, plt)
        } else {
          ret[["summary"]] <- render_summary2(dat, var, plt, class)
        }
      } else if (tp == "histogram") {
        if (is.null(class)) {
          ret[["histogram"]] <- render_histogram1(dat, var, plt)
        } else {

          ret[["histogram"]] <- render_histogram2(dat, var, plt, class)
        }
      } else if (tp == "boxplot") {
        if (is.null(class)) {
          ret[["boxplot"]] <- render_boxplot1(dat, var, plt)
        } else {
          ret[["boxplot"]] <- render_boxplot2(dat, var, plt, class)
        }
      } else if (tp == "qqplot") {
        if (is.null(class)) {
          ret[["qqplot"]] <- render_tqqplot1(dat, var, plt)
        } else {
          ret[["qqplot"]] <- render_tqqplot2(dat, var, plt, class)
        }
      } else if (tp == "interval") {
        if (is.null(class)) {
          ret[["interval"]] <- render_interval1(dat, var, plt)
        } else {
          ret[["interval"]] <- render_interval2(dat, var, plt, res)
        }
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
render_summary1 <- function(dat, var, plt) {


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

  # Assign labels
  xlbl <- var
  tlbl <- paste(" ", var)
  if (!is.null(plt$varlbl)) {
    xlbl <- "Difference"
    tlbl <- paste0(" Difference: ", plt$varlbl)
  }

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
  # scl <- range(rdt)
  # scl <- c(scl[1] * .8, scl[2] * 1.2)
  # Calculate breaks - Closer to SAS algorithm
  brks <- pretty(range(rdt), n = nclass.Sturges(rdt),
                 min.n = 1, high.u.bias = 3)

  # Calculate breaks and y scale
  h <- hist(rdt,
            breaks = brks,
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Use calculated breaks to get scale
  w <- diff(h$breaks)[1] # bin width
  xmin <- min(h$breaks) - w
  xmax <- max(h$breaks) + w
  scl <- c(xmin, xmax)

  # Use calculated break to get normal curve
  grid <- seq(scl[1] , scl[2], length.out = 300)
  y_norm_percent <- 100 * w * dnorm(grid, mean = mu, sd = sdx)
  y_mx <- max(max(y_norm_percent), max(h$counts))

  # Initial chart to draw vertical lines
  hist(rdt,
       breaks = "Sturges",
       main = "",
       ylab = "",
       xlim = scl,
       ylim = c(0, y_mx * 1.025),
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
       ylim = c(0, y_mx * 1.025),
       axes = FALSE,
       add = TRUE)

  # Title
  mtext(paste0("Distribution of", tlbl), side = 3,
        line = par("mar")[3] - 1.5,
        font = 2, cex = 1.25)

  # Subtitle
  mtext(paste0("With ", alph, "% Confidence Interval for Mean"), side = 3,
        line = par("mar")[3] - 2.5,
        font = 1)

  # Y Label
  mtext("Percent", side = 2, line = par("mar")[2] - 2)


  # Normal curve overlay (scaled to percent)
  # x <- seq(min(rdt) * 2, max(rdt) * 2, length.out = 300)
  # x <- seq(scl[1], scl[2], length.out = 300)
  #
  # y_norm <- dnorm(x, mean = mu, sd = sdx)
  # y_norm <- y_norm / max(y_norm) * max(h$counts)
  #
  # lines(x, y_norm, col = "steelblue4", lwd = 2)

  # Normal curve overlay (scaled to percent)
  lines(grid, y_norm_percent, col = "steelblue4", lwd = 2)

  # # Kernel density overlay (scaled to percent)
  # dens <- density(rdt)
  #
  # y_kern <- dens$y / max(dens$y) * max(h$counts)
  #
  # lines(dens$x, y_kern, col = "orangered2", lwd = 2)

  # Bandwidth (Silverman / SAS)
  bw <- 1.06 * sdx * n^(-1/5)

  # Kernel density estimate
  dens <- density(rdt, kernel = "gaussian", bw = bw, n = 512)

  # Scale KDE to Percent axis
  y_kernel_percent <- 100 * w * dens$y

  yscl <- seq(50, length(y_kernel_percent) - 25)

  # Overlay density on histogram
  lines(dens$x[yscl], y_kernel_percent[yscl], col = "orangered2", lwd = 2)

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
  mtext(xlbl, side = 1,
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

  # # Calculate SAS compatible quantile
  # q <- quantile(dt, probs = c(0, .25, .5, .75, 1), type = 2)
  #
  # # Prepare to pass data
  # bp <- list(
  #   stats = matrix(q, ncol = 1),
  #   n = length(dt),
  #   conf = NULL,
  #   out = numeric(0)
  # )

  # Get boxplot stats
  bp <- boxplot_stats1(dt)

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

  # Display outliers
  if (length(bp$out) > 0) {

    points(bp$out, rep(1, length(bp$out)),
           pch = 1,
           col = "grey20",
           lwd = 1.6,
           cex = 1.25)

    # Add labels
    text(bp$out, rep(1, length(bp$out)),
         labels = bp$onm,
         cex = .9,
         pos = 1)

  }

  # Legend location - Move to left side if needed
  lgnd <- "topright"
  pt <- (bp$stats[4] - xscl[1])/diff(xscl)
  if (pt > .75) {
    lgnd <- "topleft"
  }

  # Create legend
  legend(lgnd,
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


#' @noRd
render_summary2 <- function(dat, var, plt, class) {


  op <- par("mar")
  om <- par("oma")
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
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Set margins
  par(oma = c(5, 1, 2, .75) + 0.1,
      mar = c(0, 3, 0, 0) + 0.1)

  # Convert to data frame just in case it is a tibble
  dat <- as.data.frame(dat)

  # Add rownumbers
  rownames(dat) <- seq(1, nrow(dat))

  dat <- sort(dat, by = class)

  # Get analysis variables
  cvls <- unique(dat[[class]])
  vl1 <- cvls[1]
  vl2 <- cvls[2]

  # Prepare data
  dt1 <- dat[dat[[class]] == vl1, var]
  dt2 <- dat[dat[[class]] == vl2, var]

  # Overall scale
  scl <- range(dat[[var]])
  scl <- c(scl[1] * .875, scl[2] * 1.125)

  #******************************
  #*  Histogram 1
  #******************************

  par(fig = c(0, 1, .6, 1))

  # Calculate stats
  n   <- length(dt1)
  mu  <- mean(dt1)
  sdx <- sd(dt1)

  # Calculate breaks - Closer to SAS algorithm
  brks <- pretty(range(dt1), n = nclass.Sturges(dt1),
                 min.n = 1, high.u.bias = 3)

  # Histogram (Percent scale)
  h <- hist(dt1,
            breaks = brks,
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Use calculated breaks to get scale
  w <- diff(h$breaks)[1] # bin width
  xmin <- min(h$breaks) - w
  xmax <- max(h$breaks) + w
  scl1 <- c(xmin, xmax)

  # Use calculated break to get normal curve
  grid <- seq(scl1[1] , scl1[2], length.out = 300)
  y_norm_percent <- 100 * w * dnorm(grid, mean = mu, sd = sdx)
  y_mx <- max(max(y_norm_percent), max(h$counts))


  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = "",
       xlab = "",
       ylab = "Percent",
       xlim = scl,
       ylim = c(0, y_mx * 1.025),  # max(h$counts) * 1.05),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  # x <- seq(min(dt1) * 2, max(dt1) * 2, length.out = 300)
  # x <- seq(scl[1], scl[2], length.out = 300)
  #
  # y_norm <- dnorm(x, mean = mu, sd = sdx)
  # y_norm <- y_norm / max(y_norm) * max(h$counts)
  #
  # lines(x, y_norm, col = "steelblue4", lwd = 2)
  lines(grid, y_norm_percent, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  # dens <- density(dt1)
  #
  # y_kern <- dens$y / max(dens$y) * max(h$counts)
  #
  # lines(dens$x, y_kern, col = "orangered2", lwd = 2)

  # Bandwidth (Silverman / SAS)
  bw <- 1.06 * sdx * n^(-1/5)

  # Kernel density estimate
  dens <- density(dt1, kernel = "gaussian",  bw = bw, n = 512)

  # Scale KDE to Percent axis
  y_kernel_percent <- 100 * w * dens$y

  yscl <- seq(50, length(y_kernel_percent) - 25)

  # Overlay on histogram
  lines(dens$x, y_kernel_percent, col = "orangered2", lwd = 2)

  # Add custom axes
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Add y label
  mtext("Percent", side = 2, line = 2.5)

  # Add class label
  legend("topleft", legend = vl1, bty = "n",
         x.intersp = 0,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         cex = .9
  )

  # Frame
  box(col = "grey70", lwd = 1)

  #******************************
  #*  Histogram 2
  #******************************

  par(fig = c(0, 1, .2, .6), new = TRUE)

  # Calculate stats
  n   <- length(dt2)
  mu  <- mean(dt2)
  sdx <- sd(dt2)

  # Calculate breaks - Closer to SAS algorithm
  brks <- pretty(range(dt2), n = nclass.Sturges(dt2),
                 min.n = 1, high.u.bias = 3)

  # Histogram (Percent scale)
  h <- hist(dt2,
            breaks = brks,
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Use calculated breaks to get scale
  w <- diff(h$breaks)[1] # bin width
  xmin <- min(h$breaks) - w
  xmax <- max(h$breaks) + w
  scl2 <- c(xmin, xmax)

  # Use calculated break to get normal curve
  grid <- seq(scl2[1] , scl2[2], length.out = 300)
  y_norm_percent <- 100 * w * dnorm(grid, mean = mu, sd = sdx)
  y_mx <- max(max(y_norm_percent), max(h$counts))

  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = "",
       xlab = "",
       ylab = "Percent",
       xlim = scl,
       ylim = c(0, y_mx * 1.025), # max(h$counts) * 1.05),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  # x <- seq(min(dt2) * 2, max(dt2) * 2, length.out = 300)
  # x <- seq(scl[1], scl[2], length.out = 300)
  #
  # y_norm <- dnorm(x, mean = mu, sd = sdx)
  # y_norm <- y_norm / max(y_norm) * max(h$counts)
  #
  # lines(x, y_norm, col = "steelblue4", lwd = 2)

  lines(grid, y_norm_percent, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  # dens <- density(dt2)
  #
  # y_kern <- dens$y / max(dens$y) * max(h$counts)
  #
  # lines(dens$x, y_kern, col = "orangered2", lwd = 2)


  # Bandwidth (Silverman / SAS)
  bw <- 1.06 * sdx * n^(-1/5)   # 1.06

  # Kernel density estimate
  dens <- density(dt2, kernel = "gaussian", bw = bw, n = 512)

  # Scale KDE to Percent axis
  y_kernel_percent <- 100 * w * dens$y

  # Overlay on histogram
  lines(dens$x, y_kernel_percent, col = "orangered2", lwd = 2)


  # Add custom axes
  # axis(side = 1, col.ticks = "grey55", # at = xtks, labels = TRUE,
  #      mgp = c(3, .5, 0), tck = -0.015, cex.axis = .75)
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Add y label
  mtext("Percent", side = 2, line = 2.5)

  # Add class label
  legend("topleft", legend = vl2, bty = "n",
         x.intersp = 0,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         cex = .9
  )

  # Frame
  box(col = "grey70", lwd = 1)


  #**********************************
  #* Boxplot
  #**********************************

  par(fig = c(0, 1, 0, .2), new = TRUE)

  # Prepare data
  dt <- dat[[var]]
  rwnms <- rownames(dat)
  nms1 <- rwnms[dat[[class]] == vl1]
  nms2 <- rwnms[dat[[class]] == vl2]

  # Get x scale
  # xscl <- get_scale(dt, .05)

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
       xlim = scl,
       type = "n",
       xlab = "",
       ylab = "",
       yaxt = "n",
       main = "",
       axes = FALSE) #With 95% Confidence Interval for Mean")

  # Y Label
  mtext(class, side = 2,
        line = 2.5,
        font = 1)

  # Draw X axis
  aval <- axis(side = 1, las = 1, col.ticks = "grey55",
               mgp = c(3, .5, 0), tck = -0.015)

  # # Draw Y axis
  axis(side = 2, las = 1, col.ticks = "grey55", labels = cvls,
       at = c(2, 1), mgp = c(3, .5, 0), tck = -0.015)


  # Orientation lines
  abline(v = aval, col = "grey90", lwd = 1)

  # Get boxplot stats for 2 plots
  bp <- boxplot_stats2(dt1, dt2, nms1, nms2, cvls)


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
  if (length(bp$out1) > 0) {
    points(bp$out1, rep(2, length(bp$out1)),
           pch = 1,
           col = "grey20",
           lwd = 1.6,
           cex = 1.25)

    # Add labels
    text(bp$out1, rep(2, length(bp$out1)),
         labels = bp$onm1,
         cex = .9,
         pos = 1)
  }

  # Outliers for second class value
  if (length(bp$out2) > 0) {
    # Plot outlier points
    points(bp$out2, rep(1, length(bp$out2)),
           pch = 1,
           col = "grey20",
           lwd = 1.6,
           cex = 1.25)
    # Add labels
    text(bp$out2, rep(1, length(bp$out2)),
         labels = bp$onm2,
         cex = .9,
         pos = 1)
  }

  # Frame
  box(col = "grey70", lwd = 1)

  #**********************************
  #* Clean up
  #**********************************

  # Title
  mtext(paste0("Distribution of ", var), side = 3,
        line = par("oma")[3] - 1.75,
        font = 2, cex = 1.25, outer = TRUE)


  # X Label
  mtext(var, side = 1,
        line = par("oma")[1] - 3.5,
        font = 1, outer = TRUE)


  box("outer", col = "grey70", lwd = 1)


  # Legend
  mpos <- legend("bottom",
                 legend = c("Normal", "Kernel"),
                 col = c("steelblue4", "orangered2"),
                 horiz = TRUE,
                 lwd = 2,
                 box.col = "grey80",
                 bty = "o",
                 cex = .9,
                 xpd = NA,
                 inset = c(0, -1),
                 x.intersp = .5,
                 y.intersp = c(0.3, .3)  # c(0.5, 1.8)
  )


  # Restore margins
  par(mar = op, fig = of, oma = om)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)


}

# Individual Plots --------------------------------------------------------

# agreement, boxplot, histogram, interval, profiles, qqplot, summary


# Curves for normal and density most correct on this one.
#' @noRd
render_histogram1 <- function(dat, var, plt) {


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

  # Assign labels
  xlbl <- var
  tlbl <- paste(" ", var)
  if (!is.null(plt$varlbl)) {
    xlbl <- "Difference"
    tlbl <- paste0(" Difference: ", plt$varlbl)
  }

  # Calculate stats
  n   <- length(rdt)
  mu  <- mean(rdt)
  sdx <- sd(rdt)

  # Calculate breaks - Closer to SAS algorithm
  brks <- pretty(range(rdt), n = nclass.Sturges(rdt),
                   min.n = 1, high.u.bias = 3)

  # Histogram (Percent scale)
  h <- hist(rdt,
            breaks = brks,
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Use calculated breaks to get scale
  w <- diff(h$breaks)[1] # bin width
  xmin <- min(h$breaks) - w
  xmax <- max(h$breaks) + w
  scl <- c(xmin, xmax)

  # Use calculated break to get normal curve
  grid <- seq(scl[1] , scl[2], length.out = 300)
  y_norm_percent <- 100 * w * dnorm(grid, mean = mu, sd = sdx)
  y_mx <- max(max(y_norm_percent), max(h$counts))

  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = paste0("Distribution of", tlbl),
       xlab = "",
       ylab = "Percent",
       xlim = scl,
       ylim = c(0, y_mx * 1.025),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  # x <- seq(min(rdt) * 2, max(rdt) * 2, length.out = 300)
  # x <- seq(scl[1], scl[2], length.out = 300)
  #
  # y_norm <- dnorm(x, mean = mu, sd = sdx)
  # y_norm <- y_norm / max(y_norm) * max(h$counts)
  #
  # lines(x, y_norm, col = "steelblue4", lwd = 2)

  # curve(dnorm(x, mean = mean(rdt), sd = sd(rdt)),
  #       add = TRUE,
  #       col = "blue", # Color for the Normal curve
  #       lwd = 2)



  # Draw normal curve line
  lines(grid, y_norm_percent, col = "steelblue4", lwd = 2)


  # Kernel density overlay (original)
  # dens <- density(rdt)
  #
  # y_kern <- dens$y / max(dens$y) * max(h$counts)
  #
  # lines(dens$x, y_kern, col = "orangered2", lwd = 2)


  # Bandwidth (Silverman / SAS)
  bw <- 1.06 * sdx * n^(-1/5)

  # Kernel density estimate
  dens <- density(rdt, kernel = "gaussian", bw = bw, n = 512)

  # Scale KDE to Percent axis
  y_kernel_percent <- 100 * w * dens$y

  yscl <- seq(50, length(y_kernel_percent) - 25)

  # Overlay on histogram
  lines(dens$x[yscl], y_kernel_percent[yscl], col = "orangered2", lwd = 2)


  # Add x axis label
  mtext(xlbl, side = 1, line = par("mar")[1] - 2)

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


#' @noRd
render_histogram2 <- function(dat, var, plt, class) {

  op <- par("mar")
  om <- par("oma")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Convert to data frame
  dat <- as.data.frame(dat)
  dat <- sort(dat, by = class)

  # Get analysis variables
  cvls <- unique(dat[[class]])
  vl1 <- cvls[1]
  vl2 <- cvls[2]

  # Prepare data
  dt1 <- dat[dat[[class]] == vl1, var]
  dt2 <- dat[dat[[class]] == vl2, var]

  # Output to image file
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Set margins
  par(oma = c(5, 1, 2, .75) + 0.1,
      mar = c(0, 3, 0, 0) + 0.1,
      mfrow = c(2, 1))

  # Overall scale
  scl <- range(dat[[var]])
  scl <- c(scl[1] * .875, scl[2] * 1.125)


  #******************************
  #*  Histogram 1
  #******************************

  # Calculate stats
  n   <- length(dt1)
  mu  <- mean(dt1)
  sdx <- sd(dt1)

  # Calculate breaks - Closer to SAS algorithm
  brks <- pretty(range(dt1), n = nclass.Sturges(dt1),
                 min.n = 1, high.u.bias = 3)

  # Histogram (Percent scale)
  h <- hist(dt1,
            breaks = brks,
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Use calculated breaks to get scale
  w <- diff(h$breaks)[1] # bin width
  xmin <- min(h$breaks) - w
  xmax <- max(h$breaks) + w
  scl1 <- c(xmin, xmax)

  # Use calculated break to get normal curve
  grid <- seq(scl1[1] , scl1[2], length.out = 300)
  y_norm_percent <- 100 * w * dnorm(grid, mean = mu, sd = sdx)
  y_mx <- max(max(y_norm_percent), max(h$counts))


  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = "",
       xlab = "",
       ylab = "Percent",
       xlim = scl,
       ylim = c(0, y_mx * 1.025),  # max(h$counts) * 1.05),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  # x <- seq(min(dt1) * 2, max(dt1) * 2, length.out = 300)
  # x <- seq(scl[1], scl[2], length.out = 300)
  #
  # y_norm <- dnorm(x, mean = mu, sd = sdx)
  # y_norm <- y_norm / max(y_norm) * max(h$counts)
  #
  # lines(x, y_norm, col = "steelblue4", lwd = 2)
  lines(grid, y_norm_percent, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  # dens <- density(dt1)
  #
  # y_kern <- dens$y / max(dens$y) * max(h$counts)
  #
  # lines(dens$x, y_kern, col = "orangered2", lwd = 2)

  # Bandwidth (Silverman / SAS)
  bw <- 1.06 * sdx * n^(-1/5)

  # Kernel density estimate
  dens <- density(dt1, kernel = "gaussian",  bw = bw, n = 512)

  # Scale KDE to Percent axis
  y_kernel_percent <- 100 * w * dens$y

  yscl <- seq(50, length(y_kernel_percent) - 25)

  # Overlay on histogram
  lines(dens$x, y_kernel_percent, col = "orangered2", lwd = 2)

  # Add custom axes
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Add y label
  mtext("Percent", side = 2, line = 2.5)

  # Add class label
  legend("topleft", legend = vl1, bty = "n",
         x.intersp = 0,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         cex = .9
  )

  # Frame
  box(col = "grey70", lwd = 1)

  #******************************
  #*  Histogram 2
  #******************************

  # Calculate stats
  n   <- length(dt2)
  mu  <- mean(dt2)
  sdx <- sd(dt2)

  # Calculate breaks - Closer to SAS algorithm
  brks <- pretty(range(dt2), n = nclass.Sturges(dt2),
                 min.n = 1, high.u.bias = 3)

  # Histogram (Percent scale)
  h <- hist(dt2,
            breaks = brks,
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Use calculated breaks to get scale
  w <- diff(h$breaks)[1] # bin width
  xmin <- min(h$breaks) - w
  xmax <- max(h$breaks) + w
  scl2 <- c(xmin, xmax)

  # Use calculated break to get normal curve
  grid <- seq(scl2[1] , scl2[2], length.out = 300)
  y_norm_percent <- 100 * w * dnorm(grid, mean = mu, sd = sdx)
  y_mx <- max(max(y_norm_percent), max(h$counts))

  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = "",
       xlab = "",
       ylab = "Percent",
       xlim = scl,
       ylim = c(0, y_mx * 1.025), # max(h$counts) * 1.05),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  # x <- seq(min(dt2) * 2, max(dt2) * 2, length.out = 300)
  # x <- seq(scl[1], scl[2], length.out = 300)
  #
  # y_norm <- dnorm(x, mean = mu, sd = sdx)
  # y_norm <- y_norm / max(y_norm) * max(h$counts)
  #
  # lines(x, y_norm, col = "steelblue4", lwd = 2)

  lines(grid, y_norm_percent, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  # dens <- density(dt2)
  #
  # y_kern <- dens$y / max(dens$y) * max(h$counts)
  #
  # lines(dens$x, y_kern, col = "orangered2", lwd = 2)


  # Bandwidth (Silverman / SAS)
  bw <- 1.06 * sdx * n^(-1/5)   # 1.06

  # Kernel density estimate
  dens <- density(dt2, kernel = "gaussian", bw = bw, n = 512)

  # Scale KDE to Percent axis
  y_kernel_percent <- 100 * w * dens$y

  # Overlay on histogram
  lines(dens$x, y_kernel_percent, col = "orangered2", lwd = 2)


  # Add custom axes
  axis(side = 1, col.ticks = "grey55", # at = xtks, labels = TRUE,
       mgp = c(3, .5, 0), tck = -0.015, cex.axis = .75)
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Add y label
  mtext("Percent", side = 2, line = 2.5)

  # Add class label
  legend("topleft", legend = vl2, bty = "n",
         x.intersp = 0,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         cex = .9
         )

  # Frame
  box(col = "grey70", lwd = 1)


  #******************************
  #*  Clean up
  #******************************

  box("outer", col = "grey70", lwd = 1)

  # Add titles
  mtext(paste0("Distribution of ", var), side = 3, line = par("oma")[3] - 1.75,
        outer = TRUE, font = 2, cex = 1.25)

  # Add x axis label
  mtext(var, side = 1, line = par("oma")[1] - 3.5, outer = TRUE)

  # Add legend
  mpos <- legend("bottom",
         legend = c("Normal ",
                    "Kernel"),
         col = c("steelblue4", "orangered2"),
         lwd = 2,
         cex = .9,
         horiz = TRUE,
         bty = "o",
         box.col = "grey", # Grey border to match SAS
         x.intersp = 1,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         text.width = NA,  # Compute label widths dynamically
         inset = c(0, -.38),
         xpd = NA)

  # Restore margins
  par(mar = op,  mfrow = c(1, 1), oma = om)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}

#' @noRd
render_histogram2_back <- function(dat, var, plt, class) {

  op <- par("mar")
  om <- par("oma")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Convert to data frame
  dat <- as.data.frame(dat)
  dat <- sort(dat, by = class)

  # Get analysis variables
  cvls <- unique(dat[[class]])
  vl1 <- cvls[1]
  vl2 <- cvls[2]

  # Prepare data
  dt1 <- dat[dat[[class]] == vl1, var]
  dt2 <- dat[dat[[class]] == vl2, var]

  # Use calculated breaks to get scale
  scl <- range(dat[[var]])
  scl <- c(scl[1] * .8, scl[2] * 1.2)

  # Output to image file
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Set margins
  par(oma = c(5, 1, 2, .75) + 0.1,
      mar = c(0, 3, 0, 0) + 0.1,
      mfrow = c(2, 1))


  #******************************
  #*  Histogram 1
  #******************************

  # Calculate stats
  n   <- length(dt1)
  mu  <- mean(dt1)
  sdx <- sd(dt1)

  # Histogram (Percent scale)
  h <- hist(dt1,
            breaks = "Sturges",
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100



  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = "",
       xlab = "",
       ylab = "Percent",
       xlim = scl,
       ylim = c(0, max(h$counts) * 1.05),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  x <- seq(min(dt1) * 2, max(dt1) * 2, length.out = 300)
  x <- seq(scl[1], scl[2], length.out = 300)

  y_norm <- dnorm(x, mean = mu, sd = sdx)
  y_norm <- y_norm / max(y_norm) * max(h$counts)

  lines(x, y_norm, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  dens <- density(dt1)

  y_kern <- dens$y / max(dens$y) * max(h$counts)

  lines(dens$x, y_kern, col = "orangered2", lwd = 2)

  # Add custom axes
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Add y label
  mtext("Percent", side = 2, line = 2.5)

  # Add class label
  legend("topleft", legend = vl1, bty = "n",
         x.intersp = 0,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         cex = .9
  )

  # Frame
  box(col = "grey70", lwd = 1)

  #******************************
  #*  Histogram 2
  #******************************

  # Calculate stats
  n   <- length(dt2)
  mu  <- mean(dt2)
  sdx <- sd(dt2)

  # Histogram (Percent scale)
  h <- hist(dt2,
            breaks = "Sturges",
            plot = FALSE)

  # Convert counts to percent
  h$counts <- h$counts / sum(h$counts) * 100

  # Create plot using histogram values
  plot(h,
       col = "#CAD5E5", #"grey85",
       border = "grey20",
       main = "",
       xlab = "",
       ylab = "Percent",
       xlim = scl,
       ylim = c(0, max(h$counts) * 1.05),
       axes = FALSE)

  # Normal curve overlay (scaled to percent)
  x <- seq(min(dt2) * 2, max(dt2) * 2, length.out = 300)
  x <- seq(scl[1], scl[2], length.out = 300)

  y_norm <- dnorm(x, mean = mu, sd = sdx)
  y_norm <- y_norm / max(y_norm) * max(h$counts)

  lines(x, y_norm, col = "steelblue4", lwd = 2)

  # Kernel density overlay (scaled to percent)
  dens <- density(dt2)

  y_kern <- dens$y / max(dens$y) * max(h$counts)

  lines(dens$x, y_kern, col = "orangered2", lwd = 2)

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", # at = xtks, labels = TRUE,
       mgp = c(3, .5, 0), tck = -0.015, cex.axis = .75)
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  # Add y label
  mtext("Percent", side = 2, line = 2.5)

  # Add class label
  legend("topleft", legend = vl2, bty = "n",
         x.intersp = 0,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         cex = .9
  )

  # Frame
  box(col = "grey70", lwd = 1)


  #******************************
  #*  Clean up
  #******************************

  box("outer", col = "grey70", lwd = 1)

  # Add titles
  mtext(paste0("Distribution of ", var), side = 3, line = par("oma")[3] - 1.75,
        outer = TRUE, font = 2, cex = 1.25)

  # Add x axis label
  mtext(var, side = 1, line = par("oma")[1] - 3.5, outer = TRUE)

  # Add legend
  mpos <- legend("bottom",
                 legend = c("Normal ",
                            "Kernel"),
                 col = c("steelblue4", "orangered2"),
                 lwd = 2,
                 cex = .9,
                 horiz = TRUE,
                 bty = "o",
                 box.col = "grey", # Grey border to match SAS
                 x.intersp = 1,  # Spacing between group label and box
                 y.intersp = 0,  # Spacing between content and borders
                 text.width = NA,  # Compute label widths dynamically
                 inset = c(0, -.38),
                 xpd = NA)

  # Restore margins
  par(mar = op,  mfrow = c(1, 1), oma = om)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}


# Legend moves to left if mean too far to the right
#' @noRd
render_boxplot1 <- function(dat, var, plt) {


  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 2.25 # 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines
  alph <- (1 - plt$alph) * 100  # Percentage
  alpha <- plt$alph             # Actual value

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Output to image file
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Prepare data
  dt <- dat[[var]]

  # Set margins
  par(mar = c(3, 1, 3, .75) + 0.1)

  # Assign labels
  xlbl <- var
  tlbl <- paste(" ", var)
  if (!is.null(plt$varlbl)) {
    xlbl <- "Difference"
    tlbl <- paste0(" Difference: ", plt$varlbl)
  }

  # Get scales
  xscl <- get_scale(dt, .05)

  # Get stats
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
  mtext(paste0("Distribution of", tlbl), side = 3,
        line = par("mar")[3] - 1.5,
        font = 2, cex = 1.25)

  # Subtitle
  mtext(paste0("With ", alph, "% Confidence Interval for Mean"), side = 3,
        line = par("mar")[3] - 2.5,
        font = 1)

  # X Label
  mtext(xlbl, side = 1,
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

  # Get boxplot stats
  bp <- boxplot_stats1(dt)

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

  # Display outliers
  if (length(bp$out) > 0) {

    points(bp$out, rep(1, length(bp$out)),
           pch = 1,
           col = "grey20",
           lwd = 1.6,
           cex = 1.25)

    # Add labels
    text(bp$out, rep(1, length(bp$out)),
         labels = bp$onm,
         cex = .9,
         pos = 1)

  }

  # Legend location - Move to left side if needed
  lgnd <- "topright"
  pt <- (bp$stats[4] - xscl[1])/diff(xscl)
  if (pt > .75) {
    lgnd <- "topleft"
  }

  # Create legend
  legend(lgnd,
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
  alph <- (1 - plt$alph) * 100  # Percentage
  alpha <- plt$alph             # Actual value

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
  # xscl <- get_scale(dt, .05)

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

  # Get boxplot stats for 2 plots
  bp <- boxplot_stats2(dt1, dt2, nms1, nms2, fvls)


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
  if (length(bp$out1) > 0) {
    points(bp$out1, rep(2, length(bp$out1)),
           pch = 1,
           col = "grey20",
           lwd = 1.6,
           cex = 1.25)

    # Add labels
    text(bp$out1, rep(2, length(bp$out1)),
         labels = bp$onm1,
         cex = .9,
         pos = 1)
  }

  # Outliers for second class value
  if (length(bp$out2) > 0) {
    # Plot outlier points
    points(bp$out2, rep(1, length(bp$out2)),
           pch = 1,
           col = "grey20",
           lwd = 1.6,
           cex = 1.25)
    # Add labels
    text(bp$out2, rep(1, length(bp$out2)),
         labels = bp$onm2,
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
render_interval1 <- function(dat, var, plt) {


  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 2.75 # 4.5  # Height in inches
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

  # Prepare data
  dt <- dat[[var]]

  # Set margins
  par(mar = c(3, 1, 3, .75) + 0.1)

  # Calculate basic parameters
  n  <- length(dt)
  mu <- mean(dt)
  sdx <- sd(dt)

  # Assign labels
  xlbl <- var
  tlbl <- paste(" ", var)
  if (!is.null(plt$varlbl)) {
    xlbl <- "Difference"
    tlbl <- paste0(" Difference: ", plt$varlbl)
  }

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
  mtext(paste0("Mean of ", tlbl), side = 3,
        line = par("mar")[3] - 1.5,
        font = 2, cex = 1.25)

  # Subtitle
  mtext(paste0("With ", alph, "% Confidence Interval"), side = 3,
        line = par("mar")[3] - 2.5,
        font = 1)

  # X Label
  mtext(xlbl, side = 1,
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

render_interval2 <- function(dat, var, plt, res) {


  op <- par("mar")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 2.75 # 4.5  # Height in inches
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

  # Prepare data
  dt <- res$ConfLimits

  # Set margins
  par(mar = c(4, 1, 3, .75) + 0.1)

  # Get class label
  clslbl <- paste0("(", dt$CLASS[1], " - ", dt$CLASS[2], ")")

  # Get labels for means
  labels <- c("Satterthwaite", "Pooled")

  # Pull off data values
  mean_diff <- c(dt$MEAN[4], dt$MEAN[3])
  lower_ci  <- c(dt$LCLM[4], dt$LCLM[3])
  upper_ci  <- c(dt$UCLM[4],  dt$UCLM[3])

  # Create y values
  y <- c(2, 1)  # vertical positions

  # Create base plot
  plot(mean_diff, y,
       xlim = range(lower_ci, upper_ci),
       ylim = c(0.5, 2.5),
       xlab = "",
       ylab = "",
       yaxt = "n",
       pch  = NA,
       main = "",
       axes = FALSE)


  # Title
  mtext(paste("Mean of ", var, " Difference ", clslbl), side = 3,
              line = 1.5, cex = 1.25, font = 2)

  ## Subtitle
  mtext(paste("With ", alph, "% Confidence Intervals"), side = 3,
        line = .5, cex = 1)

  # X axis label
  mtext("Difference", side = 1, line = 2)

  # Draw axis
  aval <- axis(side = 1, las = 1, col.ticks = "grey55",
               mgp = c(3, .5, 0), tck = -0.015)

  ## Confidence interval lines
  segments(lower_ci, y, upper_ci, y,
           col = "brown3", lwd = 3)

  ## CI end caps
  segments(lower_ci, y - 0.08, lower_ci, y + 0.08,
           col = "brown3", lwd = 3)

  segments(upper_ci, y - 0.08, upper_ci, y + 0.08,
           col = "brown3", lwd = 3)

  ## Mean diamonds
  points(mean_diff, y,
         pch = 5,           # diamond
         col = "#05379B",
         cex = 1.25)

  ## Labels above lines
  text(mean_diff, y + 0.15, labels,
       cex = 0.9)

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



  # # Calculate basic parameters
  # n  <- length(dt)
  # mu <- mean(dt)
  # sdx <- sd(dt)
  #
  # ## 95% CI for mean (SAS uses t-based CI)
  # tcrit <- qt(1 - alpha / 2, df = n - 1)
  #
  # # Calculate confidence interval
  # ci <- mu + c(-1, 1) * tcrit * sdx / sqrt(n)
  #
  # # Get scales
  # xscl <- get_scale(ci, .001)
  #
  # # Create plot
  # plot(ci, rep(1, length(ci)),
  #      type = "n",
  #      yaxt = "n",
  #      ylab = "",
  #      xlab = "",
  #      xlim = xscl,
  #      main = "",
  #      axes = FALSE)
  #
  # # Title
  # mtext(paste0("Mean of ", var), side = 3,
  #       line = par("mar")[3] - 1.5,
  #       font = 2, cex = 1.25)
  #
  # # Subtitle
  # mtext(paste0("With ", alph, "% Confidence Interval"), side = 3,
  #       line = par("mar")[3] - 2.5,
  #       font = 1)
  #
  # # X Label
  # mtext(var, side = 1,
  #       line = par("mar")[1] - 1.5,
  #       font = 1)
  #
  # # Draw axis
  # aval <- axis(side = 1, las = 1, col.ticks = "grey55",
  #              mgp = c(3, .5, 0), tck = -0.015)
  #
  # ## CI line
  # segments(ci[1], 1, ci[2], 1,
  #          col = "firebrick3",
  #          lwd = 2)
  #
  # ## CI end caps
  # segments(ci[1], 0.95, ci[1], 1.05,
  #          col = "firebrick3",
  #          lwd = 2)
  #
  # segments(ci[2], 0.95, ci[2], 1.05,
  #          col = "firebrick3",
  #          lwd = 2)
  #
  # ## Mean diamond
  # points(mu, 1,
  #        pch = 5,
  #        col = "#05379B",
  #        cex = 1.25)
  #
  # # Create legend
  # legend("topright",
  #        legend = "Mean",
  #        pch = 5,
  #        col = "#05379B",
  #        box.col = "grey80",
  #        inset = c(.01, .02),
  #        x.intersp = .5,
  #        y.intersp = c(0.5, 1.8),
  #        cex = .9,
  #        bty = "o")

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
render_tqqplot1 <- function(dat, var, plt) {


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

  # Assign label
  ylbl <- var
  tlbl <- paste(" ", var)
  if (!is.null(plt$varlbl)) {
    ylbl <- "Difference"
    tlbl <- paste0(": ", plt$varlbl)
  }

  # Set margins
  par(mar = c(4, 5, 2, .75) + 0.1)

  # Get y scale
  # mx <- max(abs(rdt)) * 1.125  #1.0025
  # scl <- c(-mx, mx)

  # Draw plot
  qqnorm(rdt,
         main = paste0("Q-Q Plot of", tlbl),
         xlab = "",
         ylab = ylbl,
         pch  = 1,          # open circles
         col  = "#05379B",
         cex = 1.3,
        # ylim = scl,
       #  asp = 1,
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
render_tqqplot2 <- function(dat, var, plt, class) {


  op <- par("mar")
  om <- par("oma")

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 3.25  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Prepare data
  dat <- as.data.frame(dat)
  dat <- sort(dat, by = class)

  # Subset
  cvls <-  unique(dat[[class]])
  dt1 <- dat[dat[[class]] == cvls[1], var]
  dt2 <- dat[dat[[class]] == cvls[2], var]

  # Output to image file
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")


  # Set margins
  par(oma = c(0, 0, 2, .75) + 0.1,
      mar = c(4, 4, 0, .0) + 0.1,
      mfrow = c(1, 2))


  #*******************************
  #* QQPlot 1
  #*******************************

  # Get y scale
  # mx <- max(abs(rdt)) * 1.125  #1.0025
  # scl <- c(-mx, mx)

  # Draw plot
  qqnorm(dt1,
         main = "",
         xlab = "",
         ylab = "",
         pch  = 1,          # open circles
         col  = "#05379B",
         cex = 1,
         # ylim = scl,
         axes = FALSE)

  # Add diagonal line
  # This line is a problem.  Right now just putting it diagonal
  # between the corners.  All qqline() functions are worse.
  usr <- par("usr")
  segments(usr[1], usr[3], usr[2], usr[4], col = "grey60")
  # qqline(dt1, col = "grey60", lwd = 1, qtype = 3)

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # X Axis label
  mtext("Quantile", side = 1, line = par("mar")[1] - 2)

  # Y Axis label
  mtext(var, side = 2, line = par("mar")[2] - 1.5)

  # Add class label
  legend("topleft", legend = cvls[1], bty = "n",
         x.intersp = 0,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         cex = .9
  )

  # Frame
  box(col = "grey70", lwd = 1)

  #*******************************
  #* QQPlot 2
  #*******************************

  # Get y scale
  # mx <- max(abs(rdt)) * 1.125  #1.0025
  # scl <- c(-mx, mx)

  # Draw plot
  qqnorm(dt2,
         main = "",
         xlab = "",
         ylab = "",
         pch  = 1,          # open circles
         col  = "#05379B",
         cex = 1,
         # ylim = scl,
         axes = FALSE)

  # Add diagonal line
  # This line is a problem.  Right now just putting it diagonal
  # between the corners.  All qqline() functions are worse.
  usr <- par("usr")
  segments(usr[1], usr[3], usr[2], usr[4], col = "grey60")
  # qqline(dt1, col = "grey60", lwd = 1, qtype = 3)

  # Add custom axes
  axis(side = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55", mgp = c(3, .5, 0), tck = -0.015)

  # X Axis label
  mtext("Quantile", side = 1, line = par("mar")[1] - 2)

  # Y Axis label
  mtext(var, side = 2, line = par("mar")[2] - 1.5)

  # Add class label
  legend("topleft", legend = cvls[2], bty = "n",
         x.intersp = 0,  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         cex = .9
  )

  # Frame
  box(col = "grey70", lwd = 1)

  #*******************************
  #* Clean up
  #*******************************


  # Title
  mtext(paste0("Q-Q Plots of ", var), side = 3, outer = TRUE,
        line = par("oma")[3] - 1.75, font = 2, cex = 1.25)

  box("outer", col = "grey70", lwd = 1)

  par(mar = op, oma = om, mfrow = c(1, 1))

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)


}

#' @noRd
render_profiles <- function(dat, var, plt) {


  op <- par("mar")

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

  # Prepare data
  splt <- trimws(strsplit(plt$varlbl, "-", fixed = TRUE)[[1]])
  v1 <- splt[1]
  v2 <- splt[2]
  vrs <- c(v1, v2)

  dt <- as.matrix(dat[ , vrs ])

  ## X positions (categorical)
  x <- c(1, 2)

  par(mar = c(3, 3, 3, 3) + 0.1)

  ## Empty plot frame
  plot(x, range(dt),
       type = "n",
       xaxt = "n",
       xlab = "",
       ylab = "",
       main = paste0("Paired Profiles for (", v1, ", ", v2, ")"),
       axes = FALSE)

  # X Axis
  axis(1, at = x, labels = c(v1, v2))

  # Y Axis
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 4, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  ## Individual subject profiles (thin lines)
  for (i in seq_len(nrow(dt))) {
    lines(x, dt[i, ],
          col = rgb(0.2, 0.4, 0.8, 0.45),  # light blue
          lwd = 1)
  }

  ## Mean profile (bold line)
  mean_profile <- colMeans(dt)

  lines(x, mean_profile,
        col = "orangered2",
        lwd = 3)

  ## Mean legend
  legend("top",
         legend = "Mean",
         col = "orangered2",
         lwd = 3,
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
render_agreement <- function(dat, var, plt) {


  op <- par("mar")

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
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

  # Prepare data
  splt <- trimws(strsplit(plt$varlbl, "-", fixed = TRUE)[[1]])
  v1 <- splt[1]
  v2 <- splt[2]
  vrs <- c(v1, v2)

  # Get vectors
  dt1 <- dat[[v1]]
  dt2 <- dat[[v2]]

  ## Means
  m1 <- mean(dt1)
  m2 <- mean(dt2)

  ## Axis limits
  lims <- range(c(dt1, dt2))

  # Set margins
  par(mar = c(4, 10, 2, 6) + 0.1)

  ## Scatter plot
  plot(dt1, dt2,
       xlim = lims,
       ylim = lims,
       pch  = 1,
       cex = 1.25,
       col  = "#05379B",
       xlab = "",
       ylab = v2,
       main = paste0("Agreement of ", v2, " and ", v1),
       axes = FALSE,
       asp  = 1)   # equal scaling (critical)

  # X label
  mtext(v1, side = 1, line = par("mar")[1] - 2)

  # Axes
  axis(side = 1, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)
  axis(side = 2, las = 1, col.ticks = "grey55",
       mgp = c(3, .5, 0), tck = -0.015)

  ## Identity (45-degree) line
  usr <- par("usr")
  segments(usr[1], usr[3], usr[2], usr[4],
           col = "grey60", lwd = 1)

  ## Mean point
  points(m1, m2,
         pch = 21,
         bg  = "white",
         col = "#05379B",
         lwd = 2,
         cex = 2.5)

  ## Legend
  legend("right",
         legend = "Mean",
         pch = 21,
         pt.bg = "white",
         col = "#05379B",
         pt.lwd = 2,
        # lwd = 3,
         box.col = "grey80",
         inset = c(-.2, 0),
         x.intersp = 1,
         y.intersp = c(0.5, 1.8),
         pt.cex = 2.5,
         xpd = NA,
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



# Utilities ---------------------------------------------------------------

# Boxplot.stats(data1) can prepare the boxplot data more easily,
# but has no parameter to use SAS-style quantiles. So need to calculate
# everything manually.
#' @noRd
boxplot_stats1 <- function(dt1) {

  # Calculate quantile
  q1 <- quantile(dt1, probs = c(0, .25, .5, .75, 1), type = 2, na.rm = TRUE)

  # Calculate Inter-Quartile Range
  iqr1 <- IQR(dt1, type = 2, na.rm = TRUE) * 1.5

  # Calculate outlier limits
  lm1 <- c(q1[2] - iqr1, q1[4] + iqr1)

  # Determine if there are outliers
  ol1 <- dt1 <= lm1[1] | dt1 >= lm1[2]
  out1 <- dt1[ol1]

  # Change min and max to remove outliers
  onm1 <- c()
  if (length(out1) > 0) {
    mn1 <- min(dt1[dt1 > lm1[1]])
    mx1 <- max(dt1[dt1 < lm1[2]])
    q1[1] <- mn1
    q1[5] <- mx1
    onm1 <- seq(1, length(dt1))[ol1]   # Obs within class: This matches SAS
    # onm1 <- nms1[ol1]                # Original obs #: This seems more correct
  }

  # Get length
  lndt <- length(dt1)

  # Prepare to pass data
  bp <- list(
    stats = matrix(q1, ncol = 1),
    n = lndt,
    conf = NULL,
    out = out1,
    onm = onm1
  )

  return(bp)

}


# Boxplot.stats(data1) can prepare the boxplot data more easily,
# but has no parameter to use SAS-style quantiles. So need to calculate
# everything manually.
#' @noRd
boxplot_stats2 <- function(dt1, dt2, nms1, nms2, fvls) {

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
  onm1 <- c()
  if (length(out1) > 0) {
    mn1 <- min(dt1[dt1 > lm1[1]])
    mx1 <- max(dt1[dt1 < lm1[2]])
    q1[1] <- mn1
    q1[5] <- mx1
    onm1 <- nms1[ol1]                # Original obs #: This seems more correct
  }

  # Change min and max to remove outliers
  onm2 <- c()
  if (length(out2) > 0) {
    mn2 <- min(dt1[dt2 > lm2[1]])
    mx2 <- max(dt1[dt2 < lm2[2]])
    q2[1] <- mn2
    q2[5] <- mx2
    onm2 <- nms2[ol2]                # Original obs #: This seems more correct
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
    "out1" = out1,
    "out2" = out2,
    "onm1" = onm1,
    "onm2" = onm2
  )

  return(bp)

}

# breaks <- pretty(range(mdt), n = nclass.Sturges(mdt), min.n = 1, high.u.bias = 3)


# breaks <- pretty(range(mdt), n = nclass.Sturges(mdt), min.n = 1, high.u.bias = 3)
#
# breaks <- pretty(range(cls$Weight), n = nclass.Sturges(cls$Weight), min.n = 1, high.u.bias = 3)
#
#
#
# breaks <- pretty(range(mdt), n = nclass.Sturges(mdt), min.n = 1, high.u.bias = 3)
#
# get_bins <- function(x, maxbins = 6) {
#
#   cbns <- nclass.Sturges(x)
#
#
# }

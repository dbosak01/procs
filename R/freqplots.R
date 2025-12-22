

# SAS plot type options: agreeplot, all, cumfreqplot, deviationplot, freqplot,
# kappaplot, mosaicplot, none, oddsratioplot, relriskplot, riskdiffplot, wtkappaplot
# See https://documentation.sas.com/doc/en/statug/15.2/statug_freq_syntax08.htm#statug.freq.freqplots



# Plot Function Definitions -----------------------------------------------



# SAS freqplot options:
# groupby = variable,
# npanelpos = sections per panal, default = 4,
# orient = (horizontal or vertical),
# scale = (freq | grouppercent | log | percent |sqrt),
# twoway = (cluster|grouphorizontal|groupvertical|stacked)
# type = (barchart or dotplot)

#' @title Request a Frequency Plot
#' @description A function to request a frequency plot on a call to
#' \code{\link{proc_freq}}. The function allows you to specify the type of
#' frequency plot to produce, and various layout options. Function supports
#' bar charts and dot plots for one and two-way analysis.  It also supports
#' vertical or horizontal orientation, by variables, and various scale options.
#' @details Any requested
#' plots will be displayed on interactive reports only. You may, however,
#' output the report objects and pass to \code{\link{proc_print}}. To do this,
#' set \code{output = report} on the call to \code{\link{proc_freq}}.
#' @param type The type of plot to create. Valid values are "barchart" or
#' "dotplot".  Default is "barchart".
#' @param orient The orientation of the plot.  Valid values are "vertical"
#' or "horizontal".  Default is "vertical".
#' @param scale The scale to use for the plot. Valid values are "freq",
#' "log", "percent", or "sqrt".  Default is "freq".  For two-way tables,
#' the value "grouppercent" is also valid.
#' @param twoway Options for two-way layouts. Valid values are
#' "cluster", "grouphorizontal", "groupvertical", or "stacked". Default
#' is "groupvertical".  This parameter applies to two-way tables only.
#' @param groupby The variable to group by. Valid values are "column" or "row".
#' Default is "column". This parameter applies to two-way tables only.
#' @param npanelpos The number of sections per panel. Default is 4. This
#' parameter applies to two-way tables only.
#' @returns The frequency plot object.
#' @export
freqplot <- function(type = "barchart", orient = "vertical", scale = "freq",
                     twoway = "groupvertical", groupby = "column", npanelpos = 4) {


  # Create object
  ret <- structure(list(), class = c("freqplot", "list"))

  ret$groupby <- groupby
  ret$npanelpos <- npanelpos
  ret$orient <- tolower(orient)
  ret$scale <- tolower(scale)
  ret$twoway <- tolower(twoway)
  ret$type <- tolower(type)

  return(ret)
}


# Plot Rendering ----------------------------------------------------------


#' @noRd
render_freqplot <- function (dat, tbl1, tbl2 = NULL, plt) {

  ret <- NULL

  if (is.character(plt)) {
    if (plt == "freqplot") {
      plt <- freqplot()
    }
  }


  if ("freqplot" %in% class(plt)) {

    if (is.null(tbl2)) {
      ret <- render_freqplot.1way(dat, tbl1, plt)
    } else {
      ret <- render_freqplot.2way(dat, tbl1, tbl2, plt)
    }
  }




  return(ret)
}


#' @title Render the Frequency Plot - One Way plot
#' @description A function to render the frequency plot.
#' @param plt The object to render
#' @param dat The data to render
#' @param table The table request to render.
#' @returns The path to the plot.
#' @import graphics
#' @import grDevices
#' @import reporter
#' @noRd
render_freqplot.1way <- function(dat, tbl, plt) {

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Set bar background color
  bgcolor <- "#E5EAF2"

  # Set orientation
  horz <- FALSE  # Default to vertical
  if (plt$orient == "horizontal") {
    horz <- TRUE
  }

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100, antialias = "cleartype")

  # Prepare data
  if (plt$scale == "percent") {

    cnt <- as.numeric(dat$PCT)
    names(cnt) <- as.character(dat$CAT)
    slbl <- "Percent"

  } else if (plt$scale == "log") {

    cnt <- as.numeric(log10(dat$CNT))
    names(cnt) <- as.character(dat$CAT)
    slbl <- "Log Frequency"

  } else if (plt$scale == "sqrt") {

    cnt <- as.numeric(sqrt(dat$CNT))
    names(cnt) <- as.character(dat$CAT)
    slbl <- "Sqrt Frequency"

  } else {  # Default to Frequency

    cnt <- as.numeric(dat$CNT)
    names(cnt) <- as.character(dat$CAT)
    slbl <- "Frequency"

  }

  # Get original margins
  omar <- par()$mar

  if (plt$type == "dotplot") {  # Dot plot

    # X positions for categories
    xdat <- seq_along(cnt)

    if (plt$orient == "vertical") {

      # Low to high
      xlm <- c(.5, length(cnt) + .5)

      # Plot
      op <- par(mar = c(5, 5, 4, 2) + 0.1)

      plot(
        xdat, cnt,
        type = "p",
        pch  = 16,
        col  = "#05379B",
        xaxt = "n",
        cex =  1.3, #1.5,
        xlim = xlm,
        ylim = c(0, max(cnt) * 1.05),
        axes = FALSE,
        xlab = tbl,
        ylab = slbl,
        main = paste0("Distribution of ", tbl)
      )

      # Vertical dotted grid lines at each category
      abline(v = xdat, lty = "dotted", col = "gray85")

      # Axes
      axis(1, at = xdat, labels = names(cnt), col.ticks = "grey55")
      axis(2, las = 1, col.ticks = "grey55") # at = seq(0, 400, by = 100)

    } else {  # Horizontal

      # High to Low
      xlm <- c(length(cnt) + .5, .5)

      # Plot
      op <- par(mar = c(5, 6, 4, 2) + 0.1)

      plot(
        cnt, xdat,
        type = "p",
        pch  = 16,
        col  = "#05379B",
        yaxt = "n",
        cex =  1.3, #1.5,
        xlim = c(0, max(cnt) * 1.05),
        ylim = xlm,
        axes = FALSE,
        xlab = slbl,
        ylab = "",
        main = paste0("Distribution of ", tbl)
      )

      mtext(tbl, side = 2, line = op$mar[1] - 1)

      # Vertical dotted grid lines at each category
      abline(h = xdat, lty = "dotted", col = "gray85")

      # Axes
      axis(2, at = xdat, labels = names(cnt), las = 1, col.ticks = "grey55")
      axis(1, las = 1, col.ticks = "grey55") # at = seq(0, 400, by = 100)

    }

  } else {  # Bar chart

    if (horz == TRUE) {  # Horizontal

      # Set custom margins
      op <- par(mar = c(5, 6, 4, 2) + 0.1)

      # Create empty plot
      b1 <- barplot(
        rep(NA, length(cnt)),  # Empty data
        main = paste0("Distribution of ", tbl),  # Title
        xlab = slbl,  # Lable x axis
      #  ylab = tbl,  # Label y axis
        xlim = c(0, max(cnt) * 1.05),  # x axis scale
        horiz = TRUE,
        axes = FALSE  # Don't create axis yet
      )


      mtext(tbl, side = 2, line = op$mar[1] - 1)

      # Get tick marks for ablines
      a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
      # a2 <- axis(2, las = 1, col.ticks = "grey55")  # Create axis

      ## Add gridlines based on axis created above
      abline(v = a1, col = "grey90", lwd = 1)

    } else {  # Vertical

      # Set custom margins
      op <- par(mar = c(5, 5, 4, 2) + 0.1)

    # Create empty plot
      b1 <- barplot(
        rep(NA, length(cnt)),  # Empty data
        main = paste0("Distribution of ", tbl),  # Title
        xlab = tbl,  # Lable x axis
        ylab = slbl,  # Label y axis
        ylim = c(0, max(cnt) * 1.05),  # y axis scale
        horiz = FALSE,
        axes = FALSE  # Don't create axis yet
      )

      # Get tick marks for ablines
      # a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
      a2 <- axis(2, las = 1, col.ticks = "grey55")  # Create axis

      ## Add gridlines based on axis created above
      abline(h = a2, col = "grey90", lwd = 1)
    }


    # Add barplot
    b2 <- barplot(
      cnt,   # Data
      horiz = horz,
      col  = bgcolor, # adjustcolor("grey80", alpha.f = 0.45),
      border = "grey55",
      las = 1,
      tick = TRUE,
      add = TRUE,   # Add to existing plot
      axes = FALSE  # Already created above
    )

  }


  ## frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  # Restore margins
  par(mar = omar)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

  return(ret)

}

#' @noRd
render_freqplot.2way <- function(dat, tbl1, tbl2, plt) {

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Set bar background color
  bgcolor <- "#E5EAF2"

  # Set orientation
  horz <- FALSE  # Default to vertical
  if (plt$orient == "horizontal") {
    horz <- TRUE
  }

  # Create title
  ttl <- paste0("Distribution of ", tbl1, " by ", tbl2)

  # Get original margins
  omar <- par()$mar
  oma <- par()$oma

  # Assign plot margins
  if (plt$orient == "vertical") {
    pmar <- c(0.5, 0, 0, 1) + 0.1
    cmar <- c(5, 5, 4, 2) + 0.1
  } else {
    pmar <- c(0.5, 0, 0, 0) + 0.1
    cmar <- c(5, 6, 4, 2) + 0.1
  }

  # Variables to control multiple plots
  firstplot <- TRUE
  lastplot <- FALSE
  pltcnt <- 0
  ret <- list()

  # Get distinct values
  v1 <- unique(dat$CAT1)
  v2 <- unique(dat$CAT2)

  # Loop on first table variable
  for (vl in v1) {

    # Increment plot counter
    if (pltcnt >= 3) {
      pltcnt <- 1
    } else {
      pltcnt <- pltcnt + 1
    }

    # Set first plot flag
    if (pltcnt == 1) {
      firstplot <- TRUE
    } else {
      firstplot <- FALSE
    }

    # Set last plot flag
    if (pltcnt == 3 | vl == v1[length(v1)]) {
      lastplot <- TRUE
    } else {
      lastplot <- FALSE
    }

    # Subset data
    dt <- dat[dat$CAT1 == vl, ]

    # Prepare data
    if (plt$scale == "percent") {

      cnt <- as.numeric(dt$PCT)
      v2nms <- as.character(dat$CAT2)
      slbl <- "Percent"
      scl <- c(0, max(as.numeric(dat$PCT)) * 1.05)

    } else if (plt$scale == "log") {

      cnt <- as.numeric(log10(dt$CNT))
      v2nms <- as.character(dt$CAT2)
      slbl <- "Log Frequency"
      scl <- c(0, max(as.numeric(log10(dat$CNT))) * 1.05)

    } else if (plt$scale == "sqrt") {

      cnt <- as.numeric(sqrt(dt$CNT))
      v2nms <- as.character(dt$CAT2)
      slbl <- "Sqrt Frequency"
      scl <- c(0, max(as.numeric(sqrt(dat$CNT))) * 1.05)

    } else {  # Default to Frequency

      cnt <- as.numeric(dt$CNT)
      v2nms <- as.character(dt$CAT2)
      slbl <- "Frequency"
      scl <- c(0, max(dat$CNT) * 1.05)
    }


    if (firstplot) {
      # Create temp file path
      pth <- tempfile(fileext = ".jpg")

      # Output to image file
      # All output types accept jpeg
      # So start with that
      jpeg(pth, width = wd, height = ht, quality = 100, antialias = "cleartype")

      # Set up for 3 charts on plot
      par(mfrow = c(3, 1), oma = cmar)

      # Put plot in reporter plot object
      pret <- create_plot(pth, height = hti, width = wdi)

      ret[[length(ret) + 1]] <- pret

    }


    if (plt$type == "dotplot") {  # Dot plot

      # X positions for categories
      xdat <- seq_along(cnt)

      if (plt$orient == "vertical") {

        # Low to high
        xlm <- c(.5, length(cnt) + .5)

        # Plot
        op <- par(mar = pmar)

        plot(
          xdat, cnt,
          type = "p",
          pch  = 16,
          col  = "#05379B",
          xaxt = "n",
          cex =  1.3, #1.5,
          xlim = xlm,
          ylim = scl,
          axes = FALSE,
          xlab = tbl2,
          ylab = slbl,
        )

        # Vertical dotted grid lines at each category
        abline(v = xdat, lty = "dotted", col = "gray85")

        # Axes
        axis(1, at = xdat, labels = names(cnt), col.ticks = "grey55")
        axis(2, las = 1, col.ticks = "grey55") # at = seq(0, 400, by = 100)

      } else {  # Horizontal

        # High to Low
        xlm <- c(length(cnt) + .5, .5)

        # Plot
        op <- par(mar = pmar)

        plot(
          cnt, xdat,
          type = "p",
          pch  = 16,
          col  = "#05379B",
          yaxt = "n",
          cex =  1.3, #1.5,
          xlim = scl,
          ylim = xlm,
          axes = FALSE,
          xlab = slbl,
          ylab = "",
        )

        mtext(tbl2, side = 2, line = op$mar[1] - 1)

        # Vertical dotted grid lines at each category
        abline(h = xdat, lty = "dotted", col = "gray85")

        # Axes
        axis(2, at = xdat, labels = names(cnt), las = 1, col.ticks = "grey55")
        axis(1, las = 1, col.ticks = "grey55") # at = seq(0, 400, by = 100)

      }

    } else {  # Bar chart

      if (horz == TRUE) {  # Horizontal

        # Set custom margins
        op <- par(mar = pmar)

        # Create empty plot
        b1 <- barplot(
          rep(NA, length(cnt)),  # Empty data
          xlab = slbl,  # Lable x axis
          #  ylab = tbl,  # Label y axis
          xlim = scl,  # x axis scale
          horiz = TRUE,
          axes = FALSE  # Don't create axis yet
        )


        mtext(tbl2, side = 2, line = op$mar[1] - 1)

        # Get tick marks for ablines
        a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
        # a2 <- axis(2, las = 1, col.ticks = "grey55")  # Create axis

        ## Add gridlines based on axis created above
        abline(v = a1, col = "grey90", lwd = 1)

      } else {  # Vertical

        # Set custom margins
        op <- par(mar = pmar)

        # Create empty plot
        b1 <- barplot(
          rep(NA, length(cnt)),  # Empty data
          xlab = tbl2,  # Lable x axis
          ylab = slbl,  # Label y axis
          ylim = scl,  # y axis scale
          horiz = FALSE,
          axes = FALSE  # Don't create axis yet
        )

        # Get tick marks for ablines
        # a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
        a2 <- axis(2, las = 1, col.ticks = "grey55", cex.axis = 1.2)  # Create axis

        ## Add gridlines based on axis created above
        abline(h = a2, col = "grey90", lwd = 1)

        # Add label
        mtext(paste(tbl1, "=", vl), side = 4, line = .25, cex = .9)
      }


      # Add barplot
      b2 <- barplot(
        cnt,   # Data
        horiz = horz,
        col  = bgcolor, # adjustcolor("grey80", alpha.f = 0.45),
        border = "grey55",
        las = 1,
        tick = TRUE,
        add = TRUE,   # Add to existing plot
        axes = FALSE  # Already created above
      )

    }

    ## Frame around each plot
    box(col = "grey70", lwd = 1)

    # Close device context
    if (lastplot) {

      # Add title
      mtext(ttl, side = 3, line = 1, outer = TRUE, font = 2, cex = 1.2)

      # Add labels
      mtext(slbl, side = 2, line = cmar[2] - 2, outer = TRUE)
      mtext(tbl2, side = 1, line = cmar[1] - 2, outer = TRUE)

      # Add axis
      axis(1, las = 1, col.ticks = "grey55", at = as.vector(b2),
           labels = v2, col = "grey70", cex.axis = 1.2)  # Create axis

      # Outer border
      box("outer", col = "grey70", lwd = 1)

      dev.off()
    }

  } # v1 loop

  # Restore margins
  par(mar = omar, mfrow = c(1, 1), oma = oma)

  # Take out list if only one
  if (length(ret) == 1) {
    ret <- ret[[1]]
  }

  return(ret)

}

#
#
# render_scatterplot <- function() {
#
#
#
#   cols <- ifelse(Sex == "M", "royalblue3", "indianred2")
#
#
#
#   hti <- 6  # Height in inches
#   wdi <- 7  # Width in inches
#   bml <- 6  # bottom margin lines
#
#   # Convert inches to pixels
#   ht <- hti * 96
#   wd <- wdi * 96
#
#   # Save old margins
#   oldmar <- par()$mar
#
#   # Set up jpeg output
#   jpeg(width = wd, height = ht, quality = 100, antialias = "cleartype")
#
#   # Set margins
#   par(mar = c(bml, 5, 4, 2) + 0.1)
#
#   # Create plot
#   plot(Height, Weight,
#        xlab = "Height",
#        ylab = "Weight",
#        pch  = 1,        # open circles
#        las = 1,  # Horizontal label orientation
#        #lab = c(7, 5, 7),  # Number of tick marks
#        col  = cols,  # Grouping is here
#   )
#
#   # Calculate inset y value
#   line_height_fraction <- (par("mai")[1] / par("pin")[2])
#   ins <- - (line_height_fraction * 0.9)
#
#   # Draw legend
#   legend("bottom",  # Starting location is bottom of plot area
#          legend = c("Sex", "M", "F"), # Variable name and value labels
#          col    = c("black", "royalblue3", "indianred2"),
#          pch    = c(NA, 1, 1),  # Don't show any symbol for variable name
#          x.intersp = c(0, 1, 1),  # Spacing between group label and box
#          y.intersp = 0,  # Spacing between content and borders
#          horiz  = TRUE,
#          bty    = "o",  # Single line border around legend
#          inset  = c(0, ins),  # Offset from bottom of plot area
#          box.col = "grey", # Grey border to match SAS
#          xpd    = TRUE   # Allow legend to leave plot area and appear in margin
#   )
#
#   # Restore margins
#   par(mar = oldmar)
#
#   # Turn off device context
#   dev.off()
#
#
#
#
#
# }





#' @importFrom stats aggregate
#' @import graphics
#' @noRd
# render_bar <- function(dat, cmd, plt, interactive = FALSE) {
#
#
#   # Get variables
#   xvar <- cmd$var
#   yvar <- cmd$response
#
#   # Get stat
#   st <- cmd$stat
#
#   # Get titles
#   ttls <- plt$titles
#
#   # Create counter variable
#   dat$cnt. <- 1
#
#   # Get grouping variables
#   grps <- cmd$group
#
#   # Get group values
#   grpvals <- NULL
#   if (!is.null(grps)) {
#
#     grpvals <- unique(dat[[grps]])
#   }
#
#   # Colors - for now
#   if (length(grps) == 0) {
#     cols <- "grey90"
#   } else {
#     cols <- c("#7B8CC3",   # blue-ish
#               "#DC6E6C",   # red-ish
#               "#5FB7A8",   # teal
#               "#B08D52"    # brown/gold
#     )
#
#     cols <- cols[seq(1, length(grpvals))]
#   }
#
#   # Clustered display
#   bsd <- FALSE
#   if (!is.null(cmd$groupdisplay)) {
#     if (tolower(cmd$groupdisplay) == "cluster") {
#       bsd <- TRUE
#     }
#   }
#
#   # No response, use the count variable, no statistic
#   if (is.null(yvar)) {
#     yvar <- "cnt."
#   }
#
#   if (is.null(st)) {
#     st <- "sum"
#   }
#
#   byvars <- list(dat[[xvar]])
#   grpvars <- NULL
#   if (!is.null(grps)) {
#     for (grp in grps) {
#       byvars[[length(byvars) + 1]] <- dat[[grp]]
#       grpvars[[length(grpvars) + 1]] <- paste0("GRP", length(grpvars) + 1)
#     }
#     mrg <- par()$mar
#     mrg[1] <- mrg[1] + 1
#     par(mar = mrg)
#   }
#
#   # Sum by variables
#   sdat <- aggregate(dat[[yvar]], by = byvars, FUN = st)
#   names(sdat) <- c("CAT", grpvars, "FREQ")
#
#   # print("sdat")
#   # print(sdat)
#
#   # Get x names
#   xnms <- unique(sdat$CAT)
#
#   if (is.null(grps)) {
#     ynms <- ""
#     mdat <- sdat$FREQ
#   } else {
#
#     # Get y names
#     ynms <- unique(sdat$GRP1)
#
#     # Create matrix
#     mdat <- sdat$FREQ
#     dim(mdat) <- c(length(xnms), length(ynms))
#     dimnames(mdat) <- list(xnms, ynms)
#
#     # transpose
#     mdat <- t(mdat)
#   }
#
#   # Create scale
#   # yl <- c(0, max(sdat$FREQ))
#
#   # print("mdat")
#   # print(mdat)
#   # print("xnms")
#   # print(xnms)
#   # print("xvar")
#   # print(xvar)
#
#   lgnd <- FALSE
#   if (!is.null(grps)) {
#     lgnd <- TRUE
#   }
#
#   # If no output file and not interactive, don't run.
#   # Will be necessary to pass CRAN checks.
#   if ((interactive & interactive()) |
#       !interactive) {
#
#     # Bar chart
#     barplot(mdat,
#             names.arg = xnms,
#             main = ttls,
#             beside = bsd,
#             xlab = xvar,
#             ylab = "Frequency",
#             #  ylim = yl,
#             col = cols,
#             border = "grey20",
#             legend.text = lgnd
#     )
#
#     # This will take some work
#     # Comment out for now
#     # Problem is getting it placed in the bottom margin
#     # R doesn't do it that way by default
#     # Will have to custom program it
#     # if (!is.null(grps)) {
#     #
#     #   legend(
#     #   )
#   }
#
#   # }
#
# }


# Cumfreqplot -------------------------------------------------------------



# @import ggplot2
# @noRd
# gen_summarypanel <- function(dta, var1, var2 = NULL, confidence = NULL) {
#
#   lblX <- var1
#   lblY <- "Percent"
#   ttl <- 'Distribution of ' %p% var1
#   subttl <- NULL
#   # if (!is.null(confidence))
#   #   subttl <- "With " %p% confidence %p% "% Confidence Interval for Mean"
#   v1 <- dta[[var1]]
#
#   brks <- get_breaks(v1)
#   bins <- length(brks) - 1
#
#
#   ret <- ggplot(dta, aes(x=.data[[var1]])) +
#     geom_histogram(aes(y = after_stat(count)/sum(after_stat(count)) * 100),
#                    breaks= brks,
#                    colour="black", fill="#CAD5E5") +
#     theme(plot.title = element_text(size = 10, face="bold", lineheight = .2, hjust = .5),
#           plot.subtitle = element_text(size = 8, hjust = .5, lineheight = .2),
#           axis.title.x = element_text(size = 8),
#           axis.title.y = element_text(size = 8),
#           axis.text.x = element_text(size = 8),
#           axis.text.y = element_text(size = 8)) +
#     labs(title = ttl, x = lblX, y = lblY, subtitle = subttl) +
#     stat_function(fun = function(x) dnorm(x, mean = mean(v1), sd = sd(v1)) * 100 * bins,
#                   color = "#6383C0", linewidth = .75)
#
#
#
#
#   return(ret)
#
#
# }

get_breaks <- function(var1) {


  bins <- round(diff(range(var1)) / (2 * IQR(var1) / length(var1)^(1/3)))

  mn <- floor(min(var1))
  mx <- ceiling(max(var1))
  me <- mean(var1)


  #ret <- quantile(c(mn, mx), breaks=1/bins*seq(1, bins))

  if (length(unique(var1)) < 10) {

    ret <- sort(unique(var1))

  } else {

    ret <- ((mx - mn)/bins)*seq(1, bins)

    ret <- mn + ret

    diff <- ret[2] - ret[1]

    if (diff + ret[length(ret)] > mx)
      ret <- c(mn, ret)
    else
      ret <- c(mn, ret, diff + ret[length(ret)])


    if (diff >= 5)
      ret <- unique(round(ret / 5) * 5)
    else
      ret <- unique(round(ret))

  }

  print(ret)

  return(ret)
}


# @import ggplot2
gen_qqplot <- function(var1, var2 = NULL) {

  # lblX <- names(var1)
  # lblY <- "Percent"
  # bins <- 5
  # brks <- c(50, 55, 60, 65, 70, 75)
  #
  #
  # ret <- ggplot(var1, aes(x=lblX)) +
  #   geom_histogram(aes(y = (..count..)/sum(..count..) * 100), breaks= brks, colour="black", fill="#CAD5E5") +
  #   stat_function(fun = function(x) dnorm(x, mean = mean(var1), sd = sd(var1)) * 100 * bins, color = "#6383C0", size = .75)
  #
  #
  # return(ret)
}


#
#
# set.seed(1234)
# dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
#                   rating = c(rnorm(200),rnorm(200, mean=.8)))
# # View first few rows
# head(dat)
# #>   cond     rating
# #> 1    A -1.2070657
# #> 2    A  0.2774292
# #> 3    A  1.0844412
# #> 4    A -2.3456977
# #> 5    A  0.4291247
# #> 6    A  0.5060559
#
# library(ggplot2)
#
#
#
# ## Basic histogram from the vector "rating". Each bin is .5 wide.
# ## These both result in the same output:
# ggplot(dat, aes(x=rating)) + geom_histogram(binwidth=.5)
# # qplot(dat$rating, binwidth=1)
#
# # Draw with black outline, white fill






 # geom_density(adjust = 1.1) +
#
# # Density curve
# ggplot(cls, aes(x=Height)) + geom_density(adjust = 1.3)
#
# # Histogram overlaid with kernel density curve
# ggplot(dat, aes(x=rating)) +
#   geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
#                  binwidth=.5,
#                  colour="black", fill="white") +
#   geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
#

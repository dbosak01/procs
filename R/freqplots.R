

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
#' frequency plot to produce, and various layout options. It supports
#' bar charts and dot plots for one and two-way analysis.  It also supports
#' vertical or horizontal orientation, by variables, and various scale options.
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
#' @param groupby The variable configuration for two-way charts.
#' Valid values are "column" or "row". Default is "column". The "row" option
#' effectively reverses the variable configuration.
#' @param npanelpos The number of charts per panel. Default is 4. This
#' parameter applies to two-way tables only.
#' @returns The frequency plot object.  This object is then passed to
#' \code{\link{proc_freq}} for evaluation and rendering. Data from the
#' frequency plot comes directly from the \code{\link{proc_freq}} reporting
#' data frame.
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
freqplot <- function(type = "barchart", orient = "vertical", scale = "freq",
                     twoway = "groupvertical", groupby = "column", npanelpos = 4) {

  # Non-standard evaluation

  otype <- deparse(substitute(type, env = environment()))
  type <- tryCatch({if (typeof(type) %in% c("character", "NULL")) type else otype},
                     error = function(cond) {otype})

  oorient <- deparse(substitute(orient, env = environment()))
  orient <- tryCatch({if (typeof(orient) %in% c("character", "NULL")) orient else oorient},
                     error = function(cond) {oorient})

  oscale <- deparse(substitute(scale, env = environment()))
  scale <- tryCatch({if (typeof(scale) %in% c("character", "NULL")) scale else oscale},
                     error = function(cond) {oscale})

  otwoway <- deparse(substitute(twoway, env = environment()))
  twoway <- tryCatch({if (typeof(twoway) %in% c("character", "NULL")) twoway else otwoway},
                    error = function(cond) {otwoway})

  ogroupby <- deparse(substitute(groupby, env = environment()))
  groupby <- tryCatch({if (typeof(groupby) %in% c("character", "NULL")) groupby else ogroupby},
                     error = function(cond) {ogroupby})

  # Parameter Checks
  if (!type %in% c("barchart", "dotplot")) {
    stop("Parameter value for 'type' invalid. Valid values are 'barchart' or 'dotplot'.")
  }

  if (!orient %in% c("vertical", "horizontal")) {
    stop("Parameter value for 'orient' invalid. Valid values are 'vertical' or 'horizontal'.")
  }

  if (!scale %in% c("freq", "percent", "log", "sqrt", "grouppercent")) {
    stop(paste0("Parameter value for 'scale' invalid. Valid values are 'freq' ",
        "'percent', 'log', 'sqrt', or 'grouppercent'."))
  }

  if (!twoway %in% c("grouphorizontal", "groupvertical", "stacked", "cluster")) {
    stop(paste0("Parameter value for 'type' invalid. Valid values are 'groupvertical', ",
    "'grouphorizontal', 'stacked', or 'cluster'."))
  }

  if (!groupby %in% c("column", "row")) {
    stop("Parameter value for 'groupby' invalid. Valid values are 'column' or 'row'.")
  }

  if (!is.numeric(npanelpos)) {
    stop("Parameter value for 'npanelpos' invalid. Value must be an integer.")
  }

  # Create object
  ret <- structure(list(), class = c("freqplot", "list"))

  ret$groupby <- tolower(groupby)
  ret$npanelpos <- as.integer(npanelpos)
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



render_freqplot.1way <- function(dat, tbl, plt) {

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
  jpeg(pth, width = wd, height = ht, quality = 100, units = "px")

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

  # Get number of lines needed to show labels
  minlns <- get_line_count(names(cnt)) + 1

  if (plt$type == "dotplot") {  # Dot plot

    blbllns <- 1

    # X positions for categories
    xdat <- seq_along(cnt)

    if (plt$orient == "vertical") {

      # Low to high
      xlm <- c(.5, length(cnt) + .5)

      # Plot
      par(mar = c(5, 5, 2, .75) + 0.1)

      # Estimate width of plot
      twdth <- wdi - (par("mai")[2] + par("mai")[4])

      # Estimate width of bars
      bwdth <- twdth / length(cnt)

      # Process labels
      fw <- fit_width(names(cnt), bwdth)

      # Update labels
      names(cnt) <- fw$Vector

      # Calculate bottom width
      if (fw$Lines > 1) {
        bmg <- 4 + fw$Lines
        blbllns <- fw$Lines
      } else {
        bmg <- 5
      }

      # Set custom margins
      par(mar = c(bmg, 5, 2, .75) + 0.1)


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
        xlab = "",
        ylab = slbl,
        ps = 11,
        main = paste0("Distribution of ", tbl)
      )

      # X Axis label
      mtext(tbl, side = 1, line = par()$mar[1] - 2)

      # Vertical dotted grid lines at each category
      abline(v = xdat, lty = "dotted", col = "gray85")

      # Axes
      axis(1, at = xdat, labels = names(cnt), col.ticks = "grey55", mgp = c(3, blbllns, 0))
      axis(2, las = 1, col.ticks = "grey55")

    } else {  # Horizontal

      # Calculate left margin lines
      if (minlns > 3) {
        lml <- minlns + 3
      } else {
        lml <- 6
      }

      # High to Low
      xlm <- c(length(cnt) + .5, .5)

      # Plot
      par(mar = c(5, lml, 2, .75) + 0.1)

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
        ps = 11,
        main = paste0("Distribution of ", tbl)
      )

      mtext(tbl, side = 2, line = par()$mar[2] - 2)

      # Vertical dotted grid lines at each category
      abline(h = xdat, lty = "dotted", col = "gray85")

      # Axes
      axis(2, at = xdat, labels = names(cnt), las = 1, col.ticks = "grey55")
      axis(1, las = 1, col.ticks = "grey55")

    }

  } else {  # Bar chart

    blbllns <- 1

    if (horz == TRUE) {  # Horizontal

      # Calculate left margin lines
      if (minlns > 3) {
        lml <- minlns + 3
      } else {
        lml <- 6
      }

      # Set custom margins
      par(mar = c(5, lml, 2, .75) + 0.1)

      # Create empty plot
      b1 <- barplot(
        rep(NA, length(cnt)),  # Empty data
        main = paste0("Distribution of ", tbl),  # Title
        xlab = slbl,  # Lable x axis
        #  ylab = tbl,  # Label y axis
        xlim = c(0, max(cnt) * 1.05),  # x axis scale
        horiz = TRUE,
        ps = 11,
        axes = FALSE  # Don't create axis yet
      )


      mtext(tbl, side = 2, line = par()$mar[2] - 2)

      # Get tick marks for ablines
      a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
      # a2 <- axis(2, las = 1, col.ticks = "grey55")  # Create axis

      ## Add gridlines based on axis created above
      abline(v = a1, col = "grey90", lwd = 1)

    } else {  # Vertical

      # Set margins to get estimated space for bars
      par(mar = c(5, 5, 2, .75) + 0.1)

      # Estimate width of plot
      twdth <- wdi - (par()$mai[2] + par()$mai[4])

      # Estimate width of bars
      bwdth <- twdth / length(cnt)

      # Process labels
      fw <- fit_width(names(cnt), bwdth)

      # Update labels
      names(cnt) <- fw$Vector

      # Calculate bottom width
      if (fw$Lines > 1) {
        bmg <- 4 + fw$Lines
        blbllns <- fw$Lines
      } else {
        bmg <- 5
      }

      # Set custom margins
      par(mar = c(bmg, 5, 2, .75) + 0.1)


      # Create empty plot
      b1 <- barplot(
        rep(NA, length(cnt)),  # Empty data
        main = paste0("Distribution of ", tbl),  # Title
        xlab = "",  # Lable x axis
        ylab = slbl,  # Label y axis
        ylim = c(0, max(cnt) * 1.05),  # y axis scale
        horiz = FALSE,
        ps = 11,
        axes = FALSE  # Don't create axis yet
      )

      # X Axis label
      mtext(tbl, side = 1, line = par()$mar[1] - 2)

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
     # tick = TRUE,
      mgp = c(3, blbllns, 0),
      ps = 11,
      add = TRUE,   # Add to existing plot
      axes = FALSE  # Already created above
    )

  }


  # frame
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

render_freqplot.2way <- function(dat, tbl1, tbl2, plt) {

  if (plt$twoway %in% c("cluster", "stacked")) {

    ret <- render_freqplot.nongroup(dat, tbl1, tbl2, plt)

  } else {

    ret <- render_freqplot.group(dat, tbl1, tbl2, plt)
  }


  return(ret)
}

#' @noRd
render_freqplot.group <- function(dat, tbl1, tbl2, plt) {

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6  # bottom margin lines
  tml <- 3  # Top margin lines

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

  # Deal with groupby parameter.
  # If groupby is "row", then flip variables
  if (plt$groupby == "row") {
    tmp <- tbl1
    tbl1 <- tbl2
    tbl2 <- tmp
  }

  # Create title
  ttl <- paste0("Distribution of ", tbl1, " by ", tbl2)

  # Get original margins
  opar <- par(no.readonly = TRUE)

  # Assign plot margins
  if (plt$orient == "vertical") {
    cmar <- c(5, 5, tml, 2) + 0.1
  } else {
    cmar <- c(5, 6, tml, 2) + 0.1
  }

  # Variables to control multiple plots
  firstplot <- TRUE
  lastplot <- FALSE
  pltcnt <- 0
  ret <- list()

  # Assign Cat variables
  # to get orientation correct.
  # SAS is inconsistent.
  if (plt$type == "dotplot" | plt$twoway == "grouphorizontal") {
    if (plt$orient == "vertical") {
      var1 <- "CAT2"
      var2 <- "CAT1"
    } else {
      var1 <- "CAT2"
      var2 <- "CAT1"
    }
  } else {
    if (plt$orient == "vertical") {
      var1 <- "CAT1"
      var2 <- "CAT2"
    } else {
      var1 <- "CAT2"
      var2 <- "CAT1"
    }
  }

  # Also flip variables
  if (plt$groupby == "row") {
    tmp <- var1
    var1 <- var2
    var2 <- tmp
  }

  # Get distinct values
  v1 <- unique(dat[[var1]])
  v2 <- unique(dat[[var2]])

  # Plots per panel
  if (length(v1) <= plt$npanelpos) {
    pltmax <- length(v1)
  } else {
    pnls <- ceiling(length(v1) / plt$npanelpos)
    pltmax <-   ceiling(length(v1) / pnls)
  }

  # Loop on first table variable
  for (vl in v1) {

    # Increment plot counter
    if (pltcnt >= pltmax) {
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
    if (pltcnt == pltmax | vl == v1[length(v1)]) {
      lastplot <- TRUE
    } else {
      lastplot <- FALSE
    }

    # Subset data
    dt <- dat[dat[[var1]] == vl, ]

    # Reverse order for dotplot.
    # Don't know why.
    if (plt$type == "dotplot" & plt$orient == "horizontal") {
      dt$seq <- seq(nrow(dt), 1)
      dt <- sort(dt, by = "seq")
      v2 <- unique(dat[[var2]])
    }


    # Prepare data
    if (plt$scale == "percent") {

      cnt <- as.numeric(dt$PCT)
      slbl <- "Percent"
      scl <- c(0, max(as.numeric(dat$PCT)) * 1.05)

    } else if (plt$scale == "log") {

      cnt <- as.numeric(log10(dt$CNT))
      slbl <- "Log Frequency"
      scl <- c(0, max(as.numeric(log10(dat$CNT))) * 1.05)

    } else if (plt$scale == "sqrt") {

      cnt <- as.numeric(sqrt(dt$CNT))
      slbl <- "Sqrt Frequency"
      scl <- c(0, max(as.numeric(sqrt(dat$CNT))) * 1.05)

    } else if (plt$scale == "grouppercent") {

      if (plt$type == "barchart") {
        # Get group max
        mx <- aggregate(dat$CNT, by = list(dat[[var2]]), FUN = max, simplify = TRUE)

        # Get group totals
        tot <- aggregate(dat$CNT, by = list(dat[[var2]]), FUN = sum, simplify = TRUE)

        # Calculate group percentages
        cnt <- as.numeric(dt$CNT) / tot$x * 100

      } else {

        # Get group max
        mx <- aggregate(dat$CNT, by = list(dat[[var1]]), FUN = max, simplify = TRUE)

        # Get group totals
        tot <- aggregate(dat$CNT, by = list(dat[[var1]]), FUN = sum, simplify = TRUE)

        # Calculate group percentages
        cnt <- as.numeric(dt$CNT) / tot$x[match(vl, v1)] * 100
      }


      slbl <- paste0("Percent of ", tbl2, " Frequency")
      scl <- c(0, max(mx$x / tot$x * 100) * 1.05)

    } else {  # Default to Frequency

      cnt <- as.numeric(dt$CNT)
      slbl <- "Frequency"
      scl <- c(0, max(dat$CNT) * 1.05)
    }

    # Label variables
    if (var1 == "CAT1") {
      mlbl <- tbl1
      if (plt$orient == "vertical") {
        blbl <- tbl2
        llbl <- slbl
      } else {
        blbl <- slbl
        llbl <- tbl2
      }
    } else {
      mlbl <- tbl2
      if (plt$orient == "vertical") {
        blbl <- tbl1
        llbl <- slbl
      } else {
        blbl <- slbl
        llbl <- tbl1
      }
    }

    # Not happy about this
    # But it works
    if (plt$groupby == "row") {
      tmp1 <- mlbl
      tmp2 <- blbl
      tmp3 <- llbl
      if (plt$orient == "vertical") {
        mlbl <- tmp2
        blbl <- tmp1
      } else {
        mlbl <- tmp3
        llbl <- tmp1
      }
    }

    # Names for variable 2
    v2nms <- as.character(dt[[var2]])

    # Get number of lines needed to show labels
    minlns <- get_line_count(v2nms) + 1


    if (firstplot) {
      # Create temp file path
      pth <- tempfile(fileext = ".jpg")

      # Output to image file
      # All output types accept jpeg
      # So start with that
      jpeg(pth, width = wd, height = ht, quality = 100)

      # Set up for 3 charts on plot
      if (plt$twoway == "groupvertical") {
        par(mfrow = c(pltmax, 1), oma = cmar)
      } else {
        par(mfrow = c(1, pltmax), oma = cmar)
      }

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
        if (plt$twoway == "groupvertical") {
          op <- par(mar = c(0.5, 0, 1, 0) + 0.1)
        } else {
          op <- par(mar = c(0.5, 0, 1, .5) + 0.1)
        }

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
          # xlab = tbl2,
          ylab = slbl,
        )

        # Plot positions
        p2 <- xdat

        # Vertical dotted grid lines at each category
        abline(v = xdat, lty = "dotted", col = "gray85")

        # Axes
        if (plt$twoway == "grouphorizontal") {
          # Left axis
          if (firstplot) {
            axis(2, las = 1, col.ticks = "grey55")
          }
          # Bottom axis
          axis(1, las = 1, col.ticks = "grey55", at = as.vector(p2),
               labels = v2, col = "grey70")

        } else if (plt$twoway == "groupvertical") {
          # Left axis
          axis(2, las = 1, col.ticks = "grey55")
        }

        # Tbl1 Label
        mtext(paste(mlbl, "=", vl), side = 3, line = .3)

        # Add axis
        if (lastplot) {
          if (plt$twoway == "groupvertical") {

            # Estimate width of plot
            twdth <- wdi - par()$omi[2] - par()$omi[4] - par()$mai[2] - par()$mai[4]

            # Estimate width of bars
            bwdth <- twdth / length(v2)

            # Process labels
            fw <- fit_width(v2, bwdth)

            # Update labels
            nlbls <- fw$Vector

            # Calculate bottom width
            blbllns <- 1
            if (fw$Lines > 1) {
              blbllns <- fw$Lines
            }

            # Set custom margins
            #op <- par(mar = c(5, 5, 2, .75) + 0.1)
              axis(1, las = 1, col.ticks = "grey55", at = as.vector(p2),
                   labels = nlbls, col = "grey70", mgp = c(3, blbllns, 0))  # Create axis
          }
        }


      } else {  # Horizontal

        # High to Low
        xlm <- c(length(cnt) + .5, .5)

        lmg <- 2
        if (minlns > 4) {
          lmg <- minlns - 3
        }

        # Plot
        op <- par(mar = c(0.5, lmg, 1, 0) + 0.1)

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
          xlab = "",
          ylab = "",
        )

        # Plot positions
        p2 <- xdat

        # Tbl1 Label
        mtext(paste(mlbl, "=", vl), side = 3, line = .3)

        # Vertical dotted grid lines at each category
        abline(h = xdat, lty = "dotted", col = "gray85")

        # Axes
        if (plt$twoway == "groupvertical") {

          # Left axis
          axis(2, at = xdat, labels = v2nms, las = 1, col.ticks = "grey55")

          # Axis on last plot
          if (lastplot) {
            axis(1, las = 1, col.ticks = "grey55",
                 col = "grey70")
          }

        } else {

          # Left axis
          if (firstplot) {
            axis(2, at = xdat, labels = v2nms, las = 1, col.ticks = "grey55")
          }

          # Bottom axis
          axis(1, las = 1, col.ticks = "grey55",
               col = "grey70")
        }



      }

    } else {  # Bar chart

      if (horz == TRUE) {  # Horizontal

        lmg <- 2
        if (minlns > 4) {
          lmg <- minlns - 3
        }

        # Set custom margins
        op <- par(mar =  c(0.5, lmg, 1, 0) + 0.1)

        # Create empty plot
        p1 <- barplot(
          rep(NA, length(cnt)),  # Empty data
          xlab = "",  # Lable x axis
          #  ylab = tbl,  # Label y axis
          xlim = scl,  # x axis scale
          horiz = TRUE,
          axes = FALSE  # Don't create axis yet
        )

        # # Create axis
        if (plt$twoway == "groupvertical" & lastplot) {
          a1 <- axis(1, las = 1, col.ticks = "grey55",
                     col = "grey70")
        } else if (plt$twoway == "grouphorizontal") {

          a1 <- axis(1, las = 1, col.ticks = "grey55",
                     col = "grey70")
        } else {
          a1 <- axis(1, labels = FALSE, tick = FALSE)
        }

        ## Add gridlines based on axis created above
        abline(v = a1, col = "grey90", lwd = 1)

        # Middle Label
        mtext(paste(mlbl, "=", vl), side = 3, line = .3)

        # Left axis
        if (plt$twoway == "grouphorizontal") {
          if (firstplot) {
            axcex <- 1 + (1 - par("cex"))
            axis(2, at = p1, labels = v2nms, las = 1,
                 col.ticks = "grey55", cex.axis = axcex)
          }
        }

        # Left axis
        if (plt$twoway == "groupvertical") {
          if (plt$orient == "horizontal") {
            axcex <- 1 + (1 - par("cex"))
            axis(2, at = p1, labels = v2nms, las = 1,
                 col.ticks = "grey55", cex.axis = axcex)
          }
        }

      } else {  # Vertical

        lblwdth <- 0

        # Set custom margins
        if (plt$twoway == "groupvertical") {
          op <- par(mar = c(0.75, 1, 0, 1) + 0.1)
        } else {
          op <- par(mar = c(0.75, 1, 1, .5) + 0.1)
        }


        # Create empty plot
        p1 <- barplot(
          rep(NA, length(cnt)),  # Empty data
          xlab = blbl,  # Label x axis
          ylab = slbl,  # Label y axis
          ylim = scl,  # y axis scale
          horiz = FALSE,
          axes = FALSE  # Don't create axis yet
        )

        # Get tick marks for ablines
        # a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
        if (plt$twoway == "groupvertical" |
            (plt$twoway == "grouphorizontal" & firstplot)) {
          a2 <- axis(2, las = 1, col.ticks = "grey55")  # Create axis
        }

        ## Add gridlines based on axis created above
        abline(h = a2, col = "grey90", lwd = 1)

        # Add label
        if (plt$twoway == "groupvertical") {

          lblwdth <- (hti - par("omi")[1] - par("omi")[3]) / pltmax

          lbl <- force_width(paste(mlbl, "=", vl), lblwdth * 1.2)

          mtext(lbl, side = 4, line = .25)
        } else {

          lblwdth <- (wdi - par("omi")[2] - par("omi")[4]) / pltmax

          lbl <- force_width(paste(mlbl, "=", vl), lblwdth * 1.2)

          mtext(lbl, side = 3, line = .25)
        }

      }


      # Add barplot
      p2 <- barplot(
        cnt,   # Data
        horiz = horz,
        col  = bgcolor, # adjustcolor("grey80", alpha.f = 0.45),
        border = "grey55",
        las = 1,
        tick = TRUE,
        add = TRUE,   # Add to existing plot
        axes = FALSE  # Already created above
      )

      if (plt$twoway == "grouphorizontal" & plt$orient == "vertical") {
        axis(1, las = 1, col.ticks = "grey55", at = as.vector(p2),
             labels = v2, col = "grey70")  # Create axis
      }

      # Add axis
      if (lastplot) {
        if (plt$twoway == "groupvertical") {
          if (plt$orient == "vertical") {
            axcex <- 1 + (1 - par("cex"))
            axis(1, las = 1, col.ticks = "grey55", at = as.vector(p2),
                 labels = v2, col = "grey70",
                 cex.axis = axcex)  # Create axis
          }
        }
      }
    }

    ## Frame around each plot
    box(col = "grey70", lwd = 1)

    # Close device context
    if (lastplot) {

      # Add title
      mtext(ttl, side = 3, line = 1, outer = TRUE, font = 2)

      # Left label
      mtext(llbl, side = 2, line = cmar[2] - 2, outer = TRUE)

      # Adjustment if last chart is not filled up
      if (plt$twoway == "groupvertical") {
        lbladj <- 2 + ((pltmax - pltcnt) * 12)
      } else {
        lbladj <- 2
      }

      # Bottom label
      mtext(blbl, side = 1, line = cmar[1] - lbladj, outer = TRUE)

      # Outer border
      box("outer", col = "grey70", lwd = 1)

      dev.off()
    }

  } # v1 loop

  # Restore margins
  par(mar = opar$mar, mfrow = opar$mfrow, oma = opar$oma)

  # Take out list if only one
  if (length(ret) == 1) {
    ret <- ret[[1]]
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
# render_freqplot.cluster <- function(dat, tbl1, tbl2, plt) {
#
#   # Create temp file path
#   pth <- tempfile(fileext = ".jpg")
#
#   # Standard height and width
#   hti <- 4.5  # Height in inches
#   wdi <- 6  # Width in inches
#   bml <- 6  # bottom margin lines
#
#   # Convert inches to pixels
#   ht <- hti * 96
#   wd <- wdi * 96
#
#   # Set bar background color
#   bgcolor <- "#E5EAF2"
#   bgcolor <- c("royalblue3", "indianred2", "seagreen3")
#
#   # Set orientation
#   horz <- FALSE  # Default to vertical
#   if (plt$orient == "horizontal") {
#     horz <- TRUE
#   }
#
#   # Create Title
#   ttl <- paste0("Distribution of ", tbl1, " by ", tbl2)
#
#   # Output to image file
#   # All output types accept jpeg
#   # So start with that
#   jpeg(pth, width = wd, height = ht, quality = 100)
#
#   # Assign Cat variables
#   # to get orientation correct.
#   # SAS is inconsistent.
#   if (plt$type == "dotplot") {
#     if (plt$orient == "vertical") {
#       var1 <- "CAT2"
#       var2 <- "CAT1"
#     } else {
#       var1 <- "CAT2"
#       var2 <- "CAT1"
#     }
#   } else {
#     if (plt$orient == "vertical") {
#       var1 <- "CAT1"
#       var2 <- "CAT2"
#     } else {
#       var1 <- "CAT1"
#       var2 <- "CAT2"
#     }
#   }
#
#   # Get distinct values
#   v1 <- unique(dat[[var1]])
#   v2 <- unique(dat[[var2]])
#   dtm <- NULL
#   dtna <- NULL
#   mx <- 0
#
#   # Create matrix
#   # v1 in rows and v2 in columns
#   for (vl in v2) {
#
#     dt <- dat[dat[[var2]] == vl, ]
#
#     # Prepare data
#     if (plt$scale == "percent") {
#
#       cnt <- as.numeric(dt$PCT)
#       slbl <- "Percent"
#
#     } else if (plt$scale == "log") {
#
#       cnt <- as.numeric(log10(dt$CNT))
#       slbl <- "Log Frequency"
#
#     } else if (plt$scale == "sqrt") {
#
#       cnt <- as.numeric(sqrt(dt$CNT))
#       slbl <- "Sqrt Frequency"
#
#     } else {  # Default to Frequency
#
#       cnt <- as.numeric(dt$CNT)
#       slbl <- "Frequency"
#
#     }
#
#     nas <- rep(NA, length(cnt))
#     mx <- max(c(mx, cnt))
#
#     if (is.null(dtm)) {
#       dtm <- data.frame(cnt)
#       dtna <- data.frame(nas)
#     } else {
#       dtm <- cbind(dtm, cnt)
#       dtna <- cbind(dtna, nas)
#     }
#   }
#
#   # Assign names
#   names(dtm) <- NULL
#   rownames(dtm) <- v1
#
#   # NA data names
#   names(dtna) <- NULL
#   rownames(dtna) <- v1
#
#   # Convert to matrix
#   dtm <- as.matrix(dtm)
#   dtna <- as.matrix(dtna)
#
#   # Assign labels
#   if (plt$orient == "vertical") {
#     blbl <- tbl2
#     llbl <- slbl
#   } else {
#     blbl <- slbl
#     llbl <- tbl2
#   }
#
#   # Get scale
#   scl <- c(0, mx * 1.05)
#
#   # Get original margins
#   omar <- par()$mar
#
#   if (plt$type == "dotplot") {
#     stop("Two-way cluster option is not available for dot type frequency plots.")
#   } else {  # Bar chart
#
#     if (horz == TRUE) {  # Horizontal
#
#       # Set custom margins
#       op <- par(mar = c(5, 6, 4, 2) + 0.1)
#
#       # Create empty plot
#       b1 <- barplot(
#         dtna,  # Empty data
#         beside = TRUE,
#         main = ttl,  # Title
#         xlab = blbl,  # Lable x axis
#         #  ylab = tbl,  # Label y axis
#         xlim = scl,  # x axis scale
#         horiz = TRUE,
#         axes = FALSE  # Don't create axis yet
#       )
#
#
#       mtext(llbl, side = 2, line = op$mar[1] - 1)
#
#       # Get tick marks for ablines
#       a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
#       # a2 <- axis(2, las = 1, col.ticks = "grey55")  # Create axis
#
#       ## Add gridlines based on axis created above
#       abline(v = a1, col = "grey90", lwd = 1)
#
#     } else {  # Vertical
#
#       # Set custom margins
#       op <- par(mar = c(5, 5, 4, 2) + 0.1)
#
#       # Create empty plot.
#       # xlim created automatically
#       # and passed to next barplot
#       b1 <- barplot(
#         dtna,  # Empty data
#         beside = TRUE,
#         main = ttl,  # Title
#         xlab = blbl,  # Lable x axis
#         ylab = llbl,  # Label y axis
#         ylim = scl,  # y axis scale
#         horiz = FALSE,
#         axes = FALSE  # Don't create axis yet
#       )
#
#       # Get tick marks for ablines
#       # a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
#       a2 <- axis(2, las = 1, col.ticks = "grey55")  # Create axis
#
#       ## Add gridlines based on axis created above
#       abline(h = a2, col = "grey90", lwd = 1)
#     }
#
#
#     # Add barplot
#     b2 <- barplot(
#       dtm,   # Data
#       beside = TRUE,
#       horiz = horz,
#       names.arg = v2,
#       col  = bgcolor,
#       border = "grey55",
#       las = 1,
#       tick = TRUE,
#       add = TRUE,   # Add to existing plot
#       axes = FALSE  # Already created above
#     )
#
#   }
#
#
#   ## frame
#   box(col = "grey70", lwd = 1)
#   box("figure", col = "grey70", lwd = 1)
#
#   # Restore margins
#   par(mar = omar)
#
#   # Close device context
#   dev.off()
#
#   # Put plot in reporter plot object
#   ret <- create_plot(pth, height = hti, width = wdi)
#
#   return(ret)
#
# }


render_freqplot.nongroup <- function(dat, tbl1, tbl2, plt) {

  # Create temp file path
  pth <- tempfile(fileext = ".jpg")

  # Standard height and width
  hti <- 4.5  # Height in inches
  wdi <- 6  # Width in inches
  bml <- 6.6  # bottom margin lines
  tml <- 2    # Top margin lines

  # Convert inches to pixels
  ht <- hti * 96
  wd <- wdi * 96

  # Capture original par settings
  opar <- par(no.readonly = TRUE)

  # Set orientation
  horz <- FALSE  # Default to vertical
  if (plt$orient == "horizontal") {
    horz <- TRUE
  }

  # Set cluster flag
  clust <- TRUE
  if (plt$twoway == "stacked") {
    clust <- FALSE
  }

  # Deal with groupby parameter.
  # If groupby is "row", then flip variables
  if (plt$groupby == "row") {
    tmp <- tbl1
    tbl1 <- tbl2
    tbl2 <- tmp
  }

  # Create Title
  ttl <- paste0("Distribution of ", tbl1, " by ", tbl2)

  # Output to image file
  # All output types accept jpeg
  # So start with that
  jpeg(pth, width = wd, height = ht, quality = 100)

  # Assign Cat variables
  # to get orientation correct.
  # SAS is inconsistent.
  if (plt$type == "dotplot") {
    if (plt$orient == "vertical") {
      var1 <- "CAT1"
      var2 <- "CAT2"
    } else {
      var1 <- "CAT1"
      var2 <- "CAT2"
    }
  } else {
    if (plt$orient == "vertical") {
      var1 <- "CAT1"
      var2 <- "CAT2"
    } else {
      var1 <- "CAT1"
      var2 <- "CAT2"
    }
  }

  # Reverse if groupby = row
  if (plt$groupby == "row") {
    tmp <- var1
    var1 <- var2
    var2 <- tmp
  }

  # Get distinct values
  v1 <- unique(dat[[var1]])
  v2 <- unique(dat[[var2]])
  dtm <- NULL
  dtna <- NULL
  mx <- 0

  # Get number of lines needed to show labels
  minlns <- get_line_count(v2) + 1

  # Create matrix
  # v1 in rows and v2 in columns
  for (vl in v2) {

    dt <- dat[dat[[var2]] == vl, ]

    # Prepare data
    if (plt$scale == "percent") {

      cnt <- as.numeric(dt$PCT)
      slbl <- "Percent"

    } else if (plt$scale == "log") {

      cnt <- as.numeric(log10(dt$CNT))
      slbl <- "Log Frequency"

    } else if (plt$scale == "sqrt") {

      cnt <- as.numeric(sqrt(dt$CNT))
      slbl <- "Sqrt Frequency"

    } else {  # Default to Frequency

      cnt <- as.numeric(dt$CNT)
      slbl <- "Frequency"

    }

    nas <- rep(NA, length(cnt))
    mx <- max(c(mx, cnt))

    if (is.null(dtm)) {
      dtm <- data.frame(cnt)
      dtna <- data.frame(nas)
    } else {
      dtm <- cbind(dtm, cnt)
      dtna <- cbind(dtna, nas)
    }
  }

  # Assign names
  names(dtm) <- NULL
  rownames(dtm) <- v1

  # NA data names
  names(dtna) <- NULL
  rownames(dtna) <- v1

  # Get scale
  if (plt$type == "barchart" & plt$twoway == "stacked") {
    mx <- sum(dtm[[1]])
    for (idx in seq_along(v2)) {
      tmx <- sum(dtm[[idx]])
      if (tmx > mx) {
        mx <- tmx
      }
    }
    scl <- c(0, mx * 1.05)
  } else {
    scl <- c(0, mx * 1.05)
  }

  # Convert to matrix
  dtm <- as.matrix(dtm)
  dtna <- as.matrix(dtna)

  # Assign labels
  if (plt$orient == "vertical") {
    blbl <- tbl2
    llbl <- slbl
    glbl <- tbl1
  } else {
    blbl <- slbl
    llbl <- tbl2
    glbl <- tbl1
  }

  # Set bar background color
  bgcolor <- "#E5EAF2"
  if (plt$type == "barchart") {
    bgpalette <- c("#B7BFD9", "#E8ADAD", "#B3D2D0", "#D4C3AD", "#DBC4E6")
  } else {
    bgpalette <- c("#445694", "#A23A2E", "#01665E", "#543005", "#9D3CDB")
  }
  bgpalette <- rep(bgpalette, 20)
  bgcolor <- bgpalette[seq(1, length(v1))]

  if (plt$type == "dotplot" & plt$twoway == "cluster") {
    stop("Two-way cluster option is not available for dot type frequency plots.")

  } else if (plt$type == "dotplot") {

    # Low to high
    xlm <- c(.5, length(v2) + .5)
    xscl <- seq(1, length(v2))

    if (horz == TRUE) {  # Horizontal

      # Calculate left margin lines
      if (minlns > 3) {
        lml <- minlns + 3
      } else {
        lml <- 7
      }

      # Set custom margins
      par(mar = c(bml, lml, tml, .75) + 0.1)

      # Create empty plot
      b1 <- plot(
        dtna,  # Empty data
        main = ttl,  # Title
        xlab = blbl,  # Label x axis
        ylab = "",
        ylim = xlm,
        xlim = scl,  # x axis scale
        # horiz = TRUE,
        axes = FALSE  # Don't create axis yet
      )

      # Left label
      mtext(llbl, side = 2, line = par()$mar[2] - 2)

      # Get tick marks for ablines
      a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
      a2 <- axis(2, las = 1, col.ticks = "grey55", labels = v2, at = xscl)  # Create axis

      ## Add gridlines based on axis created above
      abline(h = a2, col = "grey90", lwd = 1, lty = 3)

      # Add points
      for (i in seq_along(v1)) {
        points(dtm[ i, ],
               seq_along(v2),
               pch = 1,
               col = bgcolor[i],
               cex = 1.3)
      }

    } else {  # Vertical

      # Set custom margins
      par(mar = c(bml, 5, tml,  .75) + 0.1)

      # Create empty plot.
      # xlim created automatically
      # and passed to next barplot
      b1 <- plot(
        dtna,  # Empty data
        main = ttl,  # Title
        xlab = blbl,  # Label x axis
        ylab = llbl,  # Label y axis
        ylim = scl,  # y axis scale
        xlim = xlm,
        axes = FALSE  # Don't create axis yet
      )

      # Get tick marks for ablines
      a1 <- axis(1, las = 1, col.ticks = "grey55", labels = v2, at = xscl)  # Create axis
      a2 <- axis(2, las = 1, col.ticks = "grey55")  # Create axis

      ## Add gridlines based on axis created above
      abline(v = a1, col = "grey90", lwd = 1, lty = 3)


      # Add points
      for (i in seq_along(v1)) {
        points(seq_along(v2),
               dtm[i, ],
               pch = 1,
               col = bgcolor[i],
               cex = 1.3)
      }
    }



  } else {  # Bar chart

    # Low to high
    xlm <- c(.5, length(v2) + .5)
    xscl <- seq(1, length(v2))

    if (horz == TRUE) {  # Horizontal

      # Calculate left margin lines
      if (minlns > 3) {
        lml <- minlns + 3
      } else {
        lml <- 7
      }

      # Set custom margins
      par(mar = c(bml, lml, tml, .75) + 0.1)

      # Create empty plot
      b1 <- barplot(
        dtna,  # Empty data
        beside = clust,
        main = ttl,  # Title
        xlab = blbl,  # Label x axis
        # ylim = xlm,   # Y axis scale
        xlim = scl,  # x axis scale
        horiz = TRUE,
        axes = FALSE  # Don't create axis yet
      )


      mtext(llbl, side = 2, line = par()$mar[2] - 2)

      # Get tick marks for ablines
      a1 <- axis(1, las = 1, col.ticks = "grey55")  # Create axis
      # a2 <- axis(2, las = 1, col.ticks = "grey55", labels = v2, at = xscl)

      ## Add gridlines based on axis created above
      abline(v = a1, col = "grey90", lwd = 1)

    } else {  # Vertical

      # Set custom margins
      par(mar = c(bml, 5, tml, .75) + 0.1)

      # Create empty plot.
      # xlim created automatically
      # and passed to next barplot
      b1 <- barplot(
        dtna,  # Empty data
        beside = clust,
        main = ttl,  # Title
        xlab = blbl,  # Label x axis
        ylab = llbl,  # Label y axis
        ylim = scl,  # y axis scale
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
      dtm,   # Data
      beside = clust,
      horiz = horz,
      names.arg = v2,
      col  = bgcolor,
      border = "grey55",
      las = 1,
      tick = TRUE,
      add = TRUE,   # Add to existing plot
      axes = FALSE  # Already created above
    )

  }

  # Calculate inset y value
  line_height_fraction <- (par("mai")[1] / par("pin")[2])
  ins <- - (line_height_fraction * 0.87)

  # Symbol type
  if (plt$type == "barchart") {
    syt <- 15
    px <- 1.6
  } else {
    syt <- 1
    px <- 1.3
  }

  # Draw legend
  legend("bottom",  # Starting location is bottom of plot area
         legend = c(glbl, paste0(v1, " ")), # Variable name and value labels
         col    = c("black", bgcolor),  # Colors of symbols
         pch    = c(NA, rep(syt, length(v1))),  # Don't show any symbol for variable name
         x.intersp = c(0, rep(.9, length(v1))),  # Spacing between group label and box
         y.intersp = 0,  # Spacing between content and borders
         horiz  = TRUE,  # Legend orientation
         bty    = "o",  # Single line border around legend
         inset  = c(0, ins),  # Offset from bottom of plot area
         box.col = "grey", # Grey border to match SAS
         xpd    = TRUE,   # Allow legend to leave plot area and appear in margin
         pt.cex = px,     # Make symbol bigger
         text.width = NA  # Compute label widths dynamically
  )


  ## Frame
  box(col = "grey70", lwd = 1)
  box("figure", col = "grey70", lwd = 1)

  # Restore margins
  par(mar = opar$mar)

  # Close device context
  dev.off()

  # Put plot in reporter plot object
  ret <- create_plot(pth, height = hti, width = wdi)

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

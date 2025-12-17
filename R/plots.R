

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


#' @export
freqplot <- function(groupby = NULL, npanelpos = 4, orient = "vertical",
                     scale = "freq", twoway = "groupvertical", type = "barchart") {


  # Create object
  ret <- structure(list(), class = c("freqplot", "list"))

  ret$groupby <- groupby
  ret$npanelpos <- npanelpos
  ret$orient <- orient
  ret$scale <- scale
  ret$twoway <- twoway
  ret$type <- type

  return(ret)
}



# Plot Rendering ----------------------------------------------------------


#' @noRd
render_plot <- function (dat, table) {
  UseMethod("render_plot", dat, table)
}


#' @importFrom stats aggregate
#' @import graphics
#' @noRd
render_plot.freqplot <- function(dat, table) {


  # Get variables
  xvar <- cmd$var
  yvar <- cmd$response

  # Get stat
  st <- cmd$stat

  # Get titles
  ttls <- plt$titles

  # Create counter variable
  dat$cnt. <- 1

  # Get grouping variables
  grps <- cmd$group

  # Get group values
  grpvals <- NULL
  if (!is.null(grps)) {

    grpvals <- unique(dat[[grps]])
  }

  # Colors - for now
  if (length(grps) == 0) {
    cols <- "grey90"
  } else {
    cols <- c("#7B8CC3",   # blue-ish
              "#DC6E6C",   # red-ish
              "#5FB7A8",   # teal
              "#B08D52"    # brown/gold
    )

    cols <- cols[seq(1, length(grpvals))]
  }

  # Clustered display
  bsd <- FALSE
  if (!is.null(cmd$groupdisplay)) {
    if (tolower(cmd$groupdisplay) == "cluster") {
      bsd <- TRUE
    }
  }

  # No response, use the count variable, no statistic
  if (is.null(yvar)) {
    yvar <- "cnt."
  }

  if (is.null(st)) {
    st <- "sum"
  }

  byvars <- list(dat[[xvar]])
  grpvars <- NULL
  if (!is.null(grps)) {
    for (grp in grps) {
      byvars[[length(byvars) + 1]] <- dat[[grp]]
      grpvars[[length(grpvars) + 1]] <- paste0("GRP", length(grpvars) + 1)
    }
    mrg <- par()$mar
    mrg[1] <- mrg[1] + 1
    par(mar = mrg)
  }

  # Sum by variables
  sdat <- aggregate(dat[[yvar]], by = byvars, FUN = st)
  names(sdat) <- c("CAT", grpvars, "FREQ")

  # print("sdat")
  # print(sdat)

  # Get x names
  xnms <- unique(sdat$CAT)

  if (is.null(grps)) {
    ynms <- ""
    mdat <- sdat$FREQ
  } else {

    # Get y names
    ynms <- unique(sdat$GRP1)

    # Create matrix
    mdat <- sdat$FREQ
    dim(mdat) <- c(length(xnms), length(ynms))
    dimnames(mdat) <- list(xnms, ynms)

    # transpose
    mdat <- t(mdat)
  }

  # Create scale
  # yl <- c(0, max(sdat$FREQ))

  # print("mdat")
  # print(mdat)
  # print("xnms")
  # print(xnms)
  # print("xvar")
  # print(xvar)

  lgnd <- FALSE
  if (!is.null(grps)) {
    lgnd <- TRUE
  }

  # Bar chart
  barplot(mdat,
          names.arg = xnms,
          main = ttls,
          beside = bsd,
          xlab = xvar,
          ylab = "Frequency",
          #  ylim = yl,
          col = cols,
          border = "grey20",
          legend.text = lgnd
  )

  # This will take some work
  # Comment out for now
  # Problem is getting it placed in the bottom margin
  # R doesn't do it that way by default
  # Will have to custom program it
  # if (!is.null(grps)) {
  #
  #   legend(
  #   )




}



#' @importFrom stats aggregate
#' @import graphics
#' @noRd
render_bar <- function(dat, cmd, interactive = FALSE) {


  # Get variables
  xvar <- cmd$var
  yvar <- cmd$response

  # Get stat
  st <- cmd$stat

  # Get titles
  ttls <- plt$titles

  # Create counter variable
  dat$cnt. <- 1

  # Get grouping variables
  grps <- cmd$group

  # Get group values
  grpvals <- NULL
  if (!is.null(grps)) {

    grpvals <- unique(dat[[grps]])
  }

  # Colors - for now
  if (length(grps) == 0) {
    cols <- "grey90"
  } else {
    cols <- c("#7B8CC3",   # blue-ish
              "#DC6E6C",   # red-ish
              "#5FB7A8",   # teal
              "#B08D52"    # brown/gold
    )

    cols <- cols[seq(1, length(grpvals))]
  }

  # Clustered display
  bsd <- FALSE
  if (!is.null(cmd$groupdisplay)) {
    if (tolower(cmd$groupdisplay) == "cluster") {
      bsd <- TRUE
    }
  }

  # No response, use the count variable, no statistic
  if (is.null(yvar)) {
    yvar <- "cnt."
  }

  if (is.null(st)) {
    st <- "sum"
  }

  byvars <- list(dat[[xvar]])
  grpvars <- NULL
  if (!is.null(grps)) {
    for (grp in grps) {
      byvars[[length(byvars) + 1]] <- dat[[grp]]
      grpvars[[length(grpvars) + 1]] <- paste0("GRP", length(grpvars) + 1)
    }
    mrg <- par()$mar
    mrg[1] <- mrg[1] + 1
    par(mar = mrg)
  }

  # Sum by variables
  sdat <- aggregate(dat[[yvar]], by = byvars, FUN = st)
  names(sdat) <- c("CAT", grpvars, "FREQ")

  # print("sdat")
  # print(sdat)

  # Get x names
  xnms <- unique(sdat$CAT)

  if (is.null(grps)) {
    ynms <- ""
    mdat <- sdat$FREQ
  } else {

    # Get y names
    ynms <- unique(sdat$GRP1)

    # Create matrix
    mdat <- sdat$FREQ
    dim(mdat) <- c(length(xnms), length(ynms))
    dimnames(mdat) <- list(xnms, ynms)

    # transpose
    mdat <- t(mdat)
  }

  # Create scale
  # yl <- c(0, max(sdat$FREQ))

  # print("mdat")
  # print(mdat)
  # print("xnms")
  # print(xnms)
  # print("xvar")
  # print(xvar)

  lgnd <- FALSE
  if (!is.null(grps)) {
    lgnd <- TRUE
  }

  # If no output file and not interactive, don't run.
  # Will be necessary to pass CRAN checks.
  if ((interactive & interactive()) |
      !interactive) {

    # Bar chart
    barplot(mdat,
            names.arg = xnms,
            main = ttls,
            beside = bsd,
            xlab = xvar,
            ylab = "Frequency",
            #  ylim = yl,
            col = cols,
            border = "grey20",
            legend.text = lgnd
    )

    # This will take some work
    # Comment out for now
    # Problem is getting it placed in the bottom margin
    # R doesn't do it that way by default
    # Will have to custom program it
    # if (!is.null(grps)) {
    #
    #   legend(
    #   )
  }

  # }

}


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

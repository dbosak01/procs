


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

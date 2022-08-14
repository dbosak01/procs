
# Custom Statistics Functions ---------------------------------------------

#' @import stats
get_aov <- function(data, var1, byvars, wgt = NULL,
                    bylbl = NULL, output = FALSE) {


  bv <- paste(byvars, sep = "", collapse = "+")

  if (is.null(wgt)) {
    res <- aov(as.formula(paste(var1, "~", bv)),
               data = data, weights = wgt)
    ret <- summary(res)[[1]]

  } else {

    res <- aov(as.formula(paste(var1, "~", bv)), data = data)

    ret <- summary(res)[[1]]

  }


  nms <- c("AOV.DF", "AOV.SUMSQ", "AOV.MEANSQ", "AOV.F", "AOV.P")
  if (ncol(ret) != length(nms))
    nms <- nms[seq(1, ncol(ret))]

  if (output) {

    ret <- as.data.frame(ret[1, ])
    names(ret) <- nms
    rownames(ret) <- NULL


  } else {

    lbls <- names(ret)
    ret <- data.frame(NAME = rownames(ret), ret)


    names(ret) <- c("NAME", nms)
    rownames(ret) <- NULL
    lblsl <- as.list(c("Name", lbls))
    names(lblsl) <- c("NAME", nms)
    labels(ret) <- lblsl



    spns <- list()
    if (!is.null(bylbl)) {
      spns[[1]] <- span(1, ncol(ret),
                        bylbl,
                        level = 1)
    }

    spns[[length(spns) + 1]] <- span(1, ncol(ret),
                      paste0("Analysis Of Variance - ", var1),
                      level = length(spns) + 1)

    attr(ret, "spans") <- spns

    formats(ret) <- list(AOV.SUMSQ = "%.4f",
                         AOV.MEANSQ = "%.4f",
                         AOV.F = "%.4f",
                         AOV.P = "%.4f")

  }

  return(ret)
}


#' Include missing values
#' @noRd
get_stderr <- function(x, narm = TRUE) {

  ret <- sd(x, na.rm = narm) / sqrt(sum(!is.na(x)))

  return(ret)

}


#' @noRd
get_clm <- function(x, narm = TRUE) {

  #Sample size
  n <- sum(!is.na(x), na.rm = narm)

  # Sample mean weight
  xbar <- mean(x, na.rm = narm)

  # Sample standard deviation
  std <- sd(x, na.rm = narm)

  # Margin of error
  margin <- qt(0.975,df=n-1)*std/sqrt(n)

  # Lower and upper confidence interval boundaries
  res <- c(ucl = xbar + margin,
           lcl = xbar - margin)

  return(res)
}

#' @noRd
get_mode <- function(x) {

  uniqv <- unique(x)
  res <- uniqv[which.max(tabulate(match(x, uniqv)))]

  return(res)
}


get_fisher <- function(x, y, wgt = NULL, bylbl = "", output = FALSE) {


  if (!is.null(wgt)) {

    tb <- xtabs(wgt~x + y)



  } else {

    cnt <- rep(1, length(x))

    tb <- xtabs(cnt~x + y)

  }

  tres <-  fisher.test(tb, alternative = "two.sided")
  gres <-  fisher.test(tb, alternative = "greater")
  lres <-  fisher.test(tb, alternative = "less")


  if (output) {


    ret <- data.frame(FISHER.1.1 = tb[2, 2],
                      FISHER.LS = lres[["p.value"]],
                      FISHER.RS =  gres[["p.value"]],
                      FISHER.2S = tres[["p.value"]],
                      stringsAsFactors = FALSE)

    rownames(ret) <- NULL

  } else {

    mes <- c("Cell1.1", "Left.Sided", "Right.Sided", "Two.Sided")
    val <- c(tb[2, 2],
             lres[["p.value"]],
             gres[["p.value"]],
             tres[["p.value"]])

    names(val) <- NULL

    ret <- data.frame(Measure = mes, Value = val, stringsAsFactors = FALSE)

    attr(ret$Value, "format") <- "%.4f"

    spn <- list(span(1, 2, paste0(bylbl, "Fisher's Exact Test"), level = 1))
    attr(ret, "spans") <- spn

  }

  return(ret)

}


get_chisq <- function(x, y, wgt = NULL, corrct = FALSE, bylbl = "", output = FALSE) {


  if (!is.null(wgt)) {

    tb <- xtabs(wgt~x + y)



  } else {

    cnt <- rep(1, length(x))

    tb <- xtabs(cnt~x + y)

  }

  res <- suppressWarnings(chisq.test(tb, correct = corrct))

  if (output) {

    ret <- data.frame(CHISQ = res[["statistic"]],
                      CHISQ.DF = res[["parameter"]],
                      CHISQ.P = res[["p.value"]],
                      stringsAsFactors = FALSE)

    rownames(ret) <- NULL

  } else {

    mes <- c("Chi-Square", "DF", "PR>ChiSq")
    val <- c(res[["statistic"]], res[["parameter"]], res[["p.value"]])

    names(val) <- NULL

    ret <- data.frame(Measure = mes, Value = val, stringsAsFactors = FALSE)

    attr(ret$Value, "format") <- "%.4f"


    spn <- list(span(1, 2, paste0(bylbl, "Chi-Square Test"), level = 1))
    attr(ret, "spans") <- spn
  }


  return(ret)


}

# get_cmh <- function(x, y, wgt = NULL, corrct = FALSE, bylbl = "", output = FALSE) {
#
#
#   if (!is.null(wgt)) {
#
#     tb <- xtabs(wgt~x + y)
#
#
#
#   } else {
#
#     cnt <- rep(1, length(x))
#
#     tb <- xtabs(cnt~x + y)
#
#   }
#
#   res <- suppressWarnings(mantelhaen.test(tb, correct = corrct))
#
#   if (output) {
#
#     ret <- data.frame(CHISQ = res[["statistic"]],
#                       CHISQ.DF = res[["parameter"]],
#                       CHISQ.P = res[["p.value"]],
#                       stringsAsFactors = FALSE)
#
#     rownames(ret) <- NULL
#
#   } else {
#
#     mes <- c("Chi-Square", "DF", "PR>ChiSq")
#     val <- c(res[["statistic"]], res[["parameter"]], res[["p.value"]])
#
#     names(val) <- NULL
#
#     ret <- data.frame(Measure = mes, Value = val, stringsAsFactors = FALSE)
#
#     attr(ret$Value, "format") <- "%.4f"
#
#
#     spn <- list(span(1, 2, paste0(bylbl, "Chi-Square Test"), level = 1))
#     attr(ret, "spans") <- spn
#   }
#
#
#   return(ret)
#
#
# }



# Utilities ---------------------------------------------------------------



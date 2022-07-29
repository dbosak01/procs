
# Custom Statistics Functions ---------------------------------------------



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


get_fisher <- function(x, y, wgt = NULL, bylbl = "") {


  if (!is.null(wgt)) {

    tb <- xtabs(wgt~x + y)



  } else {

    cnt <- rep(1, length(x))

    tb <- xtabs(cnt~x + y)

  }

  tres <-  fisher.test(tb, alternative = "two.sided")
  gres <-  fisher.test(tb, alternative = "greater")
  lres <-  fisher.test(tb, alternative = "less")


  mes <- c("Cell1.1", "Left.Sided", "Right.Sided", "Two.Sided")
  val <- c(tb[2, 2],
           lres[["p.value"]],
           gres[["p.value"]],
           tres[["p.value"]])

  names(val) <- NULL

  ret <- data.frame(Measure = mes, Value = val)

  attr(ret$Value, "format") <- "%.4f"

  spn <- list(span(1, 2, paste0(bylbl, "Fisher's Exact Test"), level = 1))
  attr(ret, "spans") <- spn

  return(ret)

}


get_chisq <- function(x, y, wgt = NULL, corrct = FALSE, bylbl = "") {


  if (!is.null(wgt)) {

    tb <- xtabs(wgt~x + y)



  } else {

    cnt <- rep(1, length(x))

    tb <- xtabs(cnt~x + y)

  }

  res <- suppressWarnings(chisq.test(tb, correct = corrct))


  mes <- c("Chi-Square", "DF", "PR>ChiSq")
  val <- c(res[["statistic"]], res[["parameter"]], res[["p.value"]])

  names(val) <- NULL

  ret <- data.frame(Measure = mes, Value = val)

  attr(ret$Value, "format") <- "%.4f"


  spn <- list(span(1, 2, paste0(bylbl, "Chi-Square Test"), level = 1))
  attr(ret, "spans") <- spn


  return(ret)


}




# Utilities ---------------------------------------------------------------



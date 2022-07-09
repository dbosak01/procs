
# Custom Statistics Functions ---------------------------------------------



#' Include missing values
#' @noRd
stderror <- function(x, narm = TRUE) {

  ret <- sd(x, na.rm = narm) / sqrt(sum(!is.na(x)))

  return(ret)

}


#' @noRd
clm <- function(x, narm = TRUE) {

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
getmode <- function(x) {

  uniqv <- unique(x)
  res <- uniqv[which.max(tabulate(match(x, uniqv)))]

  return(res)
}


getfisher <- function(x, y, wgt = NULL) {


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

  ret <- data.frame(Measure = mes, Value = val)


}


getchisq <- function(x, y, wgt = NULL, corrct = FALSE) {


  if (!is.null(wgt)) {

    tb <- xtabs(wgt~x + y)



  } else {

    cnt <- rep(1, length(x))

    tb <- xtabs(cnt~x + y)

  }

  res <- suppressWarnings(chisq.test(tb, correct = corrct))


  mes <- c("Chi-Square", "DF", "PR>ChiSq")
  val <- c(res[["statistic"]], res[["parameter"]], res[["p.value"]])

  ret <- data.frame(Measure = mes, Value = val)


  return(ret)


}




# Utilities ---------------------------------------------------------------



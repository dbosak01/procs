
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
getmode <- function(v) {

  uniqv <- unique(v)
  res <- uniqv[which.max(tabulate(match(v, uniqv)))]

  return(res)
}










# Utilities ---------------------------------------------------------------



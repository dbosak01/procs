
# Custom Statistics Functions ---------------------------------------------



#' Include missing values
#' @noRd
stderror <- function(x) {

  ret <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

  return(ret)

}


#' @noRd
clm <- function(x) {

  #Sample size
  n <- sum(!is.na(x))
  # Sample mean weight
  xbar <- mean(x, rm.na = TRUE)
  # Sample standard deviation
  s <- sd(x, na.rm = TRUE)

  # Margin of error
  margin <- qt(0.975,df=n-1)*s/sqrt(n)


  # Lower and upper confidence interval boundaries
  res <- c(ucl = xbar + margin,
           lcl = xbar - margin)


  # lowerinterval <- xbar - margin
  # lowerinterval
  # #[1] 195.5191
  # upperinterval <-
  # upperinterval

  return(res)
}

#' @noRd
getmode <- function(v) {

  uniqv <- unique(v)
  res <- uniqv[which.max(tabulate(match(v, uniqv)))]

  return(res)
}










# Utilities ---------------------------------------------------------------



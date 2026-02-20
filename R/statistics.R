
# Custom Statistics Functions ---------------------------------------------

pfmt <- value(condition(x < .0001, "<.0001"),
              condition(TRUE, "%.4f"), log = FALSE)

# @import stats
# get_aov <- function(data, var1, byvars, wgt = NULL,
#                     bylbl = NULL, output = FALSE, resid = TRUE) {
#
#
#   bv <- paste(byvars, sep = "", collapse = "+")
#
#   if (is.null(wgt)) {
#     res <- aov(as.formula(paste(var1, "~", bv)),
#                data = data, weights = wgt)
#     ret <- summary(res)[[1]]
#
#   } else {
#
#     res <- aov(as.formula(paste(var1, "~", bv)), data = data)
#
#     ret <- summary(res)[[1]]
#
#   }
#
#   nms <- c("AOV.DF", "AOV.SUMSQ", "AOV.MEANSQ", "AOV.F", "AOV.P")
#
#
#   lbls <- names(ret)
#   if (output) {
#
#     if (ncol(ret) == 3) {
#       ret <- data.frame("VAR" = var1, "CLASS" = trimws(rownames(ret)), ret,
#                         AOV.F = NA, AOV.P = NA,
#                         stringsAsFactors = FALSE)
#     } else {
#       ret <- data.frame("VAR" = var1, "CLASS" = trimws(rownames(ret)), ret,
#                         stringsAsFactors = FALSE)
#     }
#     names(ret) <- c("VAR", "CLASS", nms)
#
#     if (resid == FALSE) {
#       ret <- subset(ret, ret[["CLASS"]] != "Residuals")
#     }
#
#     rownames(ret) <- NULL
#     lblsl <- as.list(c("Variable", "Class", lbls))
#     names(lblsl) <- c("VAR", "CLASS", nms)
#     labels(ret) <- lblsl
#
#   } else {
#
#     if (ncol(ret) == length(nms))
#       nms <- nms[seq(1, ncol(ret))]
#
#     ret <- data.frame("CLASS" = trimws(rownames(ret)), ret, stringsAsFactors = FALSE)
#
#     names(ret) <- c("CLASS", nms)
#
#     if (resid == FALSE) {
#       ret <- subset(ret, ret[["CLASS"]] != "Residuals")
#     }
#
#     rownames(ret) <- NULL
#     lblsl <- as.list(c("Class", lbls))
#     names(lblsl) <- c("CLASS", nms)
#     labels(ret) <- lblsl
#
#   }
#
#   spns <- list()
#   if (!is.null(bylbl)) {
#     spns[[1]] <- span(1, ncol(ret),
#                       bylbl,
#                       level = 1)
#   }
#
#   spns[[length(spns) + 1]] <- span(1, ncol(ret),
#                     paste0("Analysis Of Variance - ", var1),
#                     level = length(spns) + 1)
#
#   attr(ret, "spans") <- spns
#
#   formats(ret) <- list(AOV.SUMSQ = "%.4f",
#                        AOV.MEANSQ = "%.4f",
#                        AOV.F = pfmt,
#                        AOV.P = pfmt)
#
#
#
#   return(ret)
# }


#' Include missing values
#' @noRd
get_variance <- function(x, df, wgt = NULL, narm = TRUE) {

  if (narm) {
      x <- x[!is.na(x)]
  }

  if (sum(!is.na(x)) == 0 || df <= 0) {
    ret <- NA
  } else if (is.null(wgt)) {
    ret <- sum((x - mean(x, na.rm = narm))^2, na.rm = narm) / df
  } else {
    ret <- sum(wgt*(x - sum(x*wgt, na.rm = narm)/sum(wgt))^2, na.rm = narm) / df
  }
  return(ret)
}

#' @noRd
get_stderr <- function(x, df, wgt = NULL, narm = TRUE) {
  if (narm) {
    x <- x[!is.na(x)]
  }
  if (sum(!is.na(x)) == 0 || df <= 0) {
    ret <- NA
  } else if (is.null(wgt)) {
    ret <- sqrt(get_variance(x, df, wgt, narm)) / sqrt(sum(!is.na(x)))
  } else {
    ret <- sqrt(get_variance(x, df, wgt, narm)) / sqrt(sum(wgt, na.rm = narm))
  }

  return(ret)

}

#' @noRd
get_t <- function(x, df, wgt=NULL, y = NULL, narm = TRUE, alpha = 0.05, paired = FALSE) {
  if (narm) {
    x <- x[!is.na(x)]
  }
  if (!is.numeric(alpha))
    alpha <- as.numeric(alpha)

  onesided = FALSE
  if (onesided) {
    alp <- 1 - alpha
  } else {
    alp <- 1 - (alpha / 2)
  }

  ret <- NULL

  if (is.null(y) & paired == FALSE) {

    stderr_x = get_stderr(x, df, wgt, narm)
    if (df<=0){
      t_value <- NA
      p_value <- NA
    } else {
      if (is.null(wgt))
        t_value <- mean(x, na.rm = narm) / stderr_x
      else
        t_value <- sum(x*wgt, na.rm = narm)/sum(wgt)/ stderr_x
      p_value <- 2 * pt(-abs(t_value), df = df)
    }

    ret <- c("T" = t_value,
             PRT = p_value,
             DF = df)


  }

  return(ret)
}

#' @noRd
get_weighted_quantile<- function(x, probs, wgt=NULL, narm = TRUE) {
  if (narm) {
    x <- x[!is.na(x)]
  }
  if (is.null(wgt)){
    ret <- quantile(x, probs = probs, type = 2, na.rm = narm)
  } else{
    ord <- order(x)
    x_sorted <- x[ord]
    w_sorted <- wgt[ord]
    cw <- cumsum(w_sorted) / sum(w_sorted)
    ret <- sapply(probs, function(pp) x_sorted[which(cw >= pp)[1]])
  }

  return(ret)
}


#' @noRd
get_clm <- function(x, df, wgt = NULL, narm = TRUE, alpha = 0.05, onesided = FALSE) {
  if (narm) {
    x <- x[!is.na(x)]
  }
  if (!is.numeric(alpha))
    alpha <- as.numeric(alpha)



  if (onesided) {
    alp <- 1 - alpha
  } else {
    alp <- 1 - (alpha / 2)
  }

  if (df<=0){
    res <- c(ucl = NA, lcl = NA, alpha = alpha)
  } else {
    #Sample size
    n <- sum(!is.na(x))

    # Sample weighted mean
    if (is.null(wgt))
      xbar <- mean(x, na.rm = narm)
    else
      xbar <- sum(x*wgt, na.rm = narm)/sum(wgt)

    # Margin of error

    margin <- qt(alp,df=df)*get_stderr(x, df, wgt, narm)

    # Lower and upper confidence interval boundaries
    res <- c(ucl = xbar + margin,
             lcl = xbar - margin,
             alpha = alpha)
  }
  return(res)
}


#' @noRd
get_clmstd <- function(x, df, wgt=NULL, narm = TRUE, alpha = 0.05, onesided = FALSE) {

  if (!is.numeric(alpha))
    alpha <- as.numeric(alpha)

  if (onesided) {
    alp <- alpha
  } else {
    alp <- alpha / 2
  }

  # Remove NA
  if (narm)
    x <- na.omit(x)

  # Calculate variance
  x.var <- get_variance(x, df, wgt, narm)


  # Calculate chisq for upper and lower cl
  crit.low <- qchisq(alp, df = df, lower.tail = FALSE)
  crit.upp <- qchisq(alp, df = df, lower.tail = TRUE)

  # Take square root
  ci <- sqrt(c(low = df*x.var / crit.low, upp = df*x.var / crit.upp))

  # Lower and upper confidence interval boundaries
  res <- c(ucl = ci[["upp"]],
           lcl = ci[["low"]],
           alpha = alpha)

  return(res)
}


#' @noRd
get_mode <- function(x, narm = TRUE) {
  if (narm)
    x <- x[!is.na(x)]
  uniqv <- unique(x)
  counts <- tabulate(match(x, uniqv))

  if (sum(counts == max(counts)) > 1) {
    # If there are multiple modes, return NA
    res <- NA
  } else {
    res <- uniqv[which.max(counts)]
  }

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


    ret <- data.frame(FISHER.1.1 = tb[1, 1],
                      FISHER.LS = lres[["p.value"]],
                      FISHER.RS =  gres[["p.value"]],
                      FISHER.2S = tres[["p.value"]],
                      stringsAsFactors = FALSE)

    # Add by variables if exist
    if (length(bylbl) > 0) {
      bv <- bylbl[[1]]
      if (!is.null(bv)) {
        lst <- list()
        nms <- names(bv)
        if (length(nms) > 0) {
          for (nm in nms) {
            lst[[nm]] <- bv[[nm]]
          }
          bvds <- as.data.frame(lst, stringsAsFactors = FALSE)
          names(bvds) <- nms
          ret <- cbind(bvds, ret)
        }

      }
    }


    rownames(ret) <- NULL

  } else {

    mes <- c("Cell1.1", "Left.Sided", "Right.Sided", "Two.Sided")
    val <- c(tb[1, 1],
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


get_chisq <- function(x, y, wgt = NULL, bylbl = "", output = FALSE) {

  if (!"character" %in% class(x))
    x <- as.character(x)

  if (!"character" %in% class(y))
    y <- as.character(y)

  if (!is.null(wgt)) {

    z <- wgt != 0

    wgt <- wgt[z]
    x <- x[z]
    y <- y[z]

    tb <- xtabs(wgt~x + y)

  } else {

    cnt <- rep(1, length(x))

    tb <- xtabs(cnt~x + y)

  }

  ret <- NULL

  # res <- suppressWarnings(chisq.test(tb, correct = FALSE))
  # res2 <- suppressWarnings(chisq.test(tb, correct = TRUE))

  res <- chisq.test(tb, correct = FALSE)
  res2 <- chisq.test(tb, correct = TRUE)

  if (output) {

    lbls <- c("Chi-Square", "Continuity Adj. Chi-Square")
    df <- c(res[["parameter"]], res2[["parameter"]])
    val <- c(res[["statistic"]], res2[["statistic"]])
    pval <- c(res[["p.value"]], res2[["p.value"]])

    ret <- data.frame(STAT = lbls,
                      DF = df,
                      VAL = val,
                      PROB = pval, stringsAsFactors = FALSE)

    # ret <- data.frame(CHISQ = res[["statistic"]],
    #                   CHISQ.DF = res[["parameter"]],
    #                   CHISQ.P = res[["p.value"]],
    #                   stringsAsFactors = FALSE)


    # Add by variables if exist
    if (length(bylbl) > 0) {
      bv <- bylbl[[1]]
      if (!is.null(bv)) {
        lst <- list()
        nms <- names(bv)
        if (length(nms) > 0) {
          for (nm in nms) {
            lst[[nm]] <- bv[[nm]]
          }
          bvds <- as.data.frame(lst, stringsAsFactors = FALSE)
          names(bvds) <- nms
          ret <- cbind(bvds, ret)
        }

      }
    }

  } else {

    lbls <- c("Chi-Square", "Continuity Adj. Chi-Square")
    df <- c(res[["parameter"]], res2[["parameter"]])
    val <- c(res[["statistic"]], res2[["statistic"]])
    pval <- c(res[["p.value"]], res2[["p.value"]])

    names(df) <- NULL
    names(val) <- NULL
    names(pval) <- NULL

    ret <- data.frame(STAT = lbls,
                      DF = df,
                      VAL = val,
                      PROB = pval, stringsAsFactors = FALSE)

    widths(ret) <- list(STAT = 18)
  }

  if (!is.null(ret)) {

    rownames(ret) <- NULL
    formats(ret) <- list(DF = "%d", VAL = "%.4f", PROB = pfmt)
    labels(ret) <- list(STAT = "Statistic", DF = "DF",
                        VAL = "Value", PROB = "Prob")
  }


  return(ret)


}


get_chisq_back <- function(x, y, wgt = NULL, corrct = FALSE, bylbl = "", output = FALSE) {


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


    # Add by variables if exist
    if (length(bylbl) > 0) {
      bv <- bylbl[[1]]
      if (!is.null(bv)) {
        lst <- list()
        nms <- names(bv)
        if (length(nms) > 0) {
          for (nm in nms) {
            lst[[nm]] <- bv[[nm]]
          }
          bvds <- as.data.frame(lst, stringsAsFactors = FALSE)
          names(bvds) <- nms
          ret <- cbind(bvds, ret)
        }

      }
    }

    rownames(ret) <- NULL

  } else {

    mes <- c("Chi-Square", "DF", "PR>ChiSq")
    val <- c(res[["statistic"]], res[["parameter"]], res[["p.value"]])

    names(val) <- NULL

    ret <- data.frame(Measure = mes, Value = val, stringsAsFactors = FALSE)

    fmt <- flist("%.4f", "%d", pfmt, type = "row")

    attr(ret$Value, "format") <- fmt


    spn <- list(span(1, 2, paste0(bylbl, "Chi-Square Test"), level = 1))
    attr(ret, "spans") <- spn
  }


  return(ret)

}

#' @noRd
get_skewness <- function(x, df, narm = TRUE) {

  ret <- NULL

  if(any(ina <- is.na(x))) {
    if(narm)
      x <- x[!ina]
  }

  n <- length(x)
  if(n < 3){
    ret <- NA
  }
  else{
  # ret <- Skewness(x)
    x <- x - mean(x)
    y <- sqrt(n) * sum(x ^ 3, na.rm = narm) / (sum(x ^ 2, na.rm = narm) ^ (3/2))
    if (df==n)
      ret <- y
    else
      ret <- y * sqrt(n * (n - 1)) / (n - 2)
  }
  return(ret)
}


get_skewness_back <- function(x, narm = TRUE) {

  ret <- NULL

  if(any(ina <- is.na(x))) {
    if(narm)
      x <- x[!ina]
  }

  n <- length(x)
  if(n < 3)
    stop("Skewness requires at least 3 complete observations.")

  x <- x - mean(x)
  y <- sqrt(n) * sum(x ^ 3, na.rm = narm) / (sum(x ^ 2, na.rm = narm) ^ (3/2))
  ret <- y * sqrt(n * (n - 1)) / (n - 2)

  return(ret)
}


#' @noRd
get_kurtosis <- function(x, df, narm = TRUE) {

  ret <- NULL

  if(any(ina <- is.na(x))) {
    if(narm)
      x <- x[!ina]
  }

  n <- length(x)

  if(n < 4){
    ret <- NA
  }
  else{
    x <- x - mean(x)
    r <- n * sum(x ^ 4, na.rm = narm) / (sum(x ^ 2, na.rm = narm) ^ 2)
    if (df==n)
      ret <- r-3
    else
     ret <- ((n + 1) * (r - 3) + 6) * (n - 1) / ((n - 2) * (n - 3))
  }
  return(ret)
}


get_kurtosis_back <- function(x, narm = TRUE) {

  ret <- NULL

  if(any(ina <- is.na(x))) {
    if(narm)
      x <- x[!ina]
  }

  n <- length(x)

  if(n < 4)
    stop("Kurtosis requires at least 4 complete observations.")

  x <- x - mean(x)
  r <- n * sum(x ^ 4, na.rm = narm) / (sum(x ^ 2, na.rm = narm) ^ 2)

  ret <- ((n + 1) * (r - 3) + 6) * (n - 1) / ((n - 2) * (n - 3))

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



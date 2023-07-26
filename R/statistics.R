
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
get_stderr <- function(x, narm = TRUE) {

  ret <- sd(x, na.rm = narm) / sqrt(sum(!is.na(x)))

  return(ret)

}

get_t <- function(x, y = NULL, narm = TRUE, alpha = 0.05, paired = FALSE) {

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


    res <- unclass(t.test(x, paired = FALSE, conf.level = alp))

    names(res[["statistic"]]) <- NULL
    names(res[["p.value"]]) <- NULL
    names(res[["parameter"]]) <- NULL

    ret <- c("T" = res[["statistic"]],
             PRT = res[["p.value"]],
             DF = res[["parameter"]])


  }

  return(ret)
}


#' @noRd
get_clm <- function(x, narm = TRUE, alpha = 0.05, onesided = FALSE) {

  if (!is.numeric(alpha))
    alpha <- as.numeric(alpha)

  if (onesided) {
    alp <- 1 - alpha
  } else {
    alp <- 1 - (alpha / 2)
  }

  #Sample size
  n <- sum(!is.na(x), na.rm = narm)

  # Sample mean weight
  xbar <- mean(x, na.rm = narm)

  # Sample standard deviation
  std <- sd(x, na.rm = narm)

  # Margin of error
  margin <- qt(alp,df=n-1)*std/sqrt(n)

  # Lower and upper confidence interval boundaries
  res <- c(ucl = xbar + margin,
           lcl = xbar - margin,
           alpha = alpha)

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

get_skewness <- function(x, narm = TRUE) {

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

# skewness <-
#   function(x, na.rm = FALSE, type = 3)
#   {
#     if(any(ina <- is.na(x))) {
#       if(na.rm)
#         x <- x[!ina]
#       else
#         return(NA)
#     }
#
#     if(!(type %in% (1 : 3)))
#       stop("Invalid 'type' argument.")
#
#     n <- length(x)
#     x <- x - mean(x)
#     y <- sqrt(n) * sum(x ^ 3) / (sum(x ^ 2) ^ (3/2))
#     if(type == 2) {
#       if(n < 3)
#         stop("Need at least 3 complete observations.")
#       y <- y * sqrt(n * (n - 1)) / (n - 2)
#     } else if(type == 3)
#       y <- y * ((1 - 1 / n)) ^ (3/2)
#
#     y
#   }


get_kurtosis <- function(x, narm = TRUE) {

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

# kurtosis <-
#   function(x, na.rm = FALSE, type = 3)
#   {
#     if(any(ina <- is.na(x))) {
#       if(na.rm)
#         x <- x[!ina]
#       else
#         return(NA)
#     }
#
#     if(!(type %in% (1 : 3)))
#       stop("Invalid 'type' argument.")
#
#     n <- length(x)
#     x <- x - mean(x)
#     r <- n * sum(x ^ 4) / (sum(x ^ 2) ^ 2)
#     y <- if(type == 1)
#       r - 3
#     else if(type == 2) {
#       if(n < 4)
#         stop("Need at least 4 complete observations.")
#       ((n + 1) * (r - 3) + 6) * (n - 1) / ((n - 2) * (n - 3))
#     }
#     else
#       r * (1 - 1 / n) ^ 2 - 3
#
#     y
#   }


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



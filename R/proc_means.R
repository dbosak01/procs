
# Means Procedure ---------------------------------------------------------



#' @title Calculates Summary Statistics
#' @encoding UTF-8
#' @description The \code{proc_means} function generates summary statistics
#' for selected variable on the input dataset.  The variables are identified
#' on the \code{var} parameter.  The statistics to perform are identified
#' on the \code{stats} parameter.  By default, results are returned as a list,
#' of tibbles.
#' @details
#' Some details about the sample function.
#'
#'
#' All statistics remove missing values by default.  Should there be an
#' option to include them?
#'
#' @section Statistics Options:
#' \itemize{
#'   \item{\strong{CSS}: Corrected Sum of Squares.
#'
#'   }
#'   \item{\strong{CV}: Coefficient of Variation.
#'
#'   }
#' }
#'
#' @section Using Weights:
#' Need a section explaining how to use weights.
#'
#' @section Data Constraints:
#' Explain limits of data for each stat option.  Number of non-missing
#' values, etc.
#'
#'
#' @param data The input data frame for which calculate summary statistics.
#' @param by An optional by group. You may
#' pass unquoted variable names to this parameter using the \code{\link{v}}
#' function.
#' @param class The variable or variables to perform frequency counts on.
#' @param var The variable(s) to calculate summary statistics for. You may
#' pass unquoted variable names to this parameter using the \code{\link{v}}
#' function.
#' @param stats A quoted vector of summary statistics keywords.  Valid
#' keywords are: "css", "clm", "cv", "n", "lclm", "mean", "median", "mode",
#' "min", "max",
#' "nmiss", "nobs", "range", "std", "stderr", "sum", "uclm", "var",
#' "q1", "q3", "p1", "p5", "p10", "p20", "p25", "p30", "p40",
#' "p50", "p60", "p70", "p75", "p80", "p90",
#' "p95", "p99", "qrange", "aov". You may
#' pass unquoted variable names to this parameter using the \code{\link{v}}
#' function.
#' @param weight An optional weight parameter.
#' @param view Whether to display the report in the interactive viewer.  Valid
#' values are TRUE and FALSE.  Default is TRUE.
#' @param titles A vector of one or more titles to use for the report output.
#' @param options An options object created with the \code{\link{opts}}
#' function. This parameter can accept a variety of options.
#' @param ... One or more output requests. To request a dataset output,
#' use the desired dataset name as the parameter name, and pass in the
#' \code{\link{out}} function with the desired specifics.  If only one
#' output dataset is requested, the function will return a single data frame.
#' If more than one output dataset is requested, the function will return
#' a list of data frames. If no statistics are specified in the \code{\link{out}}
#' function, the procedure will default to the statistics assigned on
#' the \code{stats} parameter above.  Note that this default behavior is different
#' from SAS®.
#' @param output A named list of output data requests.  The name of the list item
#' will become the name of the item in the return list, if there is one.  The
#' value of the list item is a output request created with the \code{\link{out}}
#' function.  Output data requests may also be made on the \code{...} parameter
#' for convenience.  This parameter is provided in case you need to generate
#' output requests dynamically inside a function.
#' @return The requested summary statistics.
#' @import fmtr
#' @import tibble
#' @export
proc_means <- function(data,
                       by = NULL,
                       class = NULL,
                       var = NULL,
                       stats = c("n", "mean", "std", "min", "max"),
              #        ways = NULL,   # Hold off for now
                       weight = NULL,
                       view = TRUE,
                       titles = NULL,
                       options = NULL,
                       ...,
                       output = NULL) {

  # SAS seems to always ignore these
  # Not sure why R as an option to keep them
  missing <- FALSE


  # Deal with single value unquoted parameter values
  oby <- deparse(substitute(by, env = environment()))
  by <- tryCatch({if (typeof(by) %in% c("character", "NULL")) by else oby},
                 error = function(cond) {oby})

  # Deal with single value unquoted parameter values
  oclass <- deparse(substitute(class, env = environment()))
  class <- tryCatch({if (typeof(class) %in% c("character", "NULL")) class else oclass},
                 error = function(cond) {oclass})

  ovar <- deparse(substitute(var, env = environment()))
  var <- tryCatch({if (typeof(var) %in% c("character", "NULL")) var else ovar},
                 error = function(cond) {ovar})

  ostats <- deparse(substitute(stats, env = environment()))
  stats <- tryCatch({if (typeof(stats) %in% c("character", "NULL")) stats else ostats},
                  error = function(cond) {ostats})

  # Parameter checks
  nms <- names(data)

  if (is.null(var)) {
    var <- c()
    for (nm in nms) {
      if (is.numeric(data[[nm]])) {

        var[length(var) + 1] <- nm
      }

    }
  }

  var <- var[!var %in% c(by, class)]

  if (!is.null(by)) {
    if (!all(by %in% nms)) {

      stop(paste("Invalid by name: ", by[!by %in% nms], "\n"))
    }
  }

  if (is.null(var)) {
    stop("var parameter is required.")
  } else {
    if (!all(var %in% nms)) {

      stop(paste("Invalid variable name: ", var[!var %in% nms], "\n"))
    }
  }

  if (!is.null(class)) {
    if (!all(class %in% nms)) {

      stop(paste("Invalid variable name: ", class[!class %in% nms], "\n"))
    }
  }

  if (is.null(stats)) {
    stop("stats parameter is required.")
  } else {
    st <- c( "css", "cv", "n", "mean", "median", "std", "min", "max",
             "nmiss", "nobs", "range", "sum", "stderr", "var", "clm", "uclm",
             "lclm","mode", "q1", "q3", "p1", "p5", "p10", "p20",
             "p25", "p30", "p40",
             "p50", "p60", "p70", "p75", "p80", "p90",
             "p95", "p99", "qrange", "aov")
    if (!all(tolower(stats) %in% st)) {

      stop(paste("Invalid stat name: ", stats[!tolower(stats) %in% st], "\n"))
    }
  }

  # Generate output specs
  if (!is.null(output))
    outreq <- get_output_specs_means(output, stats)
  else
    outreq <- get_output_specs_means(list(...), stats)

  rptflg <- FALSE
  rptnm <- ""
  if (has_report(outreq)) {
    rptflg <- TRUE
    rptnm <- get_report_name(outreq)
    outreq[[rptnm]] <- NULL
  }

  res <- NULL


  # Get report
  if (view == TRUE | rptflg) {
    rptres <- gen_report_means(data, by = by, var = var, class = class,
                            stats = stats, view = view,
                            titles = titles, weight = weight,
                            options = options)
  }

  # Get output
  if (length(outreq) > 0) {
    res <- gen_output_means(data,
                           by = by,
                           class = class,
                           var = var,
                           output = outreq
                           )
  }

  # Add report to result if requested
  if (rptflg & !is.null(rptres)) {

    if (is.null(res))
      res <- rptres
    else
      res[[rptnm]] <- rptres


  }

  # If only one dataset returned, remove list
  if (length(res) == 1)
    res <- res[[1]]


  return(res)
}


# Stats ---------------------------------------------------------------


get_output_specs_means <- function(outs, stats) {

  # outreq <- NULL

  # Set default statistics for output parameters
  # if (!is.null(outs)) {
  #   outreq <- outs  # need to check this
  #
  # } else {
    outreq <- outs
    if (length(outreq) >= 1) {
      for (nm in names(outreq)) {
        if ("out_req" %in% class(outreq[[nm]])) {
          if (is.null(outreq[[nm]]$stats)) {

            outreq[[nm]]$stats <- stats
          }
          if (is.null(outreq[[nm]]$shape)) {

            outreq[[nm]]$shape <- "long"
          }
        } else {

          outreq[[nm]] <- out(shape = "long")
        }
      }
    } else {

      outreq[["out"]] <- out(stats = stats, shape = "wide",
                             type = TRUE, freq = TRUE)

    }
  # }

  return(outreq)

}


# Purpose of this function is to prepare data for output.
# It will append the by, class, type, and freq columns to the subset df.
# These subsets will get stacked up in the driver function.
get_output <- function(data, var, stats, missing = FALSE,
                             shape = "wide", type = NULL, freq = FALSE,
                             by = NULL, class = NULL) {

  if (is.null(stats))
    stats <- c("n", "mean", "std", "min", "max")

  ret <- get_summaries(data, var, stats, missing = missing,
                       shape = shape)

  if (freq)
    ret <- cbind(data.frame(FREQ = nrow(data)), ret)


  if (!is.null(type))
    ret <- cbind(data.frame(TYPE = type), ret)

  if (!is.null(class)) {

    # Create a new vector of class names and values
    cv <- class

    if (length(cv) == 1)
      cnms <- "CLASS"
    else
      cnms <- paste0("CLASS", seq(1, length(cv)))

    names(cv) <- cnms

    # Put class names and values in a list so it
    # can be easily turned into a data frame.
    tmp <- list()
    for(nm in cnms) {
      tmp[[nm]] <- cv[nm]
    }

    # Convert to data frame
    df <- as.data.frame(tmp, stringsAsFactors = FALSE)

    # Clear out rownames
    rownames(df) <- NULL

    # Bind class columns to statistics
    ret <- cbind(df, ret)
  }

  if (!is.null(by)) {

    # Create a new vector of class names and values
    bv <- by

    if (length(bv) == 1)
      bnms <- "BY"
    else
      bnms <- paste0("BY", seq(1, length(bv)))

    names(bv) <- bnms

    tmp <- list()
    for(nm in names(bv)) {
      tmp[[nm]] <- bv[nm]
    }

    df <- as.data.frame(tmp, stringsAsFactors = FALSE)

    rownames(df) <- NULL

    ret <- cbind(df, ret)
  }

  return(ret)
}

# This is where most of the summary statistics get calculated.
get_summaries <- function(data, var, stats, missing = FALSE,
                          shape = "wide") {

  narm <- TRUE
  ret <- NULL

  for (nm in var) {

    if (!nm %in% names(data)) {

      stop(paste0("Variable '", nm, "' not found on input dataset."))
    } else {

      rw <- data.frame(VAR = nm, stringsAsFactors = FALSE)
      var <- data[[nm]]

      sts <- tolower(stats)
      #browser()

      for (st in sts) {

        if (st == "n") {

          rw[["N"]] <- sum(!is.na(var))
        }

        if (st == "css") {

          if (all(is.na(var)))
            rw[["CSS"]] <- NA
          else
            rw[["CSS"]] <- sum((var - mean(var, na.rm = narm))^2, na.rm = narm)
        }

        if (st == "cv") {

          if (all(is.na(var)))
            rw[["CV"]] <- NA
          else
            rw[["CV"]] <- sd(var, na.rm = narm) / mean(var, na.rm = narm) * 100
        }

        if (st == "mean") {

          if (all(is.na(var)))
            rw[["MEAN"]] <- NA
          else
            rw[["MEAN"]] <- mean(var, na.rm = narm)
        }

        if (st == "mode") {

          if (all(is.na(var)))
            rw[["MODE"]] <- NA
          else
            rw[["MODE"]] <- get_mode(var)
        }

        if (st == "max") {
          if (all(is.na(var)))
            rw[["MAX"]] <- NA
          else
            rw[["MAX"]] <- max(var, na.rm = narm)
        }

        if (st == "min") {

          if (all(is.na(var)))
            rw[["MIN"]] <- NA
          else
            rw[["MIN"]] <- min(var, na.rm = narm)
        }

        if (st == "median") {

          if (all(is.na(var)))
            rw[["MEDIAN"]] <- NA
          else
            rw[["MEDIAN"]] <- median(var, na.rm = narm)
        }

        if (st == "nobs") {

          rw[["NOBS"]] <- nrow(data)
        }

        if (st == "nmiss") {

          rw[["NMISS"]] <- sum(is.na(var))
        }

        if (st == "std") {

          if (all(is.na(var)))
            rw[["STD"]] <- NA
          else
            rw[["STD"]] <- sd(var, na.rm = narm)
        }

        if (st == "sum") {

          if (all(is.na(var)))
            rw[["SUM"]] <- NA
          else
            rw[["SUM"]] <- sum(var, na.rm = narm)
        }

        if (st == "range") {

          if (all(is.na(var))) {
            rw[["RANGE"]] <- NA
          } else {
          rng <- range(var, na.rm = narm)
            if (!is.null(rng) & length(rng) == 2)
              rw[["RANGE"]] <- rng[2] - rng[1]
            else
              rw[["RANGE"]] <- NA
          }
        }

        if (st == "var") {

          rw[["VARI"]] <- var(var, na.rm = narm)
        }

        if (st == "stderr") {

          rw[["STDERR"]] <- get_stderr(var, narm)
        }



        if (st == "lclm") {

          tmp <- get_clm(var, narm)

          rw[["LCLM"]] <- tmp[["lcl"]]


        }

        if (st == "uclm") {

          tmp <- get_clm(var, narm)

          rw[["UCLM"]] <- tmp[["ucl"]]


        }


        if (st == "clm") {

          tmp <- get_clm(var, narm)

          rw[["LCLM"]] <- tmp[["lcl"]]

          rw[["UCLM"]] <- tmp[["ucl"]]


        }

        if (st == "uss") {

          rw[["USS"]] <-  sum(var^2, na.rm = narm)
        }

        if (st == "p1") {

          rw[["P1"]] <- quantile(var, probs = c(0.01), type = 2, na.rm = narm)

        }

        if (st == "p5") {

          rw[["P5"]] <- quantile(var, probs = c(0.05), type = 2, na.rm = narm)

        }

        if (st == "p10") {

          rw[["P10"]] <- quantile(var, probs = c(0.1), type = 2, na.rm = narm)

        }

        if (st == "p20") {

          rw[["P20"]] <- quantile(var, probs = c(0.2), type = 2, na.rm = narm)

        }

        if (st == "p25") {

          rw[["P25"]] <- quantile(var, probs = c(0.25), type = 2, na.rm = narm)

        }

        if (st == "q1") {

          rw[["Q1"]] <- quantile(var, probs = c(0.25), type = 2, na.rm = narm)

        }

        if (st == "p30") {

          rw[["P30"]] <- quantile(var, probs = c(0.3), type = 2, na.rm = narm)

        }
        if (st == "p40") {

          rw[["P40"]] <- quantile(var, probs = c(0.4), type = 2, na.rm = narm)

        }

        if (st == "p50") {

          rw[["P50"]] <- quantile(var, probs = c(0.5), type = 2, na.rm = narm)

        }

        if (st == "p60") {

          rw[["P60"]] <- quantile(var, probs = c(0.6), type = 2, na.rm = narm)

        }
        if (st == "p70") {

          rw[["P70"]] <- quantile(var, probs = c(0.7), type = 2, na.rm = narm)

        }

        if (st == "p75") {

          rw[["P75"]] <- quantile(var, probs = c(0.75), type = 2, na.rm = narm)

        }

        if (st == "q3") {

          rw[["Q3"]] <- quantile(var, probs = c(0.75), type = 2, na.rm = narm)

        }

        if (st == "p80") {

          rw[["P80"]] <- quantile(var, probs = c(0.8), type = 2, na.rm = narm)

        }

        if (st == "p90") {

          rw[["P90"]] <- quantile(var, probs = c(0.9), type = 2, na.rm = narm)

        }

        if (st == "p95") {

          rw[["P95"]] <- quantile(var, probs = c(0.95), type = 2, na.rm = narm)

        }

        if (st == "p99") {

          rw[["P99"]] <- quantile(var, probs = c(0.99), type = 2, na.rm = narm)

        }

        if (st == "qrange") {

          q25 <- quantile(var, probs = c(0.25), type = 2, na.rm = narm)
          q75 <- quantile(var, probs = c(0.75), type = 2, na.rm = narm)

          rw[["QRANGE"]] <- q75 - q25

        }


      }

    }

    if (is.null(ret))
      ret <- rw
    else
      ret <- rbind(ret, rw)
  }

  if (!is.null(shape)) {
    ret <- shape_means_data(ret, shape)
  }


  return(ret)

}

shape_means_data <- function(ds, shape, copy = NULL) {

  # Assumed to be wide
  ret <- ds

  if (!is.null(shape)) {
    if (all(shape == "long")) {

      ret <- proc_transpose(ret, id = "VAR", name = "STAT", copy = copy)


    } else if (all(shape == "stacked")) {

      ret <- proc_transpose(ret, name = "STAT", by = "VAR", copy = copy)

      rnms <- names(ret)
      rnms[rnms %in% "COL1"] <- "VALUES"

      names(ret) <- rnms

    }
  }

  return(ret)

}



# Drivers --------------------------------------------------------------------
mlbls <- list(MEAN = "Mean", STD = "Std Dev", MEDIAN = "Median", MIN = "Minimum",
              MAX = "Maximum", VAR = "Variable", STDERR = "Std Err",
              STAT = "Statistics", VARI = "Variance", QRANGE = "Quantile Range",
              RANGE = "Range", MODE = "Mode", NMISS = "NMiss",
              LCLM = "Lower Conf. Limit", UCLM = "Upper Conf. Limit",
              TYPE = "Type", FREQ = "Frequency")

#' @import common
gen_report_means <- function(data,
                            by = NULL,
                            class = NULL,
                            var = NULL,
                            stats = c("n", "mean", "std", "min", "max"),
                            weight = NULL,
                            options = NULL,
                            view = TRUE,
                            titles = NULL) {

  # Declare return list
  res <- list()

  #browser()

  bylbls <- c()
  if (!is.null(by)) {

    lst <- unclass(data)[by]
    for (nm in names(lst))
      lst[[nm]] <- as.factor(lst[[nm]])
    dtlst <- split(data, lst, sep = "|")

    snms <- strsplit(names(dtlst), "|", fixed = TRUE)

    for (k in seq_len(length(snms))) {
      for (l in seq_len(length(by))) {
        lv <- ""
        if (!is.null(bylbls[k])) {
          if (!is.na(bylbls[k])) {
            lv <- bylbls[k]
          }
        }

        if (l == length(by))
          cma <- ""
        else
          cma <- ", "

        bylbls[k] <- paste0(lv, by[l], "=", snms[[k]][l], cma)
      }
    }

  } else {

    dtlst <- list(data)
  }

  # Loop through by groups
  for (j in seq_len(length(dtlst))) {

    # Get table for this by group
    dt <- dtlst[[j]]

    # data, var, class, outp, freq = TRUE,
    # type = NULL, byvals = NULL
    outp <- out(stats = stats, shape = "wide")
    smtbl <- get_class(dt, var, class, outp, freq = FALSE)

    aov <- NULL
    # Get aov if requested
    if (get_option(options, "aov", FALSE) & !is.null(class)) {

      bylbl <- NULL
      if (!is.null(by))
        bylbl <- bylbls[j]

      if (is.null(weight))
        aov <- get_aov(dt, var, class, bylbl = bylbl )
      else
        aov <- get_aov(dt, var, class, weight, bylbl = bylbl)

    }
    #smtbl <- get_summaries(dt, var, stats, missing)


    nm <- length(res) + 1

    # Add spanning headers if there are by groups
    if (!is.null(by) & !is.null(smtbl)) {

      # Add spanning headers
      spn <- span(1, ncol(smtbl), label = bylbls[j], level = 1)
      attr(smtbl, "spans") <- list(spn)

      nm <-  bylbls[j]
    }

    # Add default formats
    for (cnm in names(smtbl)) {

      if (typeof(smtbl[[cnm]]) %in% c("double")) {

        attr(smtbl[[cnm]], "format") <- "%.4f"
      }

    }

    # Assign labels
    if (is.null(class))
      labels(smtbl) <- mlbls
    else {

      cv <- class
      if (length(class) == 1)
        cnms <- "CLASS"
      else
        cnms <- paste0("CLASS", seq(1, length(class)))

      names(cv) <- cnms

      labels(smtbl) <- append(as.list(cv), mlbls)

    }

    # Convert to tibble if incoming data is a tibble
    if ("tbl_df" %in% class(data)) {
      res[[nm]] <- as_tibble(smtbl)
      if (!is.null(aov)) {

        res[[paste0(nm, " aov")]] <- as_tibble(aov)
      }
    } else {
      res[[nm]] <- smtbl

      if (!is.null(aov)) {
        res[[paste0(nm, " aov")]] <- as_tibble(aov)
      }
    }



  }

  gv <- options("procs.view")[[1]]
  if (is.null(gv))
    gv <- TRUE

  # Create viewer report if requested
  if (gv) {
    if (view == TRUE) {


      vrfl <- tempfile()

      out <- output_report(res, dir_name = dirname(vrfl),
                           file_name = basename(vrfl), out_type = "HTML",
                           titles = titles, margins = .5, viewer = TRUE)

      show_viewer(out)
    }
  }


  return(res)

}
#' @import fmtr
#' @import common
gen_output_means <- function(data,
                             by = NULL,
                             class = NULL,
                             var = NULL,
                             weight = NULL,
                             output = NULL) {

  res <- list()
  if (length(output) > 0) {

    nms <- names(output)
    for (i in seq_len(length(output))) {

      outp <- output[[i]]

      # Whether to include type variable
      tp <- 0
      if (outp$parameters$type %eq% FALSE)
        tp <- NULL

      # Whether to include freq variable
      frq <- TRUE
      if (outp$parameters$freq %eq% FALSE)
        frq <- FALSE

      # Create vector of NA class values
      cls <- NULL
      if (!is.null(class)) {
        cls <- rep(NA, length(class))
        names(cls) <- class

      }

      bdat <- list(data)
      if (!is.null(by)) {

        bdat <- split(data, data[ , by], sep = "|")

      }
      bynms <- names(bdat)

      # Make up by variable names for output ds
      byn <- NULL
      bylbls <- NULL
      if (!is.null(by)) {
        if (length(by) == 1)
          byn <- "BY"
        else
          byn <- paste0("BY", seq(1, length(by)))

        bylbls <- by
        names(bylbls) <- byn
      }

      tmpres <- NULL

      for (j in seq_len(length(bdat))) {

        dat <- bdat[[j]]

        # Deal with by variable values
        bynm <- NULL
        if (!is.null(bynms)) {
          bynm <- strsplit(bynms[j], "|", fixed = TRUE)[[1]]
          names(bynm) <- byn
        }

        if (!all(outp$stats == "aov")) {
          # Always add type 0
          tmpby <- get_output(dat, var = var,
                               by = bynm,
                               class = cls,
                               stats = outp$stats,
                               shape = outp$shape,
                               freq = frq,
                               type = tp)

          if (is.null(tmpres))
            tmpres <- tmpby
          else
            tmpres <- rbind(tmpres, tmpby)
        }

        if (!is.null(class)) {

          if (!is.null(tp))
            tp <- 1

          if (all(outp$stats == "aov")) {


            bylbl <- NULL
            if (!is.null(by))
              bylbl <- bylbls[j]

            for (vr in var) {
              if (is.null(weight)) {
                tmpaov <- get_aov(dat, vr, class, bylbl = bylbl, output = TRUE)
              } else {
                tmpaov <- get_aov(dat, vr, class, weight, bylbl = bylbl,
                                  output = TRUE)
              }

              tmpres <- rbind(tmpres, tmpaov)

            }

          } else if (any(outp$stats == "aov")) {

              tmpcls <- get_class(dat, var = var,
                                  class = class, outp = outp,
                                  freq = frq, type = tp, byvals = bynm)

              tmpres <- rbind(tmpres, tmpcls)


          } else {
            tmpcls <- get_class(dat, var = var,
                                class = class, outp = outp,
                                freq = frq, type = tp, byvals = bynm)

            tmpres <- rbind(tmpres, tmpcls)
          }
        }
      }

      # Where
      if (!is.null(outp$where)) {
        tmpres <- subset(tmpres, eval(outp$where))

      }

      # Drop
      if (!is.null(outp$drop)) {
        tnms <- names(tmpres)
        tmpres <- tmpres[ , !tnms %in% outp$drop]
      }

      # Keep
      if (!is.null(outp$keep)) {
        tnms <- names(tmpres)
        tmpres <- tmpres[ , tnms %in% outp$keep]
      }

      # Rename
      if (!is.null(outp$rename)) {
        tnms <- names(tmpres)
        rnm <- names(outp$rename)
        nnms <- replace(tnms, match(rnm, tnms), outp$rename)
        names(tmpres) <- nnms
      }

      # System Labels
      labels(tmpres) <- append(mlbls, bylbls)

      # Class labels
      clbls <- NULL
      if (!is.null(class)) {
        if (length(class) == 1)
          cnms <- "CLASS"
        else {
          cnms <- paste0("CLASS", seq(1, length(class)))
        }
        clbls <- as.list(class)
        names(clbls) <- cnms

        labels(tmpres) <- clbls
      }

      # User labels
      if (!is.null(outp$label))
        labels(tmpres) <- outp$label

      # Formats
      if (!is.null(outp$format))
        formats(tmpres) <- outp$format

      # Reset rownames
      rownames(tmpres) <- NULL

      res[[nms[i]]]  <- tmpres

    }
  }

  if (length(res) == 1)
    res <- res[[1]]


  return(res)

}


get_class <- function(data, var, class, outp, freq = TRUE,
                      type = NULL, byvals = NULL, weight = NULL) {


  res <- NULL
  aovds <- NULL

  clslist <- list(data)
  cnms <- NULL
  if (!is.null(class)) {
    clslist <- split(data, data[ , class], sep = "|")
    cnms <- names(clslist)
    if (length(clslist) == 0) {

      clslist <- list(data)
      cnms <- class

    }

    # Append aov stats here
    # AOV requires all the class variables, therefore
    # they can't be calculated in get_summaries.
    if (any("aov" %in% outp$stats)) {
      for (vr in var) {
        if (is.null(weight)) {
          tmpaov <- get_aov(data, vr, class, bylbl = byvals,
                            output = TRUE, resid = FALSE)
        } else {
          tmpaov <- get_aov(data, vr, class, weight, bylbl = byvals,
                            output = TRUE, resid = FALSE)
        }

        if (is.null(aovds))
          aovds <- tmpaov
        else
          aovds <- rbind(aovds, tmpaov)

      }
    }
  }




  for (k in seq_len(length(clslist))) {

    cnmv <- NULL
    if (!is.null(cnms)) {
      cnmv <- strsplit(cnms[k], "|", fixed = TRUE)[[1]]
      names(cnmv) <- class

    }

    if (!is.null(aovds)) {

      tmpres <- get_output(clslist[[k]], var = var,
                           by = byvals,
                           class = cnmv,
                           stats = outp$stats,
                           shape = "wide",
                           freq = freq,
                           type = type)

      tmpres <- cbind(tmpres, aovds[ , c("AOV.DF", "AOV.SUMSQ",
                                         "AOV.MEANSQ", "AOV.F", "AOV.P")])

      cpvars <- c("CLASS")
      if (!is.null(type))
        cpvars[length(cpvars) + 1] <- "TYPE"

      if (freq == TRUE)
        cpvars[length(cpvars) + 1] <- "FREQ"

      tmpres <- shape_means_data(tmpres, outp$shape, cpvars)


    } else {
      tmpres <- get_output(clslist[[k]], var = var,
                           by = byvals,
                           class = cnmv,
                           stats = outp$stats,
                           shape = outp$shape,
                           freq = freq,
                           type = type)
    }



    if (!is.null(res))
      res <- rbind(res, tmpres)
    else
      res <- tmpres
  }



  return(res)
}

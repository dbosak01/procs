


#' @title Sorts a data frame or tibble
#' @description Here is a description
#' @details Here is some details
#' @param data The input data to sort.
#' @param by The variables to sort by.
#' @param keep The variables on the output data to keep.
#' @param order The sort order of the variables on the by parameter.
#' Valid values are 'ascending' or 'descending'.  These values may
#' also be abbreviated to 'asc', 'desc', 'a', or 'd'.  You may pass
#' a vector of order values equal to the number of variables on the by
#' parameter.  Default is 'ascending' for all by variables.
#' @param nodupkey Whether to remove duplicates.  Valid values are
#' TRUE or FALSE. Duplicate rows will be identified by the variables listed on
#' the by parameter.  Other variables not listed on the by parameter will
#' be ignored.
#' @return The sorted dataset.  If a data frame was input, a
#' data frame will be output.  If a tibble was input, a tibble will
#' be output.
#' @import tibble
#' @export
proc_sort <- function(data,  by = NULL, keep = NULL, order = "ascending",
                      nodupkey = FALSE) {


  # Deal with single value unquoted parameter values
  oby <- deparse(substitute(by, env = environment()))
  by <- tryCatch({if (typeof(by) %in% c("character", "NULL")) by else oby},
                 error = function(cond) {oby})

  # Deal with single value unquoted parameter values
  okeep <- deparse(substitute(keep, env = environment()))
  keep <- tryCatch({if (typeof(keep) %in% c("character", "NULL")) keep else okeep},
                 error = function(cond) {okeep})


  # Deal with single value unquoted parameter values
  oorder <- deparse(substitute(order, env = environment()))
  order <- tryCatch({if (typeof(order) %in% c("character", "NULL")) order else oorder},
                 error = function(cond) {oorder})

  if (!is.null(order)) {
    order <- substr(tolower(order), 1, 1)
    if (length(order) < length(by))
      order <- rep_len(order, length(by))
  }
  asc <- ifelse(order == "a", TRUE, FALSE)

  # If null, set by with all variables
  if (is.null(by)) {

    by <- names(data)
  }

  # If null, set var with all variables
  if (is.null(keep)) {

    keep <- names(data)
  }



  if (nodupkey) {
    dt <- data[ , by]
    dups <- duplicated(dt)
    data <-  data[!dups, ]

  }


  ret <- sort(data, by = by, ascending = asc )

  # Order and keep
  ret <- ret[ , keep, drop = FALSE]


  if ("tbl_df" %in% class(data)) {

    ret <- as_tibble(ret)
  }

  # unique(prt[, c("sex", "internship")])

  log_sort(data,
           by = by,
           keep = keep,
           order = order,
           nodupkey = nodupkey,
           outdata = ret)

  return(ret)

}



log_sort <- function(data,
                     by = NULL,
                     keep = NULL,
                     order = "ascending",
                     nodupkey = FALSE,
                     outdata = NULL) {

  ret <- c()

  indt <- paste0(rep(" ", 11), collapse = "")

  ret <- paste0("proc_sort: input data set ", nrow(data),
                " rows and ", ncol(data), " columns")

  if (!is.null(by))
    ret[length(ret) + 1] <- paste0(indt, "by: ", paste(by, collapse = " "))

  if (!is.null(keep))
    ret[length(ret) + 1] <- paste0(indt, "keep: ",
                                   paste(keep, collapse = " "))

  if (!is.null(order))
    ret[length(ret) + 1] <- paste0(indt, "order: ",
                                   paste(order, collapse = " "))

  if (!is.null(nodupkey))
    ret[length(ret) + 1] <- paste0(indt, "nodupkey: ",
                                   paste(nodupkey, collapse = " "))

  if (!is.null(outdata))
    ret[length(ret) + 1]  <- paste0(indt, "output data set ", nrow(outdata),
                " rows and ", ncol(outdata), " columns")


  log_logr(ret)

}

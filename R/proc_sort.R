


#' @title Sorts a dataset
#' @description The \code{proc_sort} function sorts a dataset according to
#' the variables passed on the \code{by} parameter.  If no parameters are
#' passed on the \code{by} parameter, it will sort by all variables. The
#' direction of the sort is controlled with the \code{order} parameter.
#' Use the \code{nodupkey} parameter to eliminate duplicate rows from
#' the dataset, and the \code{keep} parameter to subset columns.
#' The parameters will accept either quoted or unquoted values.
#' @param data The input data to sort.
#' @param by A vector of variables to sort by.
#' @param keep A vector of variables on the output data to keep.  All other
#' variables will be dropped.
#' @param order The sort order of the variables on the by parameter.
#' Valid values are 'ascending' or 'descending'.  These values may
#' also be abbreviated to 'asc', 'desc', 'a', or 'd'.  You may pass
#' a vector of order values equal to the number of variables on the by
#' parameter.  Default is 'ascending' for all by variables.
#' @param nodupkey Whether to remove duplicates.  Valid values are
#' TRUE or FALSE. Duplicate rows will be identified by the variables listed on
#' the by parameter if passed.  Otherwise, the function will dedupe on
#' all variables returned.
#' @return The sorted dataset.  If a data frame was input, a
#' data frame will be output.  If a tibble was input, a tibble will
#' be output.
#' @examples
#' # Prepare data subset
#' dat <- data.frame(HairEyeColor, stringsAsFactors = FALSE)[1:32 %% 4 == 1, ]
#'
#' # View data
#' dat
#' #     Hair   Eye    Sex Freq
#' # 1  Black Brown   Male   32
#' # 5  Black  Blue   Male   11
#' # 9  Black Hazel   Male   10
#' # 13 Black Green   Male    3
#' # 17 Black Brown Female   36
#' # 21 Black  Blue Female    9
#' # 25 Black Hazel Female    5
#' # 29 Black Green Female    2
#'
#' # Sort by Frequency
#' res1 <- proc_sort(dat, by = Freq)
#'
#' # View results
#' res1
#' #    Hair   Eye    Sex Freq
#' # 29 Black Green Female    2
#' # 13 Black Green   Male    3
#' # 25 Black Hazel Female    5
#' # 21 Black  Blue Female    9
#' # 9  Black Hazel   Male   10
#' # 5  Black  Blue   Male   11
#' # 1  Black Brown   Male   32
#' # 17 Black Brown Female   36
#'
#' # Sort by Frequency descending
#' res2 <- proc_sort(dat, by = Freq, order = d)
#'
#' # View results
#' res2
#' #     Hair   Eye    Sex Freq
#' # 17 Black Brown Female   36
#' # 1  Black Brown   Male   32
#' # 5  Black  Blue   Male   11
#' # 9  Black Hazel   Male   10
#' # 21 Black  Blue Female    9
#' # 25 Black Hazel Female    5
#' # 13 Black Green   Male    3
#' # 29 Black Green Female    2
#'
#' # Get unique combinations of Eye and Sex
#' res3 <- proc_sort(dat, keep = v(Eye, Sex), nodupkey = TRUE)
#'
#' # View results
#' res3
#' #      Eye    Sex
#' # 1  Brown   Male
#' # 17 Brown Female
#' # 5   Blue   Male
#' # 21  Blue Female
#' # 9  Hazel   Male
#' # 25 Hazel Female
#' # 13 Green   Male
#' # 29 Green Female
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

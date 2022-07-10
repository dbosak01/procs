


#' @title Sorts a data.frame or tibble
#' @description Here is a description
#' @details Here is some details
#' @param data The input data to sort.
#' @param var The variables on the output data to keep.
#' @param by The variables to sort by.
#' @param nodupkey Whether to remove duplicates.
#' @return The sorted dataset.  If a data frame was input, a
#' data frame will be output.  If a tibble was input, a tibble will
#' be output.
#' @export
proc_sort <- function(data, var = NULL, by = NULL, nodupkey = FALSE) {


  # If null, set by with all variables
  if (is.null(by)) {

    by <- names(data)
  }

  # If null, set var with all variables
  if (is.null(var)) {

    var <- names(data)
  }

  if (nodupkey) {

    dups <- duplicated(data[, by])
    data <-  data[!dups, ]

  }

  # Get order vector
  ordr <- do.call('order', data[, by])

  # Order and select
  ret <- data[ordr, var]

  # unique(prt[, c("sex", "internship")])

  return(ret)

}

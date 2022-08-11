


# Output function ---------------------------------------------------------



#' @title Function to request an output dataset
#' @description The \code{out} function is used to request an output
#' dataset.  The function is used as a parameter to multiple procedures in
#' the \strong{procs} package.
#' @details Here is some details
#' @param stats The requested statistics. The available statistics depend
#' on the function the output parameter applies to.
#' @param shape How the output dataset should be organized.  Valid
#' values are "long", "wide", or "stacked".  The default is "long".
#' @param report Whether to output the tables produced for the
#' procedure report.  Valid values are TRUE and FALSE.  Default is FALSE.
#' If the parameter is TRUE, the function will output all datasets
#' produced by the report functionality, and in the same order.  If the
#' parameter is TRUE, all other output options will be ignored.
#' @param where An expression used to filter the output dataset.  Use
#' \code{expression} function to define the where clause.  The where
#' clause will be applied prior to other output options such as drop, keep,
#' or rename.
#' @param drop A vector of variables to drop from the output dataset.
#' @param keep A vector of variables to keep on the output dataset.
#' This parameter can also be used to order the output columns.
#' @param rename A named vector of columns to rename.  The name of the vector
#' item should correspond to the original output column name.  The value
#' of the vector should correspond to the new variable name.
#' @param format A named list of formats to assign to the output dataset.
#' @param label A named vector of labels to assign to the output dataset.
#' @param ... Various options.  When used with \code{\link{proc_freq}},
#' the \code{table} option is most frequently used.
#' @return The output specifications.
#' @export
out <- function(stats = NULL,
                shape = NULL, report = FALSE, where = NULL,
                drop = NULL, keep = NULL, rename = NULL,
                format = NULL, label = NULL,
                ...) {

  ret <- structure(list(), class = c("out_req", "list"))


  ret$stats <- stats
  ret$shape <- shape

  lst <- list(...)
  for (nm in names(lst)) {
    if (nm == "table") {
      ret$table <- lst[[nm]]
      lst[[nm]] <- NULL
    }
  }

  ret$parameters <-  lst
  ret$report <- report
  ret$drop <- drop
  ret$keep <- keep
  ret$rename <- rename
  ret$format <- format
  ret$where <- where
  ret$label <- label

  return(ret)
}


#' @title Specifies options
#' @description The \code{output} function is a generic options collection.
#' Used on multiple functions and parameters.
#' @details Here is some details
#' @param ... Various options.
#' @return An options class with the requested options.
#' @export
opts <- function(...) {

  ret <- tryCatch({
          if (typeof(...) == "list")
             structure(..., class = c("opts", "list"))

         }, error = function(cond) {

          structure(list(...), class = c("opts", "list"))
        })

  # if (typeof(...) == "list") {
  #   ret <- structure(..., class = c("opts", "list"))
  # } else {
  #
  #   ret <- structure(list(...), class = c("opts", "list"))
  # }


  return(ret)

}




# Output function ---------------------------------------------------------



# @title Function to request an output dataset
# @description The \code{out} function is used to request an output
# dataset.  The function is used as a parameter to multiple procedures in
# the \strong{procs} package.
# @details Here is some details
# @param stats The requested statistics. The available statistics depend
# on the function the output parameter applies to.
# @param shape How the output dataset should be organized.  Valid
# values are "long", "wide", or "stacked".  The default is "long".
# @param report Whether to output the tables produced for the
# procedure report.  Valid values are TRUE and FALSE.  Default is FALSE.
# If the parameter is TRUE, the function will output all datasets
# produced by the report functionality, and in the same order.  If the
# parameter is TRUE, all other output options will be ignored.
# @param where An expression used to filter the output dataset.  Use
# \code{expression} function to define the where clause.  The where
# clause will be applied prior to other output options such as drop, keep,
# or rename.
# @param drop A vector of variables to drop from the output dataset.
# @param keep A vector of variables to keep on the output dataset.
# This parameter can also be used to order the output columns.
# @param rename A named vector of columns to rename.  The name of the vector
# item should correspond to the original output column name.  The value
# of the vector should correspond to the new variable name.
# @param format A named list of formats to assign to the output dataset.
# @param label A named vector of labels to assign to the output dataset.
# @param table A table request.  This parameter is used only for the
# \code{\link{proc_freq}} function.
# @param ... Various options.  When used with \code{\link{proc_freq}},
# the \code{table} option is most frequently used.
# @return The output specifications.
out <- function(stats = NULL,
                shape = NULL, report = FALSE, where = NULL,
                drop = NULL, keep = NULL, rename = NULL,
                format = NULL, label = NULL, table = NULL,
                ...) {

  ret <- structure(list(), class = c("out_req", "list"))

  # Single var NSE
  ostats <- deparse(substitute(stats, env = environment()))
  stats <- tryCatch({if (typeof(stats) %in% c("character", "NULL")) stats else ostats},
                 error = function(cond) {ostats})

  odrop <- deparse(substitute(drop, env = environment()))
  drop <- tryCatch({if (typeof(drop) %in% c("character", "NULL")) drop else odrop},
                 error = function(cond) {odrop})

  okeep <- deparse(substitute(keep, env = environment()))
  keep <- tryCatch({if (typeof(keep) %in% c("character", "NULL")) keep else okeep},
                   error = function(cond) {okeep})

  otable <- deparse(substitute(table, env = environment()))
  table <- tryCatch({if (typeof(table) %in% c("character", "NULL")) table else otable},
                   error = function(cond) {otable})


  ret$stats <- stats
  ret$shape <- shape
  ret$table <- table
  ret$parameters <- list(...)
  ret$report <- report
  ret$drop <- drop
  ret$keep <- keep
  ret$rename <- rename
  ret$format <- format
  ret$where <- where
  ret$label <- label

  return(ret)
}


# @title Specifies options
# @description The \code{output} function is a generic options collection.
# Used on multiple functions and parameters.
# @details Here is some details
# @param ... Various options.
# @return An options class with the requested options.
# opts <- function(...) {
#
#   ret <- tryCatch({
#           if (typeof(...) == "list")
#              structure(..., class = c("opts", "list"))
#
#          }, error = function(cond) {
#
#           structure(list(...), class = c("opts", "list"))
#         })
#
#   # if (typeof(...) == "list") {
#   #   ret <- structure(..., class = c("opts", "list"))
#   # } else {
#   #
#   #   ret <- structure(list(...), class = c("opts", "list"))
#   # }
#
#
#   return(ret)
#
# }



#' @title Prints a dataset
#' @description The \code{proc_print} function is used to print a dataset
#' or datasets.  To print multiple datasets, pass them to the \code{data}
#' parameter in a list.
#' By default, the function prints to the viewer. It may also be used
#' to print to the file system using the \code{report_type} and
#' \code{report_location} parameters.
#' @details Here is some details
#' @param data The data to print.
#' @param titles A vector of titles.
#' @param report_type The type of report to create.
#' @param report_location The location of the report to create.
#' @param view Whether to send the print output to the viewer.  Valid
#' values are TRUE and FALSE.
#' @import reporter
#' @export
proc_print <- function(data, titles = NULL, report_type = "HTML",
                       report_location = NULL, view = TRUE) {



}

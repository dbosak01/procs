

#' @title A sample function
#' @encoding UTF-8
#' @description Here is a sample function.
#' @details
#' Some details about the sample function.
#' @param data The input data frame to perform frequency calculations on.
#' @param by An optional by group.
#' @param tables The variable or variables to perform frequency counts on.
#' @param table_options The option specifications for the table parameter.
#' @param test The test to use for the frequency.
#' @param test_options The option specifications for the test parameter.
#' @param weight An optional weight parameter.
#' @param weight_options The option specifications for the weight parameter.
#' @param print The output type for any report output.  Valid values are
#' 'HTML', 'TXT', 'PDF', 'RTF', 'DOCX', or 'none'.  Multiple outputs can be
#' requested by passing a vector of output types.  If the print_location parameter
#' is specified, the outputs will be written to that location.  Otherwise,
#' they will be written to a temp directory.  The default value is 'HTML'.
#' @param print_location A full path to write any report output requested by
#' the print parameter. If the location is not specified, files will be
#' written to a temp directory.  Default is NULL.
#' @param titles A vector of one or more titles to use for the report output.
#' @param digits The number of digits to round statistics produced
#' by the function.
#' @return A list of data frames that contain the requested frequency tables.
#' @export
proc_freq <- function(data,
                      by = NULL,
                      tables = NULL,
                      table_options = NULL,
                      test = NULL,
                      test_options = NULL,
                      weight = NULL,
                      weight_options = NULL,
                      print = "HTML",
                      print_location = NULL,
                      titles = NULL,
                      digits = 4) {

  res <- list()


  for (tb in tables) {

    var <- data[[tb]]

    categories <- names(sort(table(var)))
    frequencies <- as.vector(sort(table(var)))

    n <- sum(frequencies)
    percentages <- round(frequencies / n, digits)*100
    cum_frequencies <- cumsum(frequencies)
    cum_percentages <- cumsum(percentages)

    result <- data.frame("Category" = categories,
                         "Frequency" = frequencies,
                         "Percentage" = percentages,
                         "Cumulative.Frequency" = cum_frequencies,
                         "Cumulative.Percentage" = cum_percentages,
                         stringsAsFactors = FALSE)

    res[[length(res) + 1]] <- result
  }

  if (!any(print %in% c("none"))) {

    loc <- get_location(print, print_location)
    out <- output_report(res, proc_type = 'freq',
                         path = print_location, out_type = print)


    if (any(print %in% c("HTML"))) {

      show_viewer(out)
    }

  }

  return(res)

}



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
#' @param print_location A path to write any report output requested by
#' the print parameter. The path may be either a full path will file name,
#' or a directory name.  If the file name is not specified, the procedure
#' type will be used.  If no path is specified, files will be
#' written to a temp directory.  Default is NULL.
#' @param titles A vector of one or more titles to use for the report output.
#' @param digits The number of digits to round statistics produced
#' by the function.
#' @return A list of data frames that contain the requested frequency tables.
#' @import fmtr
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
  # print("Orig print_location")
  # print(print_location)

  for (tb in tables) {

    var <- data[[tb]]

    if (is.null(weight)) {

      categories <- names(sort(table(var)))
      frequencies <- as.vector(sort(table(var)))

    } else {

      cnts <- aggregate(data[[weight]], list(data[[tb]]), FUN = sum)
      categories <- cnts$Group.1
      frequencies <- cnts$x
    }

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

    lbl <- attr(data[[tb]], "label")
    if (is.null(lbl))
      lbl <- tb


    labels(result) <- c(Category = lbl,
                        Cumulative.Frequency = "Cumulative Frequency",
                        Cumulative.Percentage = "Cumulative Percentage")

    res[[tb]] <- result
  }

  if (!any(print %in% c("none"))) {

    loc <- get_location(print, "freq", print_location)
    out <- output_report(res, proc_type = 'freq', dir_name = loc["dir_name"],
                         file_name = loc["file_name"], out_type = print,
                         titles = titles)

    # print("Print location:")
    # print(loc)
    # print("Out value:")
    # print(out)

    if (any(print %in% c("HTML"))) {

      show_viewer(paste0(file.path(loc["dir_name"], loc["file_name"]), '.html'))
    }

  }

  return(res)

}

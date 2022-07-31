

#' @title Prints a dataset
#' @description The \code{proc_print} function is used to print a dataset
#' or datasets.  To print multiple datasets, pass them to the \code{data}
#' parameter in a list.
#' By default, the function prints to the viewer. It may also be used
#' to print to the file system using the \code{output_type} and
#' \code{file_path} parameters.
#' @details Here are some details
#' @param data The data to print.  Can be either a single dataset, or
#' a list of datasets.
#' @param file_path The path of the report to print.
#' @param output_type The type of report to create.
#' @param titles A vector of titles.
#' @param style A style object.
#' @param view Whether to send the print output to the viewer.  Valid
#' values are TRUE and FALSE.
#' @import reporter
#' @export
proc_print <- function(data, file_path = NULL, output_type = "HTML",
                       titles = NULL, style = NULL, view = TRUE) {


  out <- NULL

  inp <- data
  if ("data.frame" %in% class(data))
    inp <- list(data)


  if (!is.null(file_path)) {

    if (file.exists(file_path)) {
      file.remove(file_path)
    }

    dn <- dirname(file_path)
    if (!dir.exists(dn)) {
      dir.create(dn)
    }


    out <- output_report(inp, dir_name = dirname(file_path),
                       file_name = basename(file_path), out_type = output_type,
                       style = style,
                       titles = titles, margins = 1)

  }

  if (view == TRUE) {


    vrfl <- tempfile()

    res <- output_report(inp, dir_name = dirname(vrfl),
                         file_name = basename(vrfl), out_type = "HTML",
                         titles = titles, margins = .5, viewer = TRUE,
                         style = style)

    show_viewer(res)
  }





  return(out)
}

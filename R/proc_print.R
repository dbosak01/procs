

#' @title Prints a dataset
#' @description The \code{proc_print} function is used to print a dataset
#' or datasets.  To print multiple datasets, pass them to the \code{data}
#' parameter in a list.
#' By default, the function prints to the viewer. It may also be used
#' to print to the file system using the \code{output_type} and
#' \code{file_path} parameters.  This print function has limited options,
#' and is meant to quickly view your data or dump it out to a file.
#' For more reporting options, use the \strong{\link{reporter}} package.
#' @param data The data to print.  Can be either a single dataset, or
#' a list of datasets.
#' @param file_path The path of the report to print.
#' @param output_type The type of report to create.  Valid values are
#' "TXT", "RTF", "PDF", "HTML", and "DOCX".  Default is "HTML".
#' @param titles A vector of titles.
#' @param style A style object, as defined by the \strong{\link{reporter}}
#' package.  See that package for details.
#' @param view Whether to send the print output to the viewer.  Valid
#' values are TRUE and FALSE. Default is TRUE.
#' @return If a file report was produced, the full path of the report.
#' Otherwise, a NULL.  In either case, the value will be returned invisibly.
#' @examples
#' # Turn off printing to pass CRAN checks
#' options("procs.print" = FALSE)
#'
#' # Print mtcars to the viewer
#' proc_print(mtcars)
#'
#' # Print mtcars to an RTF
#' pth <- proc_print(mtcars,
#'                   file_path = tempfile(fileext = ".rtf"),
#'                   titles = "MTCARS Proc Print Example",
#'                   output_type = "RTF", view = FALSE)
#'
#' # View file
#' # file.show(pth)
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

  if (view == TRUE && interactive()) {


    vrfl <- tempfile()

    res <- output_report(inp, dir_name = dirname(vrfl),
                         file_name = basename(vrfl), out_type = "HTML",
                         titles = titles, margins = .5, viewer = TRUE,
                         style = style)

    show_viewer(res)
  }


  log_prnt(data, file_path = file_path, output_type = output_type,
            titles = titles, style = style, view = view)



  invisible(out)
}



log_prnt <- function(data, file_path = NULL, output_type = NULL,
                      titles = NULL, style = NULL, view = NULL) {

  ret <- c()

  indt <- paste0(rep(" ", 12), collapse = "")

  if ("data.frame" %in% class(data))
    dscnt <- 1
  else
    dscnt <- length(data)


  ret <- paste0("proc_print: printed ", dscnt, ifelse(dscnt == 1, " dataset", "datasets"))


  if (!is.null(file_path))
    ret[length(ret) + 1] <- paste0(indt, "file_path: ", paste(file_path, collapse = " "))

  if (!is.null(output_type))
    ret[length(ret) + 1] <- paste0(indt, "output_type: ",
                                   paste(output_type, collapse = " "))

  if (!is.null(titles))
    ret[length(ret) + 1] <- paste0(indt, "titles: ",
                                   paste(titles, collapse = " \n"))

  if (!is.null(style))
    ret[length(ret) + 1] <- paste0(indt, "style: ",
                                   paste(style, collapse = " "))

  if (!is.null(view))
    ret[length(ret) + 1] <- paste0(indt, "view: ",
                                   paste(view, collapse = " "))


  log_logr(ret)

}



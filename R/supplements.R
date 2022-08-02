


# Output function ---------------------------------------------------------



#' @title Function to request an output dataset
#' @description The \code{out} function is used to request an output
#' dataset.  The function is used as a parameter to multiple procedures in
#' the \strong{procs} package.
#' @details Here is some details
#' @param stats The requested statistics. The available statistics depend
#' on the function the output parameter applies to.
#' @param direction How the output dataset should be organized.  Valid
#' values are "long" or "wide".  The default is "long".
#' @param report Whether to output the tables produced for the
#' procedure report.
#' @param ... Various options.
#' @return The output specifications.
#' @export
out <- function(stats = NULL,
                direction = NULL, report = FALSE, ...) {

  ret <- structure(list(), class = c("out_req", "list"))


  ret$stats <- stats
  ret$direction <- direction

  lst <- list(...)
  for (nm in names(lst)) {
    if (nm == "table") {
      ret$table <- lst[[nm]]
      lst[[nm]] <- NULL
    }
  }

  ret$parameters <-  lst
  ret$report = report

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

  ret <- structure(list(...), class = c("opts", "list"))


  return(ret)

}

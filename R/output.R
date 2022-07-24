


# Output function ---------------------------------------------------------



#' @title Specifies output dataset parameters
#' @description The \code{output} function is used to request an output
#' dataset.  The function is used as a parameter to multiple functions in
#' the \strong{procs} package.
#' @details Here is some details
#' @param stats The requested statistics. The available statistics depend
#' on the function the output parameter applies to.
#' @param direction How the output dataset should be organized.  Valid
#' values are "long" or "wide".  The default is "long".
#' @param ... Various options.
#' @return The output specifications.
#' @import tibble
#' @export
output <- function(stats = NULL, direction = NULL, ...) {

  ret <- structure(list(), class = c("output_spec", "list"))


  ret$stats <- stats
  ret$direction <- direction
  ret$parameters <-  list(...)

  return(ret)
}

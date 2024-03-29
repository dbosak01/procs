#' @title procs: Simulates Procedures from SAS®
#'
#' @description The \strong{procs} package contains functions to
#' simulate procedures from SAS® software. The intention of the package is
#' to ease transition to R
#' adoption by providing SAS® programmers a familiar conceptual framework and
#' functions that produce nearly identical output.
#'
#' @details
#' Despite having similar analytical capabilities, SAS® and R are very different
#' languages.  The \strong{procs} package helps SAS® users become productive
#' in R by offering functions that mimic SAS® procedures.  The functions
#' have a similar syntax, and produce similar output. Most importantly,
#' the functions aim to match SAS® statistical results to a high precision.
#' This achievement will foster smoother transitions from one language to
#' another, and reduce delays in explaining the differences in statistical
#' output.
#'
#' @section Available Functions:
#' The \strong{procs} package contains the following major functions:
#' \itemize{
#'   \item \code{\link{proc_freq}}: Performs frequency  counts and statistics.
#'   \item \code{\link{proc_means}}: Generates summary statistics.
#'   \item \code{\link{proc_ttest}}: Performs hypothesis testing.
#'   \item \code{\link{proc_reg}}: Performs regression testing.
#'   \item \code{\link{proc_transpose}}: A function to pivot datasets.
#'   \item \code{\link{proc_sort}}: Sorts and dedupes a dataset.
#'   \item \code{\link{proc_print}}: An easy dataset print function.
#' }
#'
#' See function documentation for additional details.
#' @keywords internal
#' @aliases procs-package
#' @name procs
"_PACKAGE"

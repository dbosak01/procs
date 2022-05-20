

#' @import reporter
#' @noRd
output_report <- function(lst, path = NULL, out_type = 'HTML') {

  if (is.null(path)) {
    targetDir <- tempfile()
    if (!file.exists(targetDir))
      dir.create(targetDir)
  } else {
    targetDir <- path
  }

  rpt <- create_report(pth)

  for (dat in lst) {

    tbl <- create_table(dat)
    rpt <- add_content(rpt, tbl, align = 'center', page_break = FALSE)

  }


  for (ot in out_type) {

    flnm <- paste("freq.", tolower(ot))

    if (is.null(path)) {

      pth <- file.path(targetDir, flnm)

    } else {

      pth <- file.path(path, flnm)

    }

    res <- write_report(rpt, file_path = pth, output_type = ot)

  }

  return(res)
}

#' @noRd
show_viewer <- function(path) {

  flg <- FALSE

  if (file.exists(path)) {

    viewer <- getOption("viewer")

    if (!is.null(viewer))
      viewer(path)
    else
      utils::browseURL(path)

    flg <- TRUE
  }


  return(flg)

}

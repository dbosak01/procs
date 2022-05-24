

#' @import reporter
#' @noRd
output_report <- function(lst, proc_type = NULL,
                          path = NULL, out_type = 'HTML',
                          titles = NULL) {

  if (is.null(path)) {
    targetDir <- tempdir()
    if (!file.exists(targetDir))
      dir.create(targetDir)

    flnm <- proc_type

  } else {
    targetDir <- dirname(path)

    flnm <- filenm(basename(path))
  }

  rpt <- create_report(font = 'Arial')



  for (i in seq_len(length(lst))) {

    tbl <- create_table(lst[[i]], borders = "all")
    if (!is.null(titles) & i == 1) {
      tbl <- titles(tbl, titles)
    }
    rpt <- add_content(rpt, tbl, align = 'center', page_break = FALSE)



  }


  ret <- c()

  for (ot in out_type) {

    fl <- paste0(flnm, ".", tolower(ot))
    pth <- file.path(targetDir, fl)


    res <- write_report(rpt, file_path = pth, output_type = ot)

    ret[length(ret) + 1] <- res$modified_path

  }

  return(ret)
}

#' @noRd
show_viewer <- function(path) {

  pth <- ""

  if (file.exists(path)) {

    targetDir <- tempdir()
    if (!grepl(targetDir, path, fixed = TRUE)) {

      pth <- file.path(targetDir, basename(path))

      if (file.exists(pth))
        file.remove(pth)

      file.copy(path, pth)

    } else {
     pth <- path
    }

    viewer <- getOption("viewer")

    if (!is.null(viewer))
      viewer(pth)
    else
      utils::browseURL(pth)

  }


  return(pth)

}

#' @noRd
get_location <- function(print, location) {

  if (is.null(location) | file.exists(location) == FALSE) {
    targetDir <- tempdir()

  } else {
    targetDir <- location
  }

  if (file.exists(targetDir) == FALSE) {
    dir.create(targetDir)

  }

  return(targetDir)

}


#' @noRd
filenm <- function(path) {

  flnm <- basename(path)

  pos <- strsplit(flnm, ".", fixed = TRUE)
  flnm <- pos[[1]][1]

  return(flnm)

}



#' @import reporter
#' @noRd
output_report <- function(lst, proc_type,
                          dir_name, file_name, out_type = 'HTML',
                          titles = NULL) {


  if (is.null(dir_name)) {
    stop("Path cannot be null")

  } else {


    targetDir <- dir_name

    flnm <- file_name
  }

  rpt <- create_report(font = 'Arial')



  for (i in seq_len(length(lst))) {

    tbl <- create_table(lst[[i]], borders = c("top", "bottom"))
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

    dir <- tempdir()
    if (dir.exists(dir) == FALSE)
      dir.create(dir)

    pth <- tempfile(fileext = ".html")

    file.copy(path, pth)


    viewer <- getOption("viewer")

    if (!is.null(viewer))
      viewer(pth)
    else
      utils::browseURL(pth)

    ret <- TRUE

  }


  return(pth)

}

#' @noRd
get_location <- function(print, proc_type, location) {

  ret <- c(dir_name = "",
           file_name = "")

  if (is.null(location)) {

    ret["dir_name"] <- tempdir()
    ret["file_name"] <- proc_type

  } else if (dir.exists(location)) {

    ret["dir_name"] <- location
    ret["file_name"] <- proc_type

  } else {
    ret["dir_name"] <- dirname(location)
    ret["file_name"] <- filenm(location)
  }

  if (dir.exists(ret["dir_name"] ) == FALSE) {
    dir.create(ret["dir_name"] )

  }


  return(ret)

}


#' @noRd
filenm <- function(path) {

  flnm <- basename(path)

  pos <- strsplit(flnm, ".", fixed = TRUE)
  flnm <- pos[[1]][1]

  return(flnm)

}

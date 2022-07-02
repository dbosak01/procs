
# Exported Utilities ------------------------------------------------------

#' @title Simulates SAS® Rounding
#' @description A function to simulate the way SAS® performs rounding.
#' @param x A vector of values to round.
#' @returns The rounded data vector.
#' @export
round_up <- function(x) {



}


#' @title Compare data sets
#' @description A function to compare two data sets  This function
#' may be used to compare R and SAS output.  It has a parameters to
#' control the desired precision and the rounding to be used.
#' @param ds1 The first dataset to compare.
#' @param ds2 The second dataset to compare.
#' @param precision The desired number of decimal places to compare.
#' @param rounding The rounding function to be used in the comparison.
#' @returns The results of the comparison.
#' @export
compare_ds <- function(ds1, ds2, precision = NULL, rounding = NULL) {

}




# Internal Utilities ------------------------------------------------------



#' @import reporter
#' @noRd
output_report <- function(lst, proc_type,
                          dir_name, file_name, out_type = 'HTML',
                          style = NULL,
                          titles = NULL, margins = 1, viewer = FALSE) {


  if (is.null(dir_name)) {
    stop("Path cannot be null")

  } else {


    targetDir <- dir_name

    if (dir.exists(targetDir) == FALSE)
      dir.create(targetDir)

    flnm <- file_name
  }

  rpt <- create_report(font = 'Arial', orientation = 'portrait', missing = "")
  rpt <- set_margins(rpt, top = margins, bottom = margins,
                     left = margins, right = margins)



  if (!is.null(style)) {

    if (any(class(style) %in% "style_spec")) {

      rpt <- add_style(rpt, style = style)

    } else if (all(class(style) %in% "character")) {

      rpt <- add_style(rpt, theme = style)

    } else {

      stop("Report style parameter value is invalid.")
    }

  } else {

    rpt <- add_style(rpt, theme = "SASDefault")

  }


  for (i in seq_len(length(lst))) {
    dt <- lst[[i]]


    if (viewer == TRUE) {

      if ("Category" %in% names(dt)) {
        lbl <-  attr(dt$Category, "label")
        if (is.null(lbl))
          lbl <- "Category"
        nms <- names(dt)
        names(dt) <- gsub("Category", "stub", nms, fixed = TRUE)
      }

      # Create table
      tbl <- create_table(dt, borders = c("all"))

      # Dedupe stub column if it exists
      if ("stub" %in% names(dt)) {

        wth <- rpt$char_width * nchar(lbl)
        tbl <- define(tbl, "stub", dedupe = TRUE, label =lbl, width = wth,
                      standard_eval = "true")

      }

    } else {

      # Create table
      tbl <- create_table(dt, borders = c("top", "bottom"))
    }

    #

    # Add titles
    if (!is.null(titles) & i == 1) {
      tbl <- titles(tbl, titles)
    }

    #browser()
    # Add spanning headers if requested
    spns <- attr(dt, "spans")
    if (!is.null(spns)) {

      for (i in seq_len(length(spns))) {
        spn <- spns[[i]]
        tbl <- spanning_header(tbl, label = spn$label, from = as.character(spn$start),
                               to = as.character(spn$end), level = spn$level,
                               standard_eval = TRUE)

      }

    }

    # Append table to report
    rpt <- add_content(rpt, tbl, align = 'center', page_break = FALSE)


  }


  ret <- c()

  # Deal with multiple output types
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

    pth <- path

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
get_location <- function(proc_type, location) {

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

#' @noRd
option_true <- function(options, name, default = FALSE) {

  ret <- default

  if (!is.null(options)) {
    vl <- options[[name]]
    if (!is.null(vl)) {
      if (vl == TRUE)
        ret <- TRUE
      else
        ret <- FALSE
    }

  }

  return(ret)

}

has_option <- function(options, name) {


  ret <- FALSE

  if (!is.null(options)) {
    vl <- options[[name]]
    if (!is.null(vl)) {
      ret <- TRUE
    }
  }

  return(ret)

}


get_option <- function(options, name, default = NULL) {


  ret <- default

  if (!is.null(options)) {
    vl <- options[[name]]
    if (!is.null(vl)) {
      ret <- vl
    }
  }

  return(ret)

}

get_name <- function(nm = NULL, var = NULL, bylbl = NULL) {

  vnm <- ""

  if (is.null(nm))
    vnm <- var
  else if (nchar(nm) == 0)
    vnm <- var
  else
    vnm <- nm

  if (length(bylbl) == 0) {

    ret <- vnm
  } else {

    ret <- paste0(bylbl, vnm)

  }


  return(ret)
}


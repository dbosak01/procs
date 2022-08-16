


# Report Utilities ------------------------------------------------------



#' @import reporter
#' @noRd
output_report <- function(lst,
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

      if ("CAT" %in% names(dt)) {
        lbl <-  attr(dt$CAT, "label")
        if (is.null(lbl))
          lbl <- "CAT"
        nms <- names(dt)
        names(dt) <- gsub("CAT", "stub", nms, fixed = TRUE)
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
      tbl <- create_table(dt, borders = c("outside"))
    }

    #

    # Add titles
    if (!is.null(titles) & i == 1) {
      tbl <- titles(tbl, titles)
    }

    #browser()
    # Add spanning headers if requested
    # spns <- attr(dt, "spans")
    # if (!is.null(spns)) {
    #
    #   for (i in seq_len(length(spns))) {
    #     spn <- spns[[i]]
    #     tbl <- spanning_header(tbl, label = spn$label, from = as.character(spn$start),
    #                            to = as.character(spn$end), level = spn$level,
    #                            standard_eval = TRUE)
    #
    #   }
    #
    # }

    # Append table to report
    rpt <- add_content(rpt, tbl, align = 'center', page_break = FALSE)


  }


  ret <- c()

  # Deal with multiple output types
  for (ot in out_type) {

    fl <- flnm
    if (all(grepl(".", flnm, fixed = TRUE) == FALSE))
      fl <- paste0(flnm, ".", tolower(ot))

    pth <- file.path(targetDir, fl)


    if (utils::packageVersion("reporter") >= "1.3.6") {

      res <- write_report(rpt, file_path = pth,
                          output_type = ot, log = !viewer)
    } else {
      res <- write_report(rpt, file_path = pth, output_type = ot)

    }

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



has_report <- function(outreq) {

  ret <- FALSE

  if (!is.null(outreq)) {
    for (itm in outreq) {

      if (!is.null(itm$report)) {
        if (itm$report == TRUE)
          ret <- TRUE

      }
    }
  }


  return(ret)

}

get_report_name <- function(outreq) {

  ret <- ""

  if (!is.null(outreq)) {

    nms <- names(outreq)
    for (nm in nms) {

      if (!is.null(outreq[[nm]]$report)) {
        if (outreq[[nm]]$report == TRUE)
          ret <- nm

      }
    }
  }


  return(ret)

}


# Option utilities --------------------------------------------------------



#' @noRd
option_true <- function(options, name, default = FALSE) {

  ret <- default


  #browser()
  # print("option_true")
  # print(options)

  if (!is.null(options)) {
    names(options) <- tolower(names(options))

    vl <- options[[tolower(name)]]
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

  # print("has_option")
  # print(options)

  if (!is.null(options)) {

    names(options) <- tolower(names(options))

    vl <- options[[tolower(name)]]
    if (!is.null(vl)) {
      ret <- TRUE
    }
  }

  return(ret)

}


get_option <- function(options, name, default = NULL) {

  # print("get_option")
  # print(name)
  # print(options)


  ret <- default

  if (!is.null(options)) {

    names(options) <- tolower(names(options))

    vl <- options[[tolower(name)]]
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


# Other Utilities ---------------------------------------------------------


fill_missing <- function(ds, num) {

  nas <- rep(NA, num - 1)
  nw <- list()

  for (nm in names(ds)) {

    nw[[nm]] <- nas
  }

  dfn <- as.data.frame(nw, stringsAsFactors = FALSE)

  ret <- rbind(ds, nw)


  return(ret)

}

get_by_ds <- function(byvals) {

  ret <- as.data.frame(I(byvals), stringsAsFactors = FALSE)
  names(ret) <- names(byvals)
  rownames(ret) <- NULL

  return(ret)
}


#' @noRd
log_logr <- function(x) {

  if (length(find.package('logr', quiet=TRUE)) > 0) {
    if (utils::packageVersion("logr") >= "1.2.0") {
      logr::log_hook(x)
    }
  }
}

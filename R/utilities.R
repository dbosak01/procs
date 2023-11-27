


# Report Utilities ------------------------------------------------------



#' @import reporter
#' @noRd
output_report <- function(lst,
                          dir_name, file_name, out_type = 'HTML',
                          style = NULL,
                          titles = NULL, margins = 1,
                          viewer = FALSE,
                          pages = 1) {


  if (is.null(dir_name)) {
    stop("Path cannot be null")

  } else {


    targetDir <- dir_name

    if (dir.exists(targetDir) == FALSE)
      dir.create(targetDir)

    flnm <- file_name
  }

  rpt <- create_report(font = 'Arial', orientation = 'portrait', missing = "",
                       font_size = 8)
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

    thm <- get_theme("SASDefault")
    thm$font_size <- 8
    thm$title_font_size <- 10

    rpt <- add_style(rpt, style = thm)

  }

  for (j in seq_len(pages)) {


    if (pages == 1) {
      pg <- lst

    } else {
      pg <- lst[[j]]
    }

    for (i in seq_len(length(pg))) {

      brk <- FALSE
      if (pages > 1 & i == length(pg))
        brk <- TRUE

      # Assign page data
      dt <- pg[[i]]

      if ("data.frame" %in% class(dt)) {
        if (viewer == TRUE) {

          if ("CAT" %in% names(dt)) {
            lbl <-  attr(dt$CAT, "label")
            if (is.null(lbl))
              lbl <- "CAT"
            nms <- names(dt)
            names(dt) <- gsub("CAT", "stub", nms, fixed = TRUE)
          }

          if ("stub" %in% names(dt)) {
           lbl <- attr(dt$stub, "label")
           if (is.null(lbl))
             lbl <- ""

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

          tt <- titles

          if (pages > 1) {
            tt <- c(tt, names(lst)[j])
          }

          tbl <- titles(tbl, tt)
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
        rpt <- add_content(rpt, tbl, align = 'center', page_break = brk)



      } else if ("ggplot" %in% class(dt)) {

        if (viewer == TRUE) {

          # Create plot content
          fig <- create_plot(dt, height = 3, width = 3.5)

        } else {

          # Create plot content
          fig <- create_plot(dt, height = 4, width = 5)


        }

        # Add plot content
        rpt <- add_content(rpt, fig, align = 'center', page_break = brk)


      }
    } # Tables


  } # Pages


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

  #print(paste0("path: ", path))

  if (file.exists(path)) {

    opts <- options("procs.print")[[1]]
    if (is.null(opts))
      opts <- TRUE

    if (opts == TRUE) {

      pth <- path

      viewer <- getOption("viewer")

      if (!is.null(viewer))
        viewer(pth)
      else
        utils::browseURL(pth)

    }

  }


  return(pth)

}

#' @noRd
# get_location <- function(proc_type, location) {
#
#   ret <- c(dir_name = "",
#            file_name = "")
#
#   if (is.null(location)) {
#
#     ret["dir_name"] <- tempdir()
#     ret["file_name"] <- proc_type
#
#   } else if (dir.exists(location)) {
#
#     ret["dir_name"] <- location
#     ret["file_name"] <- proc_type
#
#   } else {
#     ret["dir_name"] <- dirname(location)
#     ret["file_name"] <- filenm(location)
#   }
#
#   if (dir.exists(ret["dir_name"] ) == FALSE) {
#     dir.create(ret["dir_name"] )
#
#   }
#
#
#   return(ret)
#
# }


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

# get_report_name <- function(outreq) {
#
#   ret <- ""
#
#   if (!is.null(outreq)) {
#
#     nms <- names(outreq)
#     for (nm in nms) {
#
#       if (!is.null(outreq[[nm]]$report)) {
#         if (outreq[[nm]]$report == TRUE)
#           ret <- nm
#
#       }
#     }
#   }
#
#
#   return(ret)
#
# }


# Option utilities --------------------------------------------------------



#' @noRd
option_true <- function(options, name, default = FALSE) {

  ret <- NULL


  if (!is.null(options)) {
    nms <- names(options)

    # Try to find named option
    if (!is.null(nms)) {

      if (tolower(name) %in% tolower(nms)) {
        # Force names to lower case
        names(options) <- tolower(nms)
        # See if name exists
        vl <- options[[tolower(name)]]
        if (!is.null(vl)) {
          if (vl == TRUE)
            ret <- TRUE
          else
            ret <- FALSE
        }
      }
    }

    # Try to find keyword
    if (is.null(ret)) {

      if (tolower(name) %in% tolower(options)) {

        ret <- TRUE
      }
    }
  }

  # Give up and send default
  if (is.null(ret)) {
    ret <- default
  }

  return(ret)

}

has_option <- function(options, name) {


  ret <- NULL


  if (!is.null(options)) {
    nms <- names(options)

    # Try to find named option
    if (!is.null(nms)) {
      if (tolower(name) %in% tolower(nms)) {
        ret <- TRUE
      }
    }

    # Try to find keyword
    if (is.null(ret)) {

      if (tolower(name) %in% tolower(options)) {

        ret <- TRUE
      }
    }
  }

  if (is.null(ret))
    ret <- FALSE

  return(ret)

}


get_option <- function(options, name, default = NULL) {

  ret <- NULL


  if (!is.null(options)) {
    nms <- names(options)

    # Try to find named option
    if (!is.null(nms)) {

      if (tolower(name) %in% tolower(nms)) {

        # Force names to lower case
        names(options) <- tolower(nms)
        # See if name exists
        vl <- options[[tolower(name)]]
        if (!is.null(vl)) {
          ret <- tryCatch(eval(str2expression(vl)),
                          error = function(cond){vl})
        }
      }
    }

    # Try to find keyword
    if (is.null(ret)) {

      if (tolower(name) %in% tolower(options)) {

        ret <- TRUE
      }
    }
  }

  # Give up and send default
  if (is.null(ret)) {
    ret <- default
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

has_output <- function(outpt) {

  glb <- options("procs.interactive")[[1]]

  ret <- TRUE

  if (!is.null(glb)) {

    if (glb == TRUE)
      ret <- FALSE
  }

  if (!is.null(outpt)) {
    if (any(tolower(outpt) %in% c("report", "none"))) {
      ret <- FALSE
    }

    if (any(tolower(outpt) %in% c("out"))) {
      ret <- TRUE
    }

  }

  return(ret)
}

has_view <- function(opts) {

  glb <- options("procs.interactive")[[1]]

  ret <- TRUE

  if (!is.null(glb)) {

    if (glb == FALSE)
      ret <- FALSE
  }


  if (!is.null(opts)) {
    if (any(tolower(opts) %in% c("noprint"))) {
      ret <- FALSE
    }

    if (any(tolower(opts) %in% c("print"))) {
      ret <- TRUE
    }

  }

  return(ret)

}

has_report<- function(outpt) {


  ret <- FALSE


  if (!is.null(outpt)) {
    if (any(tolower(outpt) %in% c("report"))) {
      ret <- TRUE
    }

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

# get_by_ds <- function(byvals) {
#
#   ret <- as.data.frame(I(byvals), stringsAsFactors = FALSE)
#   names(ret) <- names(byvals)
#   rownames(ret) <- NULL
#
#   return(ret)
# }


#' @noRd
log_logr <- function(x) {

  if (length(find.package('logr', quiet=TRUE)) > 0) {
    if (utils::packageVersion("logr") >= "1.2.0") {
      logr::log_hook(x)
    }
  }
}


# Check if logr.output option is set or not
log_output <- function() {

  ret <- TRUE
  tmp <- options("logr.output")
  if (!is.null(tmp$logr.output)) {

    ret <- tmp$logr.output

  }

  return(ret)
}


# parse_tables <- function(nms, tstr) {
#
#  vl <- "D * (A--C)"
#
#   rng <- regexec("[a-zA-Z0-9.]+\\-\\-[a-zA-Z0-9.]+", vl)
#
#   rng
#
# }


get_alpha <- function(opts) {

  ret <- .05

  if (!is.null(opts)) {

    nms <- tolower(names(opts))
    if ("alpha" %in% nms) {

      ret <- opts[["alpha"]]

      if (!is.numeric(ret)) {

        ret <- as.numeric(ret)
      }
    }

  }

  return(ret)
}


get_maxdec <- function(opts) {

  ret <- "%.7f"

  if (!is.null(opts)) {

    nms <- tolower(names(opts))
    if ("maxdec" %in% nms) {

      tmp <- opts[["maxdec"]]

      if (!is.numeric(tmp)) {

        tmp <- as.numeric(tmp)
      }

      if (is.numeric(tmp)) {

        ret <- paste0("%.", tmp, "f")
      }
    }

  }

  return(ret)
}



restore_datatypes <- function(res, data, class, clsnms = NULL) {


  if (is.null(clsnms))
    clsnms <- get_class_names(class)

    for (k in seq_len(length(class))) {
      if (nrow(data) == 0) {

        res[[clsnms[k]]] <- NA

      } else {
        if (all(class(data[[class[k]]]) == "factor")) {

          res[[clsnms[k]]] <- factor(res[[clsnms[k]]], levels = attr(data[[class[k]]], "levels"))

        } else if (all(class(data[[class[k]]]) == "Date")) {

          res[[clsnms[k]]] <- as.Date(res[[clsnms[k]]])

        } else if (typeof(data[[class[k]]]) == "integer") {

          res[[clsnms[k]]] <- as.integer(res[[clsnms[k]]])

        } else if (typeof(data[[class[k]]]) == "numeric") {

          res[[clsnms[k]]] <- as.numeric(res[[clsnms[k]]])
        }
      }

    }


  return(res)
}


get_class_names <- function(class) {


  # Create name lookup
  if (length(class) == 1)
    clsnms <- "CLASS"
  else
    clsnms <- paste0("CLASS", seq(1, length(class)))

  names(clsnms) <- class

  return(clsnms)

}







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

        # Add titles
        tt <- NULL
        if (i == 1) {

          if (!is.null(titles))
            tt <- titles

        }

        if (!is.null(attr(dt, "ttls")))
          tt <- c(tt, attr(dt, "ttls"))

        if (!is.null(tt))
          tbl <- titles(tbl, tt)

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
        if (any(class(data[[class[k]]]) == "factor")) {

          if (any(class(data[[class[k]]]) == "ordered")) {
            res[[clsnms[k]]] <- factor(res[[clsnms[k]]],
                                       levels = attr(data[[class[k]]], "levels"),
                                       ordered = TRUE)
          } else {
            res[[clsnms[k]]] <- factor(res[[clsnms[k]]],
                                       levels = attr(data[[class[k]]], "levels"))
          }

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




# Perform the set operation.  Works on main
# dataset plus one or more datasets.
perform_set <- function(dta, stdta) {

  # Save off class
  dtacls <- class(dta)

  # Work with pure data frames.
  # Tibbles will mess with names.
  dta <- as.data.frame(dta)

  # Put in list
  if ("data.frame" %in% class(stdta))
    dtalst <- list(stdta)
  else
    dtalst <- stdta

  # Collect Names
  fnms <- names(dta)

  # Assign counter to ensure stacking
  dta[["..ds"]] <- 0

  ret <- dta

  # Stack datasets
  for (i in seq_len(length(dtalst))){

    tmp <- as.data.frame(dtalst[[i]])
    nnms <- names(tmp)
    fnms <- c(fnms, nnms[!nnms %in% fnms])
    tmp[["..ds"]] <- i
    ret <- merge(ret, tmp, all = TRUE, sort = FALSE)

  }

  # Clean up counter
  ret[["..ds"]] <- NULL
  dta[["..ds"]] <- NULL

  # Rename so first dataset drives naming
  # Can easily break if name has been changed.
  ret <- tryCatch({ret[ , fnms]}, error = function(cond){ret})

  # Restore attributes
  ret <- copy_attributes_sp(dta, ret)
  ret <- copy_df_attributes(dta, ret)

  # Restore original class
  class(ret) <- dtacls

  return(ret)

}

copy_attributes_sp <- function(df1, df2) {

  ret <- df2

  for (nm in names(df2)) {

    col <- df1[[nm]]
    if (!is.null(col)) {
      for (at in names(attributes(col))) {

        if (!at %in% c("levels")) {

          attr(ret[[nm]], at) <- attr(col, at)
        }

      }
    }

  }

  return(ret)
}


# Copies attributes on data frame from one df to another
# Skips rownames and names, which can cause trouble.
copy_df_attributes <- function(src, trgt) {

  atts <- attributes(src)

  ret <- trgt

  for (anm in names(atts)) {

    if (!anm %in% c("names", "row.names")) {
      attr(ret, anm) <- atts[[anm]]
    }
  }

  return(ret)
}


get_ttest_type <- function(txt) {

  ret <- txt
  if (length(grep(":", txt, fixed = TRUE)) > 0) {

    pos <- unlist(gregexpr(':', txt, fixed = TRUE))[1]
    ret <- substring(txt, pos + 1)

  }


  return(ret)

}


# Function to split the paired parameter string and create
# separate variables for VAR1, VAR2, and DIFF
add_paired_vars <- function(dt, vlbl, shape) {

  if (is.null(vlbl)) {

    ret <- dt

  } else {

    if (shape %in% c("wide", "stacked")) {
      nms <- names(dt)
      slbl <- strsplit(vlbl, "-", fixed = TRUE)
      pos <- match("VAR", nms)
      ret <- dt

      ret[["LBL"]] <- as.integer(sub("..diff", "", ret[["VAR"]], fixed = TRUE))
      ret[["DIFF"]] <- ret[["VAR"]]
      for (i in seq_len(nrow(ret))) {
        lbl <- ret[["LBL"]][i]
        ret[["VAR1"]][i] <- trimws(slbl[[lbl]][1])
        ret[["VAR2"]][i] <- trimws(slbl[[lbl]][2])
      }
      ret[["VAR"]] <- NULL
      ret[["LBL"]] <- NULL

      # Rearrange df
      prenms <- c()
      if (pos > 1)
        prenms <- nms[seq(1, pos - 1)]

      postnms <- nms[seq(pos + 1, length(nms))]

      nmv <- c(prenms, "VAR1", "VAR2", "DIFF", postnms)
      ret <- ret[ , nmv]

    } else if (shape == "long") {

      nms <- names(dt)
      rnms <- sub("..diff", "DIFF", nms, fixed = TRUE)
      ret <- dt
      names(ret) <- rnms


    } else {

      ret <- dt
    }

  }


  return(ret)

}

# A function to replace temporary variable names with real ones
fix_var_names <- function(dat, varnms, varlbls, shp, dnam) {

  ret <- dat
  if (!is.null(varnms) & !is.null(varlbls)) {

    lkp <- varlbls
    names(lkp) <- varnms

    if (shp == "long" & dnam == "TTests") {

      nms <- names(dat)
      nnms <- c()

      for (i in seq_len(length(nms))) {

        if (nms[i] %in% names(lkp)) {
          nnms[i] <- lkp[nms[i]]
        } else {
          nnms[i] <- nms[i]
        }
      }

      names(ret) <- nnms

    } else if (shp == "stacked" & dnam == "TTests") {

      nms <- names(dat)
      if ("VAR" %in% nms) {
        ret[["VAR"]] <- lkp[ret[["VAR"]]]
      }
    }
  }

  return(ret)
}

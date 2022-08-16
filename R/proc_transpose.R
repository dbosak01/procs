
#' @title Transposes a Data Frame
#' @encoding UTF-8
#' @description A function to pivot or transpose a data frame. In the default
#' usage, the variables identified by the parameter \code{var} are transposed
#' to become rows. The variable values in the parameter \code{id} become
#' the new column names.  The function has several more parameters to control
#' how variables are named in the transposed data set.
#' @details
#' The \code{proc_tranpose} function takes an input data frame or tibble
#' and transposes the columns and rows.
#'
#' @param data The input data frame for which will be transposed.
#' @param by An optional by group.  Parameter accepts a vector of one or more
#' quoted variable names. If the by group is requested, the data will be subset
#' by that variable and the transpose function will
#' transpose each group and stack them together in a single table.
#' @param var The variable or variables to transpose.  Parameter accepts a vector
#' of quoted variable names.
#' @param id The variable or variables to use for the transposed column names.
#' @param idlabel The variable to use for the transposed column labels.
#' @param copy A vector of variables to retain in the output data
#' without transposition.
#' @param name Specifies the name of the variable to be used for the
#' var values.
#' @param namelabel The label to use for the name variable.
#' @param prefix Contains a prefix to be used in the construction of
#' column names.
#' @param delimiter Specifies a delimiter to be used in the construction
#' of column names.
#' @param suffix Contains a suffix to be used in the construction of
#' column names.
#' @param where An expression to filter the rows after the transform
#' is complete.  Use the \code{\link{expression}} function to define
#' the where clause.
#' @param log Whether or not to log the procedure.  Default is TRUE.
#' This parameter is used internally.
#' @return A data frame that contains the transposed data. If a data frame
#' is input, a data frame will be output.  If a tibble is input, a tibble
#' will be output.
# @import stats
#' @examples
#' # Create data
#' dt <- data.frame(names = rownames(mtcars), mtcars, stringsAsFactors = FALSE)[1:5,]
#'
#' # Transpose data
#' tdt <- proc_transpose(dt, var = c("mpg", "cyl", "disp"),
#'                           id = "names", name = "Variable")
#'
#' # View transposed data
#' tdt
#' #   Variable Mazda RX4 Mazda RX4 Wag Datsun 710 Hornet 4 Drive Hornet Sportabout
#' # 1      mpg        21            21       22.8           21.4              18.7
#' # 2      cyl         6             6        4.0            6.0               8.0
#' # 3     disp       160           160      108.0          258.0             360.0
#' @import tibble
#' @import fmtr
#' @export
proc_transpose <- function(data,
                           by = NULL,
                           var = NULL,
                           id = NULL,
                           idlabel = NULL,
                           copy = NULL,
                           name = "NAME",
                           namelabel = NULL,
                           prefix = NULL,
                           delimiter = ".",
                           suffix = NULL,
                           where = NULL,
                           log = TRUE
                           ) {

  if (!"data.frame" %in% class(data)) {

   stop("Input data must be inherited from a data frame.")
  }

  # Deal with single value unquoted parameter values
  oby <- deparse(substitute(by, env = environment()))
  by <- tryCatch({if (typeof(by) %in% c("character", "NULL")) by else oby},
                 error = function(cond) {oby})

  # Deal with single value unquoted parameter values
  ovar <- deparse(substitute(var, env = environment()))
  var <- tryCatch({if (typeof(var) %in% c("character", "NULL")) var else ovar},
                 error = function(cond) {ovar})

  # Deal with single value unquoted parameter values
  oid <- deparse(substitute(id, env = environment()))
  id <- tryCatch({if (typeof(id) %in% c("character", "NULL")) id else oid},
                 error = function(cond) {oid})

  # Deal with single value unquoted parameter values
  oidlabel <- deparse(substitute(idlabel, env = environment()))
  idlabel <- tryCatch({if (typeof(idlabel) %in% c("character", "NULL")) idlabel else oidlabel},
                 error = function(cond) {oidlabel})

  # Deal with single value unquoted parameter values
  ocopy <- deparse(substitute(copy, env = environment()))
  copy <- tryCatch({if (typeof(copy) %in% c("character", "NULL")) copy else ocopy},
                 error = function(cond) {ocopy})

  oname <- deparse(substitute(name, env = environment()))
  name <- tryCatch({if (typeof(name) %in% c("character", "NULL")) name else oname},
                   error = function(cond) {oname})

  onamelabel <- deparse(substitute(namelabel, env = environment()))
  namelabel <- tryCatch({if (typeof(namelabel) %in% c("character", "NULL")) namelabel else onamelabel},
                   error = function(cond) {onamelabel})

  # Parameter checks
  nms <- names(data)
  if (!is.null(by)) {
    if (!all(by %in% nms)) {

      stop(paste("Invalid by name: ", by[!by %in% nms], "\n"))
    }
  }

  if (!is.null(var)) {
    if (!all(var %in% nms)) {

      stop(paste("Invalid var name: ", var[!var %in% nms], "\n"))
    }
  }

  if (!is.null(id)) {
    if (!all(id %in% nms)) {

      stop(paste("Invalid id name: ", id[!id %in% nms], "\n"))
    }
  }

  if (!is.null(idlabel)) {
    if (!all(idlabel %in% nms)) {

      stop(paste("Invalid idlabel name: ", idlabel[!idlabel %in% nms], "\n"))
    }
  }

  if (!is.null(copy)) {
    if (!all(copy %in% nms)) {

      stop(paste("Invalid copy name: ", copy[!copy %in% nms], "\n"))
    }
  }


   ret <- NULL
   retlst <- list()

   #browser()

   if (!is.null(var)) {
    nms <- var

   } else {

     # Use all numeric variables by default
     nms <- c()
     for (nm in names(data)) {
       if (is.numeric(data[[nm]])) {
         nms[length(nms) + 1] <- nm
       }

     }
   }

   nms <- nms[!nms %in% c(by, id, idlabel, copy)]

   if (length(nms) == 0) {
     stop("No variables to transpose.  You may need to specify the var parameter")

   }

   # Prepare list of labels for by groups
   bylbls <- list()
   if (!is.null(by)) {

     lst <- unclass(data)[by]
     for (nm in names(lst))
       lst[[nm]] <- as.factor(lst[[nm]])
     dtlst <- split(data, lst, sep = "|", drop = TRUE)

     bylbls <- strsplit(names(dtlst), "|", fixed = TRUE)

     for (k in seq_len(length(bylbls))) {

         names(bylbls[[k]]) <- by
     }

   } else {

     dtlst <- list(data)
   }

   for (j in seq_len(length(dtlst))) {

     # Get table for this by group
     dt <- dtlst[[j]]

     #browser()
     # Select desired column names
     tpd <- dt[, nms]

     # Select copy columns
     if (!is.null(copy)) {
       if (length(copy) == 1) {
         cpy <- as.data.frame(dt[, copy])
         names(cpy) <- copy
       } else
         cpy <- dt[, copy]
     } else
       cpy <- NULL

     # Create ID column
     ret1 <- data.frame(name = nms, stringsAsFactors = FALSE)

     if (!is.null(name))
       names(ret1) <- name

     # Transpose data
     ret2 <- as.data.frame(t(tpd), stringsAsFactors = FALSE)

     #browser()

     bygrps <- NULL
     if (!is.null(by)) {

       bygrps <- list()
       for (grp in seq_len(length(by))) {

        bygrps[[by[[grp]]]] <- bylbls[[j]][[grp]]
       }

       bygrps <- as.data.frame(bygrps, stringsAsFactors = FALSE)

     }

     # Recycle copy rows
     if (!is.null(cpy)) {
       if (nrow(cpy) != nrow(ret1)) {
         cpy2 <- list()
         for(cnm in names(cpy))
          cpy2[[cnm]] <- rep_len(cpy[[cnm]], nrow(ret1))

         cpy <- as.data.frame(cpy2)
       }

     }

     # Combine ID column and transposed columns
     if (!is.null(by)) {
       if (!is.null(cpy))
         ret <- cbind(bygrps, cpy, ret1, ret2)
       else
         ret <- cbind(bygrps, ret1, ret2)
     } else {
       if (!is.null(cpy))
         ret <- cbind(cpy, ret1, ret2)
       else
         ret <- cbind(ret1, ret2)
     }

     rownames(ret) <- NULL

     # Convert to tibble
     if ("tbl_df" %in% class(data)) {
       ret <- as_tibble(ret)

     }

     nms_new <- c()

     # Assign column names
     if (!is.null(id)) {

       idcat <- c()
       cntr <- 1
       for (idnm in id) {
         if (cntr == length(id))
           idcat <- paste0(idcat, dt[[idnm]])
         else
           idcat <- paste0(idcat, dt[[idnm]], sep = delimiter)
         cntr <- cntr + 1
       }

       nms_new <- paste0(prefix, idcat, suffix)

       names(ret) <- c(by, copy, name,  nms_new)

     } else {

       if (nrow(dt) > 0) {
         nms_new <- paste0("COL", seq(1, nrow(dt)))
         names(ret) <- c(by, copy, name,  nms_new)
       } else {
         names(ret) <- name
       }

     }

     # Retain original column order for by and copy variables
     if (!is.null(by) & !is.null(copy)) {
       rnms <- names(data)
       nnms <- names(ret)
       ret <- ret[ , c(rnms[rnms %in% c(by, copy)], nnms[!nnms %in% c(by, copy)])]
     }

     # Where
     if (!is.null(where)) {
       ret <- subset(ret, eval(where))

     }

     # Assign name label
     if (!is.null(namelabel)) {

       attr(ret[[name]], "label") <-  namelabel
     }

     # Assign ID labels
     if (!is.null(idlabel)) {

       lbls <- dt[[idlabel]]
       names(lbls) <- nms_new
       labels(ret) <- lbls

     }

     rownames(ret) <- NULL

     retlst[[length(retlst) + 1]] <- ret
   }


   # Always at least one result
   res <- retlst[[1]]

   # Append by groups if necessary
   if (!is.null(by)) {
    for (p in seq(2, length(retlst))) {
      res <- rbind(res, retlst[[p]])
    }
   }

   if (log) {
     log_transpose(data,
                   by = by,
                   var = var,
                   id = id,
                   idlabel = idlabel,
                   copy = copy,
                   name = name,
                   namelabel = namelabel,
                   where = where,
                   outdata = res)
   }

 return(res)

}


log_transpose <- function(data,
                      by = NULL,
                      var = NULL,
                      id = NULL,
                      idlabel = NULL,
                      copy = NULL,
                      name = "NAME",
                      namelabel = NULL,
                      where = NULL,
                      outdata = NULL) {

  ret <- c()

  indt <- paste0(rep(" ", 16), collapse = "")

  ret <- paste0("proc_transpose: input data set ", nrow(data),
                " rows and ", ncol(data), " columns")

  if (!is.null(by))
    ret[length(ret) + 1] <- paste0(indt, "by: ", paste(by, collapse = " "))


  if (!is.null(var))
    ret[length(ret) + 1] <- paste0(indt, "var: ",
                                   paste(var, collapse = " "))

  if (!is.null(id))
    ret[length(ret) + 1] <- paste0(indt, "id: ",
                                   paste(id, collapse = " "))

  if (!is.null(idlabel))
    ret[length(ret) + 1] <- paste0(indt, "idlabel: ",
                                   paste(idlabel, collapse = " "))


  if (!is.null(copy))
    ret[length(ret) + 1] <- paste0(indt, "copy: ", paste(copy, collapse = " "))

  if (!is.null(name))
    ret[length(ret) + 1]<- paste0(indt, "name: ", paste(name, collapse = " "))

  if (!is.null(namelabel))
    ret[length(ret) + 1]<- paste0(indt, "namelabel: ", paste(namelabel, collapse = " "))

  if (!is.null(where))
    ret[length(ret) + 1] <- paste0(indt, "where: ", paste(as.character(where), collapse = "\n"))


  if (!is.null(outdata))
    ret[length(ret) + 1] <- paste0(indt, "output dataset ", nrow(outdata),
                                   " rows and ", ncol(outdata), " columns")


  log_logr(ret)

}


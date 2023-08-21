
#' @title Transposes a Dataset
#' @encoding UTF-8
#' @description A function to pivot or transpose a data frame. In the default
#' usage, the variables identified by the parameter \code{var} are transposed
#' to become rows. The variable values in the parameter \code{id} become
#' the new columns. The function has several more parameters to control
#' how variables are named in the transposed data set. Parameters will
#' accept quoted or unquoted values.
#' @details
#' The \code{proc_tranpose} function takes an input data frame or tibble
#' and transposes the columns and rows.  If no parameters are specified,
#' the function will assign all numeric variables to the \code{var} parameter.
#' These variables will become rows, and generic column names
#' ("COL1", "COL2", etc.) will be generated.
#' Other variables will be dropped.
#'
#' There are several parameters to control how new column names are constructed.
#' If the desired column names already exist in your data, identify them on the
#' \code{id} parameter.  The function will then use those data values
#' unaltered.  The label for these new columns can also be constructed from
#' data values using the \code{idlabel} parameter.
#'
#' The \code{name} and \code{namelabel} parameter are used to control the
#' name of the column created for the \code{var} values.  If this parameter
#' is not passed, the column will be called "NAME", and no label will be assigned.
#'
#' You may group the transposed values using the \code{by} parameter. This
#' parameter accepts one or more variable names to use for grouping.  If this
#' parameter is used, the function will first subset the data by the unique
#' combination of \code{by} variables, transpose each subset, and then
#' combine the result into a single output dataset.  The by group variables
#' will be named on the output dataset with a generic name ("BY1", "BY2", etc.).
#'
#' The \code{copy} parameter is used to simply copy columns from the input
#' dataset to the transposed dataset.  If necessary, these values will be
#' recycled or truncated to fit the number of output rows.  Any input
#' variables not included in the \code{var}, \code{id}, or \code{copy} parameter
#' will be dropped.
#'
#' Once the transpose is complete, you may wish to filter the output data.
#' Filtering can be accomplished using the \code{where} parameter.  This parameter
#' takes an expression using the \code{expression} function.  The expression
#' is constructed using standard R logical operators.  Variable names do
#' not need to be quoted.
#'
#' The \code{prefix}, \code{delimiter}, and \code{suffix} parameter are used
#' to control how generic column names are constructed.  These parameters
#' are especially useful when there are multiple \code{var} variables.
#'
#' @param data The input data to transpose.
#' @param by An optional by group.  Parameter accepts a vector of one or more
#' quoted variable names. If the by group is requested, the data will be subset
#' by that variable and the transpose function will
#' transpose each group and stack them together in a single table.
#' @param var The variable or variables to transpose.  Parameter accepts a vector
#' of variable names.  By default, all numeric variables will be transposed.
#' @param id The variable or variables to use for the transposed column names.
#' @param idlabel The variable to use for the transposed column labels.
#' @param copy A vector of variables to retain in the output data
#' without transposition.  Values will be truncated or recycled to fit
#' the number of output rows.
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
#' @return The transposed dataset. If a data frame
#' is input, a data frame will be output.  If a tibble is input, a tibble
#' will be output.
#' @examples
#' # Prepare data
#' dat <- data.frame(CAT = rownames(USPersonalExpenditure),
#'                   USPersonalExpenditure, stringsAsFactors = FALSE,
#'                   row.names = NULL)[1:4, ]
#'
#' # View data
#' dat
#' #                   CAT X1940 X1945 X1950 X1955 X1960
#' # 1    Food and Tobacco 22.20 44.50 59.60  73.2  86.8
#' # 2 Household Operation 10.50 15.50 29.00  36.5  46.2
#' # 3  Medical and Health  3.53  5.76  9.71  14.0  21.1
#' # 4       Personal Care  1.04  1.98  2.45   3.4   5.4
#'
#' # Default transpose
#' tdat1 <- proc_transpose(dat)
#'
#' # View results
#' tdat1
#' #    NAME COL1 COL2  COL3 COL4
#' # 1 X1940 22.2 10.5  3.53 1.04
#' # 2 X1945 44.5 15.5  5.76 1.98
#' # 3 X1950 59.6 29.0  9.71 2.45
#' # 4 X1955 73.2 36.5 14.00 3.40
#' # 5 X1960 86.8 46.2 21.10 5.40
#'
#' # Transpose with ID and Name
#' tdat2 <-  proc_transpose(dat, id = CAT, name = Year)
#'
#' # View results
#' tdat2
#' #   Year Food and Tobacco Household Operation Medical and Health Personal Care
#' # 1 X1940             22.2                10.5               3.53          1.04
#' # 2 X1945             44.5                15.5               5.76          1.98
#' # 3 X1950             59.6                29.0               9.71          2.45
#' # 4 X1955             73.2                36.5              14.00          3.40
#' # 5 X1960             86.8                46.2              21.10          5.40
#'
#' # Transpose only some of the variables
#' tdat3 <- proc_transpose(dat, var = v(X1940, X1950, X1960), id = CAT, name = Year)
#'
#' # View results
#' tdat3
#' #    Year Food and Tobacco Household Operation Medical and Health Personal Care
#' # 1 X1940             22.2                10.5               3.53          1.04
#' # 2 X1950             59.6                29.0               9.71          2.45
#' # 3 X1960             86.8                46.2              21.10          5.40
#'
#' # By with a where clause
#' tdat4 <- proc_transpose(dat, by = CAT, name = Year,
#'                         where = expression(Year %in% c("X1940", "X1950", "X1960")))
#'
#' # View Results
#' tdat4
#' #                    CAT  Year  COL1
#' # 1     Food and Tobacco X1940 22.20
#' # 2     Food and Tobacco X1950 59.60
#' # 3     Food and Tobacco X1960 86.80
#' # 4  Household Operation X1940 10.50
#' # 5  Household Operation X1950 29.00
#' # 6  Household Operation X1960 46.20
#' # 7   Medical and Health X1940  3.53
#' # 8   Medical and Health X1950  9.71
#' # 9   Medical and Health X1960 21.10
#' # 10       Personal Care X1940  1.04
#' # 11       Personal Care X1950  2.45
#' # 12       Personal Care X1960  5.40
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

  oprefix <- deparse(substitute(prefix, env = environment()))
  prefix <- tryCatch({if (typeof(prefix) %in% c("character", "NULL")) prefix else oprefix},
                        error = function(cond) {oprefix})

  osuffix <- deparse(substitute(suffix, env = environment()))
  suffix <- tryCatch({if (typeof(suffix) %in% c("character", "NULL")) suffix else osuffix},
                     error = function(cond) {osuffix})

  odelimiter <- deparse(substitute(delimiter, env = environment()))
  delimiter <- tryCatch({if (typeof(delimiter) %in% c("character", "NULL")) delimiter else odelimiter},
                     error = function(cond) {odelimiter})

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

        if (all(class(data[[by[grp]]]) == "factor")) {
          bygrps[[by[[grp]]]] <- factor(bylbls[[j]][[grp]],
                                        levels = levels(data[[by[grp]]]))
        } else {
          if (typeof(data[[by[grp]]]) == "integer")
            bygrps[[by[[grp]]]] <- as.integer(bylbls[[j]][[grp]])
          else if (typeof(data[[by[grp]]]) == "double") {
            if (all(class(data[[by[grp]]]) == "Date"))
              bygrps[[by[[grp]]]] <- as.Date(bylbls[[j]][[grp]])
            else
              bygrps[[by[[grp]]]] <- as.double(bylbls[[j]][[grp]])
          } else if (typeof(data[[by[grp]]]) == "logical")
            bygrps[[by[[grp]]]] <- as.logical(bylbls[[j]][[grp]])
          else
            bygrps[[by[[grp]]]] <- bylbls[[j]][[grp]]
        }
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

       if (is.null(prefix))
         prefix <- "COL"

       if (is.null(suffix))
         suffix <- ""

       if (nrow(dt) > 0) {
         nms_new <- paste0(prefix, seq(1, nrow(dt)), suffix)
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



     retlst[[length(retlst) + 1]] <- ret
   }


   # Always at least one result
   res <- retlst[[1]]

   # Append by groups if necessary
   if (!is.null(by)) {
    bnms <- names(res)

     if (length(retlst) > 1) {
        for (p in seq(2, length(retlst))) {
          #res <- rbind(res, retlst[[p]])   # Old way

          # Get names of widest by group
          if (ncol(retlst[[p]]) > length(bnms))
            bnms <- names(retlst[[p]])

          res <- merge(res, retlst[[p]], all = TRUE, sort = FALSE)
        }
     }

    # Use widest by group as final column order
    if (ncol(res) == length(bnms)) {
      res <- tryCatch({res[ , bnms]}, error = function(cond){res})
    }
   }

   # Where
   if (!is.null(where)) {
     res <- subset(res, eval(where))

   }

   # Assign name label
   if (!is.null(namelabel)) {

     attr(res[[name]], "label") <-  namelabel
   }

   # Assign ID labels
   if (!is.null(idlabel)) {

     lbls <- dt[[idlabel]]
     names(lbls) <- nms_new
     labels(res) <- lbls

   }

   rownames(res) <- NULL

   # Restore original class
   class(res) <- class(data)

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

     if (log_output()) {
       log_logr(res)
       return(res)
     }
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


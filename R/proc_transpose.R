
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
                           suffix = NULL
                           ) {

  if (!"data.frame" %in% class(data)) {

   stop("Input data must be inherited from a data frame.")
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

   if (length(nms) == 0) {
     stop("No variables to transpose.  You may need to specify the var parameter")

   }

   # Prepare list of labels for by groups
   bylbls <- list()
   if (!is.null(by)) {

     lst <- unclass(data)[by]
     for (nm in names(lst))
       lst[[nm]] <- as.factor(lst[[nm]])
     dtlst <- split(data, lst, sep = "|")

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

       nms_new <- paste0(prefix, dt[[id]], suffix)

       names(ret) <- c(by, copy, name,  nms_new)

     } else {

       if (nrow(dt) > 0) {
         nms_new <- paste0("COL", seq(1, nrow(dt)))
         names(ret) <- c(by, copy, name,  nms_new)
       } else {
         names(ret) <- name
       }

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

 return(res)

}

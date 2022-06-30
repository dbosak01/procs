
#' @title Transposes a Data Frame
#' @encoding UTF-8
#' @description Here is a sample function.
#' @details
#' Some details about the sample function.
#' @param data The input data frame for which calculate summary statistics.
# @param by An optional by group.
#' @param var The variable or variables to transpose.
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
#' @return A data frames that contains the transposed data.
# @import stats
#' @import tibble
#' @import fmtr
#' @export
proc_transpose <- function(data,
                       #   by = NULL,
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

   ret <- NULL

   #browser()

   if (!is.null(var)) {
    nms <- var

   } else {
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

   #browser()
   # Select desired column names
   tpd <- data[, nms]

   # Create ID column
   ret1 <- data.frame(name = nms, stringsAsFactors = FALSE)

   if (!is.null(name))
     names(ret1) <- name

   # Transpose data
   ret2 <- as.data.frame(t(tpd), stringsAsFactors = FALSE)

   # Combine ID column and transposed columns
   ret <- cbind(ret1, ret2)

   rownames(ret) <- NULL

   # Convert to tibble
   if ("tbl_df" %in% class(data)) {
     ret <- as_tibble(ret)

   }



   nms_new <- c()

   # Assign column names
   if (!is.null(id)) {

     nms_new <- paste0(prefix, data[[id]], suffix)

     names(ret) <- c(name,  nms_new)

   } else {

     nms_new <- paste0("COL", seq(1, nrow(data)))
     names(ret) <- c(name,  nms_new)

   }

   # Assign name label
   if (!is.null(namelabel)) {

     attr(ret[[name]], "label") <-  namelabel
   }

   # Assign ID labels
   if (!is.null(idlabel)) {

     lbls <- data[[idlabel]]
     names(lbls) <- nms_new
     labels(ret) <- lbls

   }

 return(ret)

}

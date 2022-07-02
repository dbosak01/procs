
#' @title Transposes a Data Frame
#' @encoding UTF-8
#' @description A function to pivot or transpose a data frame. In the default
#' usage, the variables identified by the parameter \code{var} are transposed
#' to become rows. The variable values in the parameter \code{id} become
#' the new column names.  The function has several more parameters to control
#' how variables are named in the transposed data set.
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
#' @return A data frames that contains the transposed data. If a data frame
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

  if (!"data.frame" %in% class(data)) {

   stop("Input data must be inherited from a data frame.")
  }

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

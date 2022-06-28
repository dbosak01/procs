
# Proc Freq ---------------------------------------------------------------



#' @title Generates Frequency Statistics
#' @encoding UTF-8
#' @description The \code{proc_freq} function generates frequency statistics.
#' The function can perform one and two-way frequencies.  Two-way
#' frequencies are produced as a crosstabulation by default.  There
#' are many options to control the generated tables.  The function will return
#' requested tables in a named list. Additionally, it can output results
#' directly to a report, or show them in the viewer.
#' @details
#' The \code{proc_freq} function generates frequency statistics
#' for one-way and two-way tables.  Data is passed in on the \code{data}
#' parameter.  The desired frequencies are specified on the \code{tables}
#' parameter. Function results are returned as a named list of data
#' frames. Results may also be returned as a report.
#'
#' @section Data Frame Output:
#' By default, frequency results are returned as data frames
#' in a named list. Data in these data frames is not rounded or formatted
#' to give you the most accurate statistical results.
#' The data frame items are named according to the
#' strings specified on the \code{tables} parameter. You may control
#' the names of the returned results by using a named vector on the
#' \code{tables} parameter.
#'
#' If you are requesting only a single table, and
#' do not want it returned in a list, set the \code{piped} parameter to TRUE.
#' Setting \code{piped} to TRUE will cause the function to return results
#' as an individual data frame. This option is useful if you are using
#' the \code{proc_freq} function inside a pipeline.
#'
#' @section Report Output:
#' If the \code{view} option is TRUE, \code{proc_freq} results will
#' be immediately sent to the viewer as an HTML report.  This functionality
#' makes it easy to get a quick analysis of your data with very little
#' effort.
#'
#' The function also allows you to easily export this report to the file system
#' by setting the \code{report_type} and \code{report_location} parameters.
#' Setting these parameters will cause the function to output your report
#' in the location specified, and in the desired output type.
#' You may output the report in TXT, HTML, PDF, RTF, or DOCX file formats.
#'
#' The \code{titles} parameter allows you to set one or more titles for your
#' report.  Pass these titles as a vector of strings.
#'
#' If the frequency variables have a label assigned, that label
#' will be used in the report output. This feature gives you some control
#' over the column headers in the final report.
#'
#' @section Frequency Weight:
#' Normally the \code{proc_freq} function counts each row in the
#' input data equally. In some cases, however, each row in the data
#' can represent multiple observations, and rows should not be treated
#' equally.  In these cases, use the \code{weight} parameter.  The parameter
#' accepts a quoted variable/column name to use as the weighted value.  If the
#' \code{weight} parameter is used, the function will sum the weighted values
#' instead of counting rows.
#'
#' @section Table Options:
#' The \code{table_options} parameter accepts a named list of options and
#' their values.  For example, the "sparse" option may be turned off
#' like this: \code{table_options = list(sparse = FALSE)}.
#'
#' Below are all the available table options and a description of each:
#' \itemize{
#' \item{\strong{colpct}: This option controls whether or not to include
#' column percents on crosstab tables. Value values are TRUE and FALSE.
#' The default is TRUE.
#' }
#' \item{\strong{cumsum}: Whether to include a cumulative sum on one-way
#' tables.  Valid values are TRUE and FALSE.  The default is TRUE.
#' }
#' \item{\strong{cumpct}: Whether to include a cumulative precent on
#' one-way tables.  Valid values are TRUE and FALSE.  The default is TRUE.
#' }
#' \item{\strong{freq}: Whether to include the frequency column on one-way
#' tables. Valid values are TRUE and FALSE.  The default is TRUE.
#' }
#' \item{\strong{format}: A format to use for crosstab percentage values.
#' The format
#' must be a valid format from the \strong{\link[fmtr]{fmtr}} package.  Typically,
#' the format is passed as a formatting string.
#' For example, the formatting string "%.1f%%" indicates that the
#' data should be rounded to a single decimal place and include a percent sign.
#' }
#' \item{\strong{out}: The "out" option tells the procedure to output
#' a frequency table, and allows you to name it.  This name will be used
#' as the list item name.  If more than one table is requested, the "out"
#' option will apply to the last table requested.
#' }
#' \item{\strong{outcum}: Whether to include the cumulative frequency and
#' cumulative percent on the output table specified on the "out" parameter.
#' Valid values are TRUE and FALSE.  The default is TRUE.
#' }
#' \item{\strong{pct}: Whether to include the percent column on one-way
#' tables. Valid values are TRUE and FALSE.  The default is TRUE.
#' }
#' \item{\strong{rowpct}: Whether to include the row percent values on two-way
#' crosstab tables. Valid values are TRUE and FALSE.  The default is TRUE.
#' }
#' \item{\strong{sparse}: Whether to include categories for which there are no
#' frequency counts.  If the parameter is TRUE, sparse categories will be included.
#' If the parameter is FALSE, sparse categories will be removed.
#' The default is TRUE.
#' }
#' \item{\strong{totcol}: Whether to include the total column on two-way
#' crosstab tables. Valid values are TRUE and FALSE.  The default is TRUE.
#' }
#' \item{\strong{totrow}: Whether to include the total rows on two-way
#' crosstab tables. Valid values are TRUE and FALSE.  The default is TRUE.
#' }
#' }
#' @param data The input data frame to perform frequency calculations on.
#' Input data as the first parameter makes this function pipe-friendly.
# @param by An optional by group.
#' @param tables The variable or variables to perform frequency counts on.
#' The table specifications are passed as a vector of quoted strings. If the
#' strings are named, the name will be used as the list item name on the return
#' list of tables.  For one-way frequencies, simply pass the variable name.
#' For two-way tables, pass the desired combination of variables separated by a
#' star (*) operator.
#' @param table_options The option specifications for the \code{table} parameter.
#' Table options are passed to the parameter as a named list.  The name
#' of each list item should correspond to the option you wish to set.  The
#' value of the list item should be the value you wish to set the option
#' to.  The following options are available: "colpct", "cumsum", "cumpct",
#' "freq", "format", "out", "outcum", "pct", "rowpct", "sparse", "totcol"
#' and "totrow". See
#' the \strong{Table Options} section for a description of these options.
#' @param weight An optional weight parameter.  This parameter is passed
#' as a quoted variable name to use for the weight.  If a weight variable is
#' indicated, the weighted value will be summed to calculate the frequency
#' counts.
# @param weight_options The option specifications for the weight parameter.
# @param output The directory path or libname to output data generated by
# the \code{proc_freq} function.
#' @param view Whether to display procedure results in the viewer.  Valid values
#' are TRUE and FALSE.  Default is TRUE.
#' @param report_type The output type for any report output.  Valid values are
#' 'HTML', 'TXT', 'PDF', 'RTF', and 'DOCX'.  Multiple outputs can be
#' requested by passing a vector of output types.  If the \code{report_location}
#' parameter is specified, the outputs will be written to that location.
#' Otherwise, they will be written to a temp directory.  The default value is NULL.
#' @param report_location A path to write any report output requested by
#' the \code{report_type} parameter. The path may be either a full path with
#' file name, or a directory name.  If a directory name, the function
#' will create a default file name based on the report type.
#' If no path is specified, files will be
#' written to a temp directory.  Default is NULL.
#' @param report_style A theme name or style object to use on the report output.
#' See the \code{\link[reporter]{create_style}} in the \strong{reporter}
#' package for additional information on styles.
#' @param titles A vector of one or more titles to use for the report output.
#' @param piped Whether or not the \code{proc_freq} function is part of a data
#' pipeline.  Set this parameter to TRUE if you want the function to return
#' a single dataset instead of a list of datasets.  If there is more than one
#' table requested, the function will return the last requested table.
#' @return By default the function returns a list of data frames
#' that contains the requested frequency tables.
#' The tables are named according to the variable or variables
#' that were requested, and the output tables are in the same order as requested
#' in the \code{tables} parameter. If the
#' \code{proc_freq} function is being used in a data pipeline, you may wish to return
#' the results as a single dataset instead of a list.  To get a single dataset,
#' set the \code{piped} parameter to TRUE. This option will return only the last
#' table requested.
#' @seealso For summary statistics, see \code{\link{proc_means}}.  To pivot
#' or transpose the data coming from \code{proc_freq},
#' see \code{\link{proc_transpose}}.
#' @examples
#' library(procs)
#' library(fmtr)
#'
#' # Get temp directory for output
#' tmp <- tempdir()
#'
#' # Create sample data
#' df <- as.data.frame(HairEyeColor, stringsAsFactors = FALSE)
#'
#' # Assign labels
#' labels(df) <- list(Hair = "Hair Color",
#'                    Eye = "Eye Color",
#'                    Sex = "Sex at Birth")
#'
#' # Example #1: One way frequencies on Hair and Eye color with weight option.
#' res <- proc_freq(df, tables = c("Hair", "Eye"), weight = "Freq")
#'
#' # View result data
#' res
#' #$Hair
#' #  Category Frequency  Percent Cum_Freq   Cum_Pct
#' #1    Black       108 18.24324      108  18.24324
#' #2    Brown       286 48.31081      394  66.55405
#' #3      Red        71 11.99324      465  78.54730
#' #4    Blond       127 21.45270      592 100.00000
#' #
#' #$Eye
#' #  Category Frequency  Percent Cum_Freq   Cum_Pct
#' #1    Brown       220 37.16216      220  37.16216
#' #2     Blue       215 36.31757      435  73.47973
#' #3    Hazel        93 15.70946      528  89.18919
#' #4    Green        64 10.81081      592 100.00000
#'
#' # Example #2: Crosstab table
#' res <- proc_freq(df, tables = "Hair * Eye", weight = "Freq")
#'
#' # View result data
#' res
#' #$`Hair * Eye`
#' #   Category Statistic       Blue      Brown      Green     Hazel     Total
#' #1     Black Frequency  20.000000  68.000000  5.0000000 15.000000 108.00000
#' #2     Black   Percent   3.378378  11.486486  0.8445946  2.533784  18.24324
#' #3     Black   Row Pct  18.518519  62.962963  4.6296296 13.888889        NA
#' #4     Black   Col Pct   9.302326  30.909091  7.8125000 16.129032        NA
#' #5     Blond Frequency  94.000000   7.000000 16.0000000 10.000000 127.00000
#' #6     Blond   Percent  15.878378   1.182432  2.7027027  1.689189  21.45270
#' #7     Blond   Row Pct  74.015748   5.511811 12.5984252  7.874016        NA
#' #8     Blond   Col Pct  43.720930   3.181818 25.0000000 10.752688        NA
#' #9     Brown Frequency  84.000000 119.000000 29.0000000 54.000000 286.00000
#' #10    Brown   Percent  14.189189  20.101351  4.8986486  9.121622  48.31081
#' #11    Brown   Row Pct  29.370629  41.608392 10.1398601 18.881119        NA
#' #12    Brown   Col Pct  39.069767  54.090909 45.3125000 58.064516        NA
#' #13      Red Frequency  17.000000  26.000000 14.0000000 14.000000  71.00000
#' #14      Red   Percent   2.871622   4.391892  2.3648649  2.364865  11.99324
#' #15      Red   Row Pct  23.943662  36.619718 19.7183099 19.718310        NA
#' #16      Red   Col Pct   7.906977  11.818182 21.8750000 15.053763        NA
#' #17    Total Frequency 215.000000 220.000000 64.0000000 93.000000 592.00000
#' #18    Total   Percent  36.317568  37.162162 10.8108108 15.709459 100.00000
#'
#' #' # Example #3: Crosstab table with totrow, totcol, rowpct, and colpct turned off
#' res <- proc_freq(df, tables = c(HairByEyes = "Hair * Eye"),
#'                  table_options = list(totrow = FALSE,
#'                                       totcol = FALSE,
#'                                       rowpct = FALSE,
#'                                       colpct = FALSE),
#'                  weight = "Freq")
#'
#' # View result data
#' res
#' #$HairByEyes
#' #  Category Statistic      Blue      Brown      Green     Hazel
#' #1    Black Frequency 20.000000  68.000000  5.0000000 15.000000
#' #2    Black   Percent  3.378378  11.486486  0.8445946  2.533784
#' #3    Blond Frequency 94.000000   7.000000 16.0000000 10.000000
#' #4    Blond   Percent 15.878378   1.182432  2.7027027  1.689189
#' #5    Brown Frequency 84.000000 119.000000 29.0000000 54.000000
#' #6    Brown   Percent 14.189189  20.101351  4.8986486  9.121622
#' #7      Red Frequency 17.000000  26.000000 14.0000000 14.000000
#' #8      Red   Percent  2.871622   4.391892  2.3648649  2.364865
#' @import fmtr
#' @export
proc_freq <- function(data,
                   #   by = NULL,
                      tables = NULL,
                      table_options = NULL,
                      weight = NULL,
                   #   weight_options = NULL,
                      view = TRUE,
                   #   output = NULL,
                      report_type = NULL,
                      report_location = NULL,
                      report_style = NULL,
                      titles = NULL,
                      piped = FALSE) {

  res <- list()
  # print("Orig print_location")
  # print(print_location)
  #browser()

  # Loop through table requests
  for (i in seq_len(length(tables))) {

    nm <- names(tables)[i]
    tb <- tables[i]
    #browser()
    out <- i == length(tables) & has_option(table_options, "out")

    crstab <- NULL

    # Split cross variables
    splt <- trimws(strsplit(tb, "*", fixed = TRUE)[[1]])

    # Perform either one-way or two-way frequency count
    if (length(splt) == 1) {

      result <- freq_oneway(data, tb, weight, table_options, out)

    } else if (length(splt) == 2) {

      result <- freq_twoway(data, splt[1], splt[2], weight, table_options, out)

      crstab <- cross_tab(result, table_options, splt[1], splt[2])

    } else {

      stop("Procedure does not yet support n-way frequencies.")
    }

    # If a cross tab was produced, add it to result
    if (!is.null(crstab)) {

      if (is.null(nm))
        res[[tb]] <- crstab
      else if (nchar(nm) == 0)
        res[[tb]] <- crstab
      else
        res[[nm]] <- crstab

      if ("out" %in% names(table_options) & i == length(tables)) {

        res[[table_options[["out"]]]] <- result
      }

    } else { # Otherwise add one-way to result

      if (is.null(nm))
        res[[tb]] <- result
      else if (nchar(nm) == 0)
        res[[tb]] <- result
      else
        res[[nm]] <- result

    }
  }

  # Create output reports if requested
  if (!is.null(report_type)) {

    loc <- get_location("freq", report_location)
    out <- output_report(res, proc_type = 'freq', dir_name = loc["dir_name"],
                         file_name = loc["file_name"], out_type = report_type,
                         style = report_style,
                         titles = titles, margins = 1)


  }

  # Create viewer report if requested
  if (view == TRUE) {


    vrfl <- tempfile()

    out <- output_report(res, proc_type = 'freq', dir_name = dirname(vrfl),
                         file_name = basename(vrfl), out_type = "HTML",
                         style = report_style,
                         titles = titles, margins = .5, viewer = TRUE)

    show_viewer(out)
  }

  if (piped == TRUE) {

    res <- res[[length(res)]]
  }

  return(res)

}


# Sub Procedures ----------------------------------------------------------



#' @import fmtr
#' @import stats
#' @noRd
freq_oneway <- function(data, tb, weight, options, out = FALSE) {

  # Get target variable vector
  var <- data[[tb]]

  # Get frequency counts
  if (is.null(weight)) {

    categories <- names(sort(table(var)))
    frequencies <- as.vector(sort(table(var)))

  } else {

    cnts <- aggregate(data[[weight]], list(data[[tb]]), FUN = sum)
    categories <- cnts$Group.1
    frequencies <- cnts$x
  }

  # Perform calculations
  n <- sum(frequencies)
  percentages <- frequencies / n * 100
  cum_frequencies <- cumsum(frequencies)
  cum_percentages <- cumsum(percentages)


  # Create result data frame
  result <- data.frame("Category" = categories,
                       "Frequency" = frequencies,
                       "Percent" = percentages,
                       "Cum_Freq" = cum_frequencies,
                       "Cum_Pct" = cum_percentages,
                       stringsAsFactors = FALSE)



  # Get any existing label for target variable
  lbl <- attr(data[[tb]], "label")

  if (is.null(lbl))
    lbl <- tb

  # Clear out any names
  names(tb) <- NULL

  # Apply default labels
  labels(result) <- c(Category = tb,
                      Cum_Freq = "Cumulative Frequency",
                      Cum_Pct = "Cumulative Percent")

  # Apply default formats
  formats(result) <- list(Cum_Pct = "%.2f",
                          Percent = "%.2f")

 # browser()

  # Kill freq if requested
  if ((!option_true(options, "freq", TRUE))) {

    result[["Frequency"]] <- NULL
  }

  # Kill pct if requested
  if ((!option_true(options, "pct", TRUE))) {

    result[["Percent"]] <- NULL
  }

  # Kill cum freq if requested
  if ((out == FALSE & !option_true(options, "cumsum", TRUE)) |
      (out == TRUE & !option_true(options, "outcum", TRUE))) {

    result[["Cum_Freq"]] <- NULL
  }

  # Kill cum pct if requested
  if ((out == FALSE & !option_true(options, "cumpct", TRUE)) |
      (out == TRUE & !option_true(options, "outcum", TRUE))) {

    result[["Cum_Pct"]] <- NULL
  }


  # Add spanning headers
  spn <- span_spec(label = lbl, 1, ncol(result), 1)
  attr(result, "spans") <- list(spn)


  return(result)
}


#' @import fmtr
#' @import stats
#' @noRd
freq_twoway <- function(data, tb1, tb2, weight, options, out = FALSE) {

  # Assign 1 to count column
  data[["__cnt"]] <- 1

  # Get target variables into vectors
  v1 <- data[[tb1]]
  v2 <- data[[tb2]]

  # Get unique values of variables
  t1 <- names(sort(table(v1)))
  t2 <- names(sort(table(v2)))

  # Get unique combinations of variable values for zero-fill
  ex <- expand.grid(tb1 = t1, tb2 = t2, stringsAsFactors = FALSE)

  # Assign zero fill value
  ex[["__cnt"]] <- 0

  # Use weight variable if requested
  if (is.null(weight)) {

    c1 <- data[["__cnt"]]

  } else {

    c1 <-data[[weight]]

  }

  # Append zero fills
  if (option_true(options, "sparse", TRUE)) {
    c1 <- append(c1, ex[["__cnt"]])
    v1 <- append(v1, ex[["tb1"]])
    v2 <- append(v2, ex[["tb2"]])
  }

  # Get frequencies
  cnts <- aggregate(c1, list(v1, v2), FUN = sum)
  categories1 <- cnts$Group.1
  categories2 <- cnts$Group.2
  frequencies <- cnts$x

  # Perform calculations
  n <- sum(frequencies)
  percentages <- frequencies / n * 100


  # Create result data frame
  result <- data.frame("Category1" = categories1,
                       "Category2" = categories2,
                       "Frequency" = frequencies,
                       "Percent" = percentages,
                       stringsAsFactors = FALSE)

  # Sort result data frame
  result <- result[order(result$Category1, result$Category2), ]

  # Kill rownames
  rownames(result) <- NULL

  # Get labels on target variables if they exist
  lbl1 <- attr(data[[tb1]], "label")
  lbl2 <- attr(data[[tb2]], "label")

  if (is.null(lbl1))
    lbl1 <- tb1
  if (is.null(lbl2))
    lbl2 <- tb2

  # Assign labels
  labels(result) <- c(Category1 = lbl1,
                      Category2 = lbl2)

  # Assign default formats
  formats(result) <- list(Percent = paste0("%.4f"))

  # Kill freq if requested
  if ((!option_true(options, "freq", TRUE))) {

    result[["Frequency"]] <- NULL
  }

  # Kill pct if requested
  if ((!option_true(options, "pct", TRUE))) {

    result[["Percent"]] <- NULL
  }

  # Kill cum freq if requested
  if ((out == FALSE & !option_true(options, "cumsum", TRUE)) |
      (out == TRUE & !option_true(options, "outcum", TRUE))) {

    result[["Cum_Freq"]] <- NULL
  }

  # Kill cum pct if requested
  if ((out == FALSE & !option_true(options, "cumpct", TRUE)) |
      (out == TRUE & !option_true(options, "outcum", TRUE))) {

    result[["Cum_Pct"]] <- NULL
  }


  return(result)

}

#' @import fmtr
#' @import stats
#' @noRd
cross_tab <- function(freqdata, options, var1, var2) {

  lbl1 <- attr(freqdata$Category1, "label")
  lbl2 <- attr(freqdata$Category2, "label")

  #browser()

  # Group by both dimensions
  cat1grp <- aggregate(freqdata$Frequency, list(freqdata$Category1), FUN=sum)
  cat2grp <- aggregate(freqdata$Frequency, list(freqdata$Category2), FUN=sum)

  # Create lookup from cat1 group (rows)
  lkp1 <- cat1grp$x
  names(lkp1) <- cat1grp$Group.1

  # Create lookup from cat2 group (columns)
  lkp2 <- cat2grp$x
  names(lkp2) <- cat2grp$Group.1

  # Assign data to new variable
  dt <- freqdata

  # Create freq columns for both dimensions
  dt$rowcnt <- lkp1[dt$Category1]
  dt$colcnt <- lkp2[dt$Category2]

  # Create percentages for both dimensions
  dt$Percentage <- dt$Percentage
  dt$rowpct <- dt$Frequency / dt$rowcnt * 100
  dt$colpct <- dt$Frequency / dt$colcnt * 100

  # Transpose Frequency statistics
  dt1 <- reshape(dt, timevar = "Category2", idvar = "Category1",
                 v.names = "Frequency", direction = "wide",
                 drop = c("Percent", "rowcnt", "colcnt", "rowpct", "colpct"))
  dt1$Total <- lkp1[dt1$Category1]
  dt1$Order <- 1
  dt1$Statistic <- "Frequency"
  names(dt1) <- gsub("Frequency.",  "", names(dt1), fixed = TRUE)

  # Transpose Percents
  dt2 <- reshape(dt, timevar = "Category2", idvar = "Category1",
                 v.names = "Percent", direction = "wide",
                 drop = c("Frequency", "rowcnt", "colcnt", "rowpct", "colpct"))
  dt2$Total <- lkp1[dt1$Category1] / sum(lkp1, na.rm = TRUE) * 100
  dt2$Order <- 2
  dt2$Statistic <- "Percent"
  names(dt2) <- gsub("Percent.",  "", names(dt2), fixed = TRUE)


  # Transpose Row Percents
  dt3 <- NULL
  if (get_option(options, "rowpct", TRUE) == TRUE) {
    dt3 <- reshape(dt, timevar = "Category2", idvar = "Category1",
                   v.names = "rowpct", direction = "wide",
                   drop = c("Percent", "rowcnt", "colcnt", "Frequency", "colpct"))
    dt3$Total <- NA
    dt3$Order <- 3
    dt3$Statistic <- "Row Pct"
    names(dt3) <- gsub("rowpct.",  "", names(dt3), fixed = TRUE)
  }

  # Transpose Col Percents
  dt4 <- NULL
  if (get_option(options, "colpct", TRUE) == TRUE) {
    dt4 <- reshape(dt, timevar = "Category2", idvar = "Category1",
                   v.names = "colpct", direction = "wide",
                   drop = c("Percent", "rowcnt", "colcnt", "Frequency", "rowpct"))
    dt4$Total <- NA
    dt4$Order <- 4
    dt4$Statistic <- "Col Pct"
    names(dt4) <- gsub("colpct.",  "", names(dt4), fixed = TRUE)
  }

  #browser()
  dt5 <- NULL
  dt6 <- NULL

  # Add row total if requested
  if (option_true(options, "totrow", TRUE)) {
    dt5 <- data.frame(Category1 = "Total")
    for (nm in names(lkp2)) {
      dt5[[nm]] <- lkp2[[nm]]
    }
    dt5$Total = sum(lkp2, na.rm = TRUE)
    dt5$Order = 1
    dt5$Statistic = "Frequency"


    dt6 <- data.frame(Category1 = "Total")
    for (nm in names(lkp2)) {
      dt6[[nm]] <- lkp2[[nm]] / sum(lkp2, na.rm = TRUE) * 100
    }
    dt6$Total = 100
    dt6$Order = 2
    dt6$Statistic = "Percent"
  }


  # Combine everthing
  ret <- rbind(dt1, dt2, dt3, dt4, dt5, dt6,
               make.row.names = FALSE,
               stringsAsFactors = FALSE)

  # Get all value column names
  nnms <- names(ret)[!names(ret) %in% c("Category1", "Order", "Statistic")]

  # Sort data frame by category and order
  ret <- ret[order(ret$Category1, ret$Order), c("Category1", "Statistic", nnms) ]

  # Kill rownames
  rownames(ret) <- NULL

  # Rename to Category so output_report() will recognize as a stub
  names(ret)[1] <- "Category"

  # Get format
  fmt <- get_option(options, "format", "%.2f")

  # Create formatting list
  lst <- list(Frequency = "%d", Percent = fmt,
              'Row Pct' = fmt, 'Col Pct' = fmt)
  fl <- as.flist(lst, type = "row", lookup = ret$Statistic)

  # Assign flist to data columns
  fmts <- list()
  for (nm in nnms) {
    fmts[[nm]] <- fl
  }
  formats(ret) <- fmts

  # Kill column total if requested
  if (!option_true(options, "totcol", TRUE)) {

    ret$Total <- NULL
  }

  # Assign label to Category
  attr(ret$Category, "label") <- lbl1

  # Add spanning headers
  lbl <- paste0("Table of ", var1, " by ", var2)
  spn2 <- span_spec(label = lbl, 1, ncol(ret), 2)
  spn1 <- span_spec(label = lbl2, 3, ncol(ret), 1)
  attr(ret, "spans") <- list(spn1, spn2)

  return(ret)
}


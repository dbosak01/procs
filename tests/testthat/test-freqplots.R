base_path <- file.path(getwd(), "/tests/testthat")
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dat <- read.table(header = TRUE, text = '
  Region Eyes Hair Count
  1 blue  fair   23
  1 blue  dark   11
  1 green medium 18
  1 brown red     5
  1 brown black   3
  2 blue  medium 44
  2 green fair   50
  2 green dark   23
  2 brown medium 53
  1 blue  red     7
  1 green fair   19
  1 green dark   14
  1 brown medium 41
  2 blue  fair   46
  2 blue  dark   40
  2 green red    31
  2 brown fair   56
  2 brown dark   54
  1 blue  medium 24
  1 green red     7
  1 brown fair   34
  1 brown dark   40
  2 blue  red    21
  2 blue  black   6
  2 green medium 37
  2 brown red    42
  2 brown black  13
  ')



prt <- read.table(header = TRUE, text = '
  sex internship enrollment count
  1  boys        yes        yes    35
  2  boys         no        yes    14
  3 girls        yes        yes    32
  4 girls         no        yes    53
  5  boys        yes         no    29
  6  boys         no         no    27
  7 girls        yes         no    10
  8 girls         no         no    23')


prt2 <- read.table(header = TRUE, text = '
      sex internship enrollment count  group
  1  boys        yes        yes    35      1
  2  boys         no        yes    14      1
  3 girls        yes        yes    32      1
  4 girls         no        yes    53      1
  5  boys        yes         no    29      2
  6  boys         no         no    27      2
  7 girls        yes         no    10      2
  8 girls         no         no    23      2')

adsl <- read.table(header = TRUE, text = '
  SUBJID  ARM    SEX  RACE    AGE
  "001"   "Placebo" "F"  "WHITE" 19
  "002"   "Drug A (Dose 20mg)" "F"  "WHITE" 21
  "003"   "Drug A (Dose 40mg)" "F"  "WHITE" 23
  "004"   "Competitor" "F"  "BLACK OR AFRICAN AMERICAN" 28
  "005"   "Placebo" "M"  "WHITE" 37
  "006"   "Drug A (Dose 20mg)" "M"  "WHITE" 34
  "007"   "Drug A (Dose 40mg)" "M"  "WHITE" 36
  "008"   "Competitor" "M"  "WHITE" 30
  "009"   "Placebo" "F"  "WHITE" 39
  "010"   "Drug A (Dose 20mg)" "F"  "WHITE" 31
  "011"   "Drug A (Dose 40mg)" "F"  "BLACK OR AFRICAN AMERICAN" 33
  "012"   "Competitor" "F"  "WHITE" 38
  "013"   "Placebo" "M"  "BLACK OR AFRICAN AMERICAN" 37
  "014"   "Drug A (Dose 20mg)" "M"  "WHITE" 34
  "015"   "Drug A (Dose 40mg)" "M"  "WHITE" 36
  "016"   "Placebo" "M"  "WHITE" 40')


options("logr.output" = FALSE)
options("procs.print" = FALSE)

dev <- FALSE

test_that("freqplot1: render_freqplot() works for freqplots.", {

  dt <- proc_freq(dat, "Eyes")

  plt <- freqplot()

  # Plot object
  res <- render_freqplot(dt, "Eyes", plt = plt)

  pth <- res$plot

  # file.show(pth)

  expect_equal(file.exists(pth), TRUE)

  # Character string
  res <- render_freqplot(dt, "Eyes", plt = "freqplot")

  pth <- res$plot

  # file.show(pth)

  expect_equal(file.exists(pth), TRUE)

})

test_that("freqplot2: One-way proc_freq with plots works.", {

  # Freqplot function
  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = freqplot(),
                   output = report,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Freqplot string
  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = "freqplot",
                   output = report,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Freqplot NSE
  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = freqplot,
                   output = report,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


})



# Have to check this visually
test_that("freqplot3: One-way proc_freq with plots order variations work.", {

  if (dev) {

    # Internal order
    res <- proc_freq(dat, tables = c("Eyes"),
                     plots = freqplot(),
                     output = report,
                     order = internal,
                     titles = "My first Frequency Plot")

    res

    expect_equal(is.null(res), FALSE)

    expect_equal("data.frame" %in% class(res[[1]]), TRUE)
    expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

    # Data order
    res <- proc_freq(dat, tables = c("Eyes"),
                     plots = "freqplot",
                     output = report,
                     order = data,
                     titles = "My first Frequency Plot")

    res

    expect_equal(is.null(res), FALSE)

    expect_equal("data.frame" %in% class(res[[1]]), TRUE)
    expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


    # Frequency order
    res <- proc_freq(dat, tables = c("Eyes"),
                     plots = freqplot,
                     output = report,
                     order = freq,
                     titles = "My first Frequency Plot")

    res

    expect_equal(is.null(res), FALSE)

    expect_equal("data.frame" %in% class(res[[1]]), TRUE)
    expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


    # Format order
    library(fmtr)
    pfmt <- value(condition(x == "green", "green"),
                  condition(x == "brown", "brown"),
                  condition(x == "blue", "blue"))
    dat2 <- dat
    formats(dat2) <- list(Eyes = pfmt)


    res <- proc_freq(dat2, tables = c("Eyes"),
                     plots = freqplot(),
                     output = report,
                     order = formatted,
                     titles = "My first Frequency Plot")

    res

    expect_equal(is.null(res), FALSE)

    expect_equal("data.frame" %in% class(res[[1]]), TRUE)
    expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  } else {
    expect_equal(TRUE, TRUE)
  }

})

# Great!
test_that("freqplot4: One-way proc_freq with plots and weight works.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = freqplot(),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

})

# Nice!
test_that("freqplot5: One-way proc_freq with plots horizontal works.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = freqplot(orient = "horizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

})


# Works!
test_that("freqplot6: One-way proc_freq with plots and by works.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = freqplot(),
                   output = report,
                   weight = Count,
                   by = Region,
                   order = freq,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

})



# Works!
test_that("freqplot7: One-way proc_freq with plots dotplot option works.", {

  # Vertical
  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = freqplot(type = "dotplot", orient = "vertical"),
                   output = report,
                   weight = Count,
                   order = freq,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal
  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = freqplot(type = "dotplot", orient = "horizontal"),
                   output = report,
                   weight = Count,
                   order = data,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

})

test_that("freqplot8: One-way proc_freq with plots scale options work.", {

  if (dev) {

    # Percent scale
    res <- proc_freq(dat, tables = "Eyes",
                     plots = freqplot(scale = "percent"),
                     output = report,
                     weight = Count,
                     order = internal,
                     titles = "My first Frequency Plot")

    res

    expect_equal(is.null(res), FALSE)
    expect_equal("data.frame" %in% class(res[[1]]), TRUE)
    expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

    # Log scale
    res <- proc_freq(dat, tables = "Eyes",
                     plots = freqplot(scale = "log"),
                     output = report,
                     weight = Count,
                     order = internal,
                     titles = "My first Frequency Plot")

    res

    expect_equal(is.null(res), FALSE)
    expect_equal("data.frame" %in% class(res[[1]]), TRUE)
    expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

    # Sqrt scale
    res <- proc_freq(dat, tables = "Eyes",
                     plots = freqplot(scale = "sqrt"),
                     output = report,
                     weight = Count,
                     order = internal,
                     titles = "My first Frequency Plot")

    res

    expect_equal(is.null(res), FALSE)
    expect_equal("data.frame" %in% class(res[[1]]), TRUE)
    expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  } else {

    expect_equal(TRUE, TRUE)
  }

})

# Test with:
# - Different data
# - Long labels
# - 0 values
# - NA values
# - Large values
# - Many categories
# - Real clinical data
test_that("freqplot9: One-way proc_freq with plots edge cases.", {

  # Single bar chart
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[2]]), TRUE)


})



test_that("freqplot10: One-way proc_freq with plots more edge cases.", {

  # Single bar chart
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[2]]), TRUE)



})


test_that("freqplot11: Two-way proc_freq with vertical plots and groupvertical works.", {

  # Single bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Two bar charts
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)


  # Single dot plot
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "dotplot"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Two dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)


})



test_that("freqplot12: Two-way proc_freq with horizontal plots and groupvertical works.", {

  # Single bar chart
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(orient = "horizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Two bar charts
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "horizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)


  # Single dot plot
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "dotplot", orient = "horizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Two dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", orient = "horizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)

})

# ** Missing labels ***
test_that("freqplot13: Two-way proc_freq with vertical plots and grouphorizontal works.", {

  # Single bar chart
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(twoway = "grouphorizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Two bar charts
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(twoway = "grouphorizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)


  # Single dot plot
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "dotplot", twoway = "grouphorizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Two dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", twoway = "grouphorizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)


})



test_that("freqplot14: Two-way proc_freq with horizontal plots and grouphorizontal works.", {

  # Single bar chart
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(orient = "horizontal", twoway = "grouphorizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Two bar charts
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "horizontal", twoway = "grouphorizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)


  # Single dot plot
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "dotplot", orient = "horizontal",
                                    twoway = "grouphorizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Two dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", orient = "horizontal",
                                    twoway = "grouphorizontal"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)

})


# Add legend
test_that("freqplot15: Two-way proc_freq with clustered bar chart.", {

  # Vertical bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "vertical", twoway = "cluster"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "horizontal", twoway = "cluster"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Vertical bar chart
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "barchart", orient = "vertical", twoway = "cluster"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal bar chart
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "barchart", orient = "horizontal", twoway = "cluster"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


})

# Add legend
test_that("freqplot16: Two-way proc_freq with stacked bar chart.", {

  # Vertical bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "vertical", twoway = "stacked"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "horizontal", twoway = "stacked"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Vertical bar chart
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "barchart", orient = "vertical",
                                    twoway = "stacked"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal bar chart
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "barchart", orient = "horizontal",
                                    twoway = "stacked"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


})


# Add legend
# Add dot plot
test_that("freqplot17: Two-way proc_freq with stacked dot plot.", {

  # Vertical dot plot
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", orient = "vertical",
                                    twoway = "stacked"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal dot plot
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", orient = "horizontal",
                                    twoway = "stacked"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Vertical dot plot
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "dotplot",
                                    orient = "vertical", twoway = "stacked"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal dot plot
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "dotplot",
                                    orient = "horizontal", twoway = "stacked"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


})


test_that("freqplot18: Two-way proc_freq barchart npanelpos parameter works.", {

  # Freq plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(twoway = "grouphorizontal", npanelpos = 1),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 5)

  # Freq plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(twoway = "grouphorizontal", npanelpos = 2),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 3)


  # Freq plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(twoway = "grouphorizontal", npanelpos = 3),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)

  # Freq plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(twoway = "groupvertical", npanelpos = 1),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 3)

  # Freq plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(twoway = "groupvertical", npanelpos = 2),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)


  # Freq plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(twoway = "groupvertical", npanelpos = 3),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


})

test_that("freqplot19: Two-way proc_freq dotplot npanelpos parameter works.", {

  # Dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", twoway = "grouphorizontal",
                                    npanelpos = 1),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 5)

  # Dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", twoway = "grouphorizontal",
                                    npanelpos = 2),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 3)


  # Dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", twoway = "grouphorizontal",
                                    npanelpos = 3),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)

  # Dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", twoway = "groupvertical",
                                    npanelpos = 1),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 5)

  # Dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", twoway = "groupvertical",
                                    npanelpos = 2),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 3)


  # Dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", twoway = "groupvertical",
                                    npanelpos = 3),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal(length(res[[2]]), 2)


})

#
# test_that("freqplot20: Two-way proc_freq barchart npanelpos parameter works.", {
#
#   # Freq plots
#   res <- proc_freq(dat, tables = "Eyes * Hair",
#                    plots = freqplot(twoway = "grouphorizontal", npanelpos = 1),
#                    output = report,
#                    weight = Count,
#                    titles = "My first Frequency Plot")
#
#   res
#
#   expect_equal("data.frame" %in% class(res[[1]]), TRUE)
#   expect_equal(length(res[[2]]), 5)
#
#   # Freq plots
#   res <- proc_freq(dat, tables = "Eyes * Hair",
#                    plots = freqplot(twoway = "grouphorizontal", npanelpos = 2),
#                    output = report,
#                    weight = Count,
#                    titles = "My first Frequency Plot")
#
#   res
#
#
#   expect_equal("data.frame" %in% class(res[[1]]), TRUE)
#   expect_equal(length(res[[2]]), 3)
#
#
#   # Freq plots
#   res <- proc_freq(dat, tables = "Eyes * Hair",
#                    plots = freqplot(twoway = "grouphorizontal", npanelpos = 3),
#                    output = report,
#                    weight = Count,
#                    titles = "My first Frequency Plot")
#
#   res
#
#
#   expect_equal("data.frame" %in% class(res[[1]]), TRUE)
#   expect_equal(length(res[[2]]), 2)
#
#   # Freq plots
#   res <- proc_freq(dat, tables = "Eyes * Hair",
#                    plots = freqplot(twoway = "groupvertical", npanelpos = 1),
#                    output = report,
#                    weight = Count,
#                    titles = "My first Frequency Plot")
#
#   res
#
#   expect_equal("data.frame" %in% class(res[[1]]), TRUE)
#   expect_equal(length(res[[2]]), 3)
#
#   # Freq plots
#   res <- proc_freq(dat, tables = "Eyes * Hair",
#                    plots = freqplot(twoway = "groupvertical", npanelpos = 2),
#                    output = report,
#                    weight = Count,
#                    titles = "My first Frequency Plot")
#
#   res
#
#
#   expect_equal("data.frame" %in% class(res[[1]]), TRUE)
#   expect_equal(length(res[[2]]), 2)
#
#
#   # Freq plots
#   res <- proc_freq(dat, tables = "Eyes * Hair",
#                    plots = freqplot(twoway = "groupvertical", npanelpos = 3),
#                    output = report,
#                    weight = Count,
#                    titles = "My first Frequency Plot")
#
#   res
#
#
#   expect_equal("data.frame" %in% class(res[[1]]), TRUE)
#   expect_equal("plot_spec" %in% class(res[[2]]), TRUE)
#
#
# })

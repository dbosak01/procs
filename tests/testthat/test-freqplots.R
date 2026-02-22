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

  expect_error(freqplot(c("barchart", "dotplot")))

})

test_that("freqplot2: One-way proc_freq with plots works.", {

  # Freqplot function
  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = freqplot(),
                   output = report)

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
                   plots = freqplot(),
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
                     plots = freqplot(),
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
test_that("freqplot9: One-way edge cases.", {


  # Long bottom label
  res <- proc_freq(adsl, tables = "ARM",
                   plots = freqplot(),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Long left label
  res <- proc_freq(adsl, tables = "RACE",
                   plots = freqplot(orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Dot plot - Long bottom label
  res <- proc_freq(adsl, tables = "ARM",
                   plots = freqplot(type = "dotplot", orient = "vertical"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Dot plot - Long left label
  res <- proc_freq(adsl, tables = "ARM",
                   plots = freqplot(type = "dotplot", orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


})



test_that("freqplot10: One-way more edge cases.", {

  dt <- adsl
  dt$ARM <- ifelse(dt$ARM == "Drug A (Dose 20mg)",
                    "Drug A\n(Dose 20mg)", dt$ARM)
  dt$ARM <- ifelse(dt$ARM == "Drug A (Dose 40mg)",
                    "Drug A\n(Dose 40mg)", dt$ARM)
  dt$RACE <- ifelse(dt$RACE == "BLACK OR AFRICAN AMERICAN",
                     "BLACK\nOR\nAFRICAN\nAMERICAN", dt$RACE)

  # Handles forced line breaks - Vertical
  res <- proc_freq(dt, tables = "RACE",
                   plots = freqplot(),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Handles forced line breaks - Horizontal
  res <- proc_freq(dt, tables = "RACE",
                   plots = freqplot(orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Dotplot handles forced line breaks - Vertical
  res <- proc_freq(dt, tables = "RACE",
                   plots = freqplot(type = "dotplot"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Dotplot handles forced line breaks - Horizontal
  res <- proc_freq(dt, tables = "RACE",
                   plots = freqplot(type = "dotplot", orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


})


test_that("freqplot11: Two-way proc_freq with vertical plots and groupvertical works.", {

  # Single bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(npanelpos = 3),
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


test_that("freqplot20: Two-way proc_freq groupby parameter works.", {

  # Freq plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "vertical", groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)

  # Freq plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "horizontal", groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)



  # Freq plots
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(orient = "vertical", groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Freq plots
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(orient = "horizontal", groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)

  # Dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", orient = "vertical",
                                    groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Dot plots
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", orient = "horizontal",
                                    groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Freq plots
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "dotplot", orient = "vertical",
                                    groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)

  # Freq plots
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(type = "dotplot", orient = "horizontal",
                                    groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)


})

test_that("freqplot21: Two-way proc_freq with nongroup charts groupby row.", {

  # Vertical bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "vertical", twoway = "stacked",
                                    groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "horizontal", twoway = "stacked",
                                    groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Vertical dot plot
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", orient = "vertical",
                                    twoway = "stacked", groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(type = "dotplot", orient = "horizontal",
                                    twoway = "stacked", groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Vertical bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "vertical", twoway = "cluster",
                                    groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Horizontal bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "horizontal", twoway = "cluster",
                                    groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)
})


test_that("freqplot22: Two-way proc_freq with grouppercent scale.", {

  # Vertical bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "vertical", scale = "grouppercent"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Vertical bar chart
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "vertical", scale = "grouppercent",
                                    groupby = "row"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)

  # Vertical dot plot
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "vertical", scale = "grouppercent",
                                    type = "dotplot"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)

  # Vertical dot plot
  res <- proc_freq(dat, tables = "Eyes * Hair",
                   plots = freqplot(orient = "vertical", scale = "grouppercent",
                                    groupby = "row", type = "dotplot"),
                   output = report,
                   weight = Count,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

})

test_that("freqplot23: Two-way with by variable.", {

  # Single bar chart
  res <- proc_freq(dat, tables = "Hair * Eyes",
                   plots = freqplot(orient = "horizontal",
                                    twoway = "grouphorizontal"),
                   output = report,
                   by = Region,
                   weight = Count,
                   titles = "My first Frequency Plot")

  expect_equal(length(res), 4)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)
  expect_equal("data.frame" %in% class(res[[3]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[4]]), TRUE)

})


test_that("freqplot24: Two-way edge cases grouped.", {

  # Long right labels
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(npanelpos = 2),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[2]]), TRUE)

  # Long left labels - horizontal
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Dot plot
  # Long bottom labels -  vertical
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(type = "dotplot", orient = "vertical"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Dot plot
  # Long left labels - horizontal
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(type = "dotplot", orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


})



# Fix all these
test_that("freqplot25: Two-way more edge cases grouped.", {

  dt <- adsl
  dt$ARM <- ifelse(dt$ARM == "Drug A (Dose 20mg)",
                   "Drug A\n(Dose 20mg)", dt$ARM)
  dt$ARM <- ifelse(dt$ARM == "Drug A (Dose 40mg)",
                   "Drug A\n(Dose 40mg)", dt$ARM)
  dt$RACE <- ifelse(dt$RACE == "BLACK OR AFRICAN AMERICAN",
                    "BLACK\nOR\nAFRICAN\nAMERICAN", dt$RACE)

  # Handles forced line breaks - Vertical
  res <- proc_freq(dt, tables = "ARM * RACE",
                   plots = freqplot(),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Handles forced line breaks - Horizontal
  res <- proc_freq(dt, tables = "ARM * RACE",
                   plots = freqplot(orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Dotplot handles forced line breaks - Vertical
  res <- proc_freq(dt, tables = "ARM * RACE",
                   plots = freqplot(type = "dotplot"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Dotplot handles forced line breaks - Horizontal
  res <- proc_freq(dt, tables = "ARM * RACE",
                   plots = freqplot(type = "dotplot", orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Dotplot handles forced line breaks - Horizontal
  res <- proc_freq(dt, tables = "ARM * RACE",
                   plots = freqplot(type = "dotplot", orient = "horizontal",
                                    groupby = "row"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Barchart handles forced line breaks - Horizontal
  res <- proc_freq(dt, tables = "ARM * RACE",
                   plots = freqplot(type = "barchart", orient = "vertical",
                                    groupby = "row"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

})

test_that("freqplot26: Two-way edge cases ungrouped.", {

  # Long bottom labels
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(twoway= "cluster"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)
  # expect_equal("plot_spec" %in% class(res[[2]][[2]]), TRUE)

  # Long left labels - horizontal
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(twoway = "cluster", orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  # Long bottom labels
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(twoway= "stacked"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)
  # expect_equal("plot_spec" %in% class(res[[2]][[2]]), TRUE)

  # Long left labels - horizontal
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(twoway = "stacked", orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Dot plot
  # Long bottom labels -  vertical
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(type = "dotplot", twoway = "stacked", orient = "vertical"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

  # Dot plot
  # Long left labels - horizontal
  res <- proc_freq(adsl, tables = "ARM * RACE",
                   plots = freqplot(type = "dotplot", twoway = "stacked", orient = "horizontal"),
                   output = report,
                   weight = AGE,
                   titles = "My first Frequency Plot")

  res

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


})

test_that("freqplot27: Multiple table requests work as expected", {

  res <- proc_freq(dat, tables = v(Hair, Eyes, Hair * Eyes),
                    weight = Count,
                    output = report,
                    plots = freqplot(),
                    titles = "Hair and Eye Frequency Statistics")

  expect_equal(length(res), 6)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)
  expect_equal("data.frame" %in% class(res[[3]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[4]]), TRUE)
  expect_equal("data.frame" %in% class(res[[5]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[6]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[6]][[2]]), TRUE)

  # Example 2: Frequency statistics with custom plots
  res <- proc_freq(dat, tables = v(Hair, Eyes, Hair * Eyes),
                   weight = Count,
                   output = report,
                   plots = list(freqplot(type = "barchart",
                                         orient = "horizontal"),
                                freqplot(type = "dotplot",
                                         scale = "percent"),
                                freqplot(type = "barchart",
                                         twoway = "cluster")),
                   titles = "Hair and Eye Frequency Statistics")


  expect_equal(length(res), 6)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)
  expect_equal("data.frame" %in% class(res[[3]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[4]]), TRUE)
  expect_equal("data.frame" %in% class(res[[5]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[6]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[6]]), TRUE)


  res <- proc_freq(dat, tables = v(Hair, Eyes, Hair * Eyes),
                   weight = Count,
                   output = report,
                   plots = freqplot(type = "dotplot"),
                   titles = "Hair and Eye Frequency Statistics")

  expect_equal(length(res), 6)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)
  expect_equal("data.frame" %in% class(res[[3]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[4]]), TRUE)
  expect_equal("data.frame" %in% class(res[[5]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[6]]), TRUE)

})

test_that("freqplot28: Two Way only works as expected.", {


  res <- proc_freq(mtcars, tables = c("cyl"),
                   plots = freqplot(),
                   output = report)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  res <- proc_freq(mtcars, tables = c("cyl * am"),
            plots = freqplot(),
            output = report)


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)



})

test_that("freqplot29: Report with plot can be printed with proc_print.", {

  pth <- file.path(base_path, "print/test13.pdf")

  res <- proc_freq(mtcars, tables = c("cyl * am"),
                   plots = freqplot(),
                   output = report)

  proc_print(res, pth, output_type = "PDF", view = FALSE)


  expect_equal(file.exists(pth), TRUE)
  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)

})

test_that("freqplot30: Parameter checks.", {

  res <- freqplot(type = dotplot, orient = horizontal, scale = percent,
                  twoway = cluster, groupby = row, npanelpos = 5)

  expect_equal(res$groupby, "row")
  expect_equal(res$npanelpos, 5)
  expect_equal(res$orient, "horizontal")
  expect_equal(res$scale, "percent")
  expect_equal(res$twoway, "cluster")
  expect_equal(res$type, "dotplot")

  expect_error(freqplot(type = "fork"))
  expect_error(freqplot(orient = "fork"))
  expect_error(freqplot(scale = "fork"))
  expect_error(freqplot(twoway = "fork"))
  expect_error(freqplot(groupby = "fork"))
  expect_error(freqplot(npanelpos = "fork"))

})


test_that("freqplot31: Odd cases with plots.", {

  if (dev) {

    # Error because of nofreq option
    res <- proc_freq(mtcars, tables = c("cyl"),
                     plots = freqplot,
                     output = report,
                     options = nofreq)


    # Works because scale = percent
    res <- proc_freq(mtcars, tables = c("cyl"),
                     plots = freqplot(scale = percent),
                     output = report,
                     options = nofreq)


    # Lots of bars works
    res <- proc_freq(mtcars, tables = c("disp"),
                     plots = freqplot,
                     output = report)




  } else {

    expect_equal(TRUE, TRUE)

  }

})


test_that("freqplot32: TRUE and multiple plot requests work as expected.", {


  # TRUE
  res <- proc_freq(mtcars, tables = c("cyl", "am", "cyl * am"),
                   plots = TRUE,
                   output = report)

  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)


  # Multiple requests
  res <- proc_freq(mtcars, tables = c("cyl", "am", "cyl * am"),
                   plots = list(freqplot("dotplot"),
                                freqplot("barchart", orient = "horizontal"),
                                freqplot("barchart", twoway = "cluster")),
                   output = report)


  expect_equal("data.frame" %in% class(res[[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]]), TRUE)
  expect_equal("data.frame" %in% class(res[[3]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[4]]), TRUE)
  expect_equal("data.frame" %in% class(res[[5]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[6]]), TRUE)

  # Create sample data
  dat <- read.table(header = TRUE,
                    text = 'x y z
                          6 A 60
                          6 A 70
                          2 A 100
                          2 B 10
                          3 B 67
                          2 C 81
                          3 C 63
                          5 C 55')


  proc_freq(dat, tables = v(x, y, x * y),
            weight = z,
            plots = list(freqplot(type = "dotplot"),
                         freqplot(orient = "horizontal"),
                         freqplot(twoway = "stacked")))


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



  # freq 007
  proc_freq(dat, tables = v(Eyes, Hair * Eyes, Eyes * Hair), weight = Count,
            title = "Eye and Hair Color of European Children",
            plots = list(freqplot("dotplot", orient = "horizontal"),
                         freqplot("barchart", twoway = "stacked"),
                         freqplot("barchart", twoway = "cluster")))


})


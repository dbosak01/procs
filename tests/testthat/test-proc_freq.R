base_path <- "c:/packages/procs/tests/testthat/freq"
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

test_that("freq1: Simple proc_freq test works.", {

  library(fmtr)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   table_options = "FreqCount",
                   titles = "My first Frequency Table",
                   print = "none")

  res

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)

})


test_that("freq2: Simple proc_freq test with print html works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq2.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   table_options = "FreqCount",
                   titles = "My first Frequency Table",
                   print = "HTML",
                   print_location = fl)

  res

  print(fl)
  ex <- file.exists(fl)
  print(ex)

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq3: Two table proc_freq test with print html works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq3.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair"),
                   table_options = "FreqCount",
                   titles = "My first Frequency Table",
                   print = "HTML",
                   print_location = fl)

  res

  ex <- file.exists(fl)

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq4: Simple proc_freq test with weight works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq4.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   table_options = "FreqCount",
                   weight = "Count",
                   titles = "My first Frequency Table",
                   print = "HTML",
                   print_location = fl)

  res

  print(fl)
  ex <- file.exists(fl)
  print(ex)

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq5: Two var proc_freq with weight works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq5.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair"),
                   table_options = "FreqCount",
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   print = "HTML",
                   print_location = fl)

  res

  #print(fl)
  ex <- file.exists(fl)
  #print(ex)

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

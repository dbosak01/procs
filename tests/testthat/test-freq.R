base_path <- "c:/packages/procs/tests/testthat"
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
                   titles = "My first Frequency Table",
                   view = FALSE)

  res

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)

})


test_that("freq2: Simple proc_freq test with report_type html works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq/freq2.html")


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   titles = "My first Frequency Table",
                   report_type = "HTML",
                   report_location = fl)

  res
  ex <- file.exists(fl)

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq3: Two table proc_freq test with report_type html works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq/freq3.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", HairCount = "Hair"),
                   titles = "My first Frequency Table",
                   report_type = "HTML",
                   report_location = fl)

  res

  ex <- file.exists(fl)

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq4: Simple proc_freq test with weight works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq/freq4.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = "HTML",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq5: Two var proc_freq with weight works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq/freq5.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   report_type = "HTML",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})


test_that("freq6: Simple proc_freq in docx works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq/freq6.docx")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = "DOCX",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq7: Simple proc_freq in pdf works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq/freq7.pdf")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = "PDF",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})


test_that("freq8: Simple proc_freq in multiple outputs works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq/freq8")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = c("TXT", "RTF", "PDF"),
                   report_location = fl)

  res
  ex1 <- file.exists(paste0(fl, ".txt"))
  ex2 <- file.exists(paste0(fl, ".rtf"))
  ex3 <- file.exists(paste0(fl, ".pdf"))


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex1, TRUE)
  expect_equal(ex2, TRUE)
  expect_equal(ex3, TRUE)

})

test_that("freq9: Simple proc_freq with no file name works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = "TXT",
                   report_location = fl)

  res
  ex <- file.exists(file.path(fl, "freq.txt"))


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})


test_that("freq10: Crosstab proc_freq works.", {

  library(fmtr)

  fl <- file.path(base_path, "freq/freq10.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c(FreqCount = "Eyes * Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   report_type = "HTML",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 14)
  expect_equal(ncol(res[[1]]), 4)
  expect_equal(ex, TRUE)

})

#
# test_that("freq10: GTables", {
#
#
#
#
#   res2 <- fisher.test(dat$Eyes, dat$Hair)
#
#   class(res2)
#
#   wb <- aggregate(warpbreaks$breaks,
#                   by = list(w = warpbreaks$wool,
#                             t = warpbreaks$tension),
#                   FUN = mean)
#   wb
#   res3 <- friedman.test(wb$x, wb$w, wb$t)
#
#   unlist(res3)
#   friedman.test(x ~ w | t, data = wb)
#
# })


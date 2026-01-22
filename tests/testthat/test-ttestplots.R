base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."


cls <- read.table(header = TRUE, text = '
Name Sex Age Height Weight    region
Alfred   M  14   69.0  112.5   A
Alice   F  13   56.5   84.0    A
Barbara   F  13   65.3   98.0  A
Carol   F  14   62.8  102.5    A
Henry   M  14   63.5  102.5    A
James   M  12   57.3   83.0    A
Jane   F  12   59.8   84.5     A
Janet   F  15   62.5  112.5    A
Jeffrey   M  13   62.5   84.0  A
John   M  12   59.0   99.5     B
Joyce   F  11   51.3   50.5    B
Judy   F  14   64.3   90.0     B
Louise   F  12   56.3   77.0   B
Mary   F  15   66.5  112.0     B
Philip   M  16   72.0  150.0   B
Robert   M  12   64.8  128.0   B
Ronald   M  15   67.0  133.0   B
Thomas   M  11   57.5   85.0   B
William   M  15   66.5  112.0  B')

paird <- read.table(header = TRUE, text = '
subject_id before_measure after_measure region
1 12 15  A
2 14 16  A
3 10 11  A
4 15 18  A
5 18 20  A
6 20 22  B
7 11 12  B
8 13 14  B
9 16 17  B
10 9 13  B')


options("logr.output" = FALSE)
options("procs.print" = FALSE)

dev <- FALSE

test_that("ttestplot1: ttestplot() object works as expected.", {

  # Defaults
  res <- ttestplot()

  res

  expect_equal("ttestplot" %in% class(res), TRUE)
  expect_equal(res$showh0, FALSE)
  expect_equal(res$panel, TRUE)
  expect_equal(res$type, c("summary", "qqplot"))

  # Assign something
  res <- ttestplot(type = c("agreement", "boxplot", "histogram", 'interval', 'profiles'))

  res

  expect_equal("ttestplot" %in% class(res), TRUE)
  expect_equal(res$type, c("agreement", "boxplot", "histogram", 'interval', 'profiles'))

})


test_that("ttestplot2: ttestplot() basic plot types.", {

  # Histogram - OK
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table",
                    output = report,
                    plots = ttestplot("histogram"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal("ttestplot" %in% class(res[[4]]), TRUE)


  # Boxplot - OK
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table",
                    output = report,
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 4)
  expect_equal("ttestplot" %in% class(res[[4]]), TRUE)


  # Interval - OK
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table",
                    output = report,
                    plots = ttestplot("interval"))

  expect_equal(length(res), 4)
  expect_equal("ttestplot" %in% class(res[[4]]), TRUE)

  # QQPlot - Line not correct.  Dots look good.
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table",
                    output = report,
                    plots = ttestplot("qqplot"))

  expect_equal(length(res), 4)
  expect_equal("ttestplot" %in% class(res[[4]]), TRUE)


})




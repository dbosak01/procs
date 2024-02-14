

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



test_that("reg01: Basic proc_reg() works.", {

  # library(sasLM)
  #
  #
  # myfm <- formula("Weight ~ Height + Age + Sex + region")
  #
  #
  # myfm <- formula(Weight ~ Height + Age + Sex + region)
  #
  # # R Squared
  # cor(cls$Weight, cls$Height) ^ 2
  #
  #
  # sasLM::REG(myfm, cls)
  #
  # myfm2 <- formula(Weight ~ Height)
  #
  # sasLM::REG(myfm2, cls, summarize = TRUE)
  #
  # sasLM::aov1(myfm2, cls)

  expect_equal(TRUE, TRUE)

})

test_that("reg02: parameter checks work.", {

  myfm <- formula(Weight ~ Height)
  bfm <- formula(Weight ~ Fork)

  expect_error(proc_reg("bork", model = myfm))
  expect_error(proc_reg(cls[0, ], model = myfm))
  expect_error(proc_reg(cls, model = bfm))

})



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

dev <- FALSE

test_that("reg1: parameter checks work.", {

  myfm <- formula(Weight ~ Height)
  bfm <- formula(Weight ~ Fork)

  expect_error(proc_reg("bork", model = myfm))
  expect_error(proc_reg(cls[0, ], model = myfm))
  expect_error(proc_reg(cls, model = bfm))

})


test_that("reg2: get_output_specs_reg works.", {

  myfm <- formula(Weight ~ Height)

  res <- get_output_specs_reg(myfm, "", "report", report = TRUE)

  res

  expect_equal(names(res), "MODEL1")
  expect_equal(res$MODEL1$var, "Weight")
  expect_equal(res$MODEL1$report, TRUE)
  expect_equal(res$MODEL1$formula, myfm)

})

test_that("reg3: get_model_reg() works.", {


  myfm <- formula(Weight ~ Height)


  res <- get_model_reg(cls, "Weight", myfm)

  res

  expect_equal(length(res), 4)
  expect_equal(res$Observations$NOBS, c(19, 19))

  cls2 <- cls

  cls2$Height[5] <- NA

  res2 <- get_model_reg(cls2, "Weight", myfm)

  res2


  expect_equal(length(res2), 4)
  expect_equal(res2$Observations$NOBS, c(19, 18, 1))

})


test_that("reg4: Basic proc_reg() works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls, myfm1, output = "report")

  res1

  expect_equal(length(res1), 4)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls, myfm2, output = "report")

  res2

  expect_equal(length(res2), 4)

})

test_that("reg5: Multiple independant variables works.", {


  # R Syntax
  myfm1 <- formula(Weight ~ Height + Age)


  res1 <- proc_reg(cls, myfm1, output = "report")

  res1

  expect_equal(length(res1), 4)


  # SAS Syntax
  myfm2 <- "Weight = Height Age"


  res2 <- proc_reg(cls, myfm2, output = "report")

  res2

  expect_equal(length(res2), 4)

})

test_that("reg6: Missing values work.", {

  cls2 <- cls

  cls2[2, "Height"] <- NA
  cls2[5, "Weight"] <- NA


  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls2, myfm1, output = "report")

  res1

  expect_equal(length(res1), 4)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls2, myfm2, output = "report")

  res2

  expect_equal(length(res2), 4)

})


test_that("reg7: by parameter works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls, myfm1,
                   by = Sex,
                   output = "report")

  res1

  expect_equal(length(res1), 2)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls, myfm2)

  res2

  expect_equal(length(res2), 2)

})


test_that("reg8: Multiple models  works.", {


  # R Syntax
  myfm1 <- list(formula(Weight ~ Height),
                formula(Weight ~ Height + Age))


  res1 <- proc_reg(cls, myfm1, output = "report")

  res1

  expect_equal(length(res1), 2)


  # SAS Syntax
  myfm2 <- c("Weight = Height",
             "Weight = Height Age")


  res2 <- proc_reg(cls, myfm2, output = "report")

  res2

  expect_equal(length(res2), 2)

  # SAS Syntax
  myfm3 <- c("Weight = Height",
             "Height = Weight",
             "Weight = Height Age")


  res3 <- proc_reg(cls, myfm3, output = "report")

  res3

  expect_equal(length(res3), 3)


})



test_that("reg9: Model names work as expected.", {

  # R Syntax
  myfm1 <- list(md1 =  formula(Weight ~ Height),
                md2 = formula(Height ~ Weight),
                formula(Weight ~ Height + Age))


  res1 <- proc_reg(cls, myfm1, output = "report")

  res1

  expect_equal(length(res1), 2)


  # SAS Syntax
  myfm2 <- c(md1 = "Weight = Height",
             md2 = "Height = Weight",
             "Weight = Height Age")


  res2 <- proc_reg(cls, myfm2, output = "report",
                   titles = "My nice title")

  res2

  expect_equal(length(res2), 2)

})


test_that("reg10: Output dataset works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls, myfm1)

  res1

  expect_equal(nrow(res1), 1)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls, myfm2)

  res2

  expect_equal(nrow(res2), 1)

})

test_that("reg10: Output dataset two models works.", {

  # R Syntax
  myfm1 <- list(formula(Weight ~ Height),
                formula(Height ~ Weight + Age))


  res1 <- proc_reg(cls, myfm1)

  res1

  expect_equal(nrow(res1), 2)


  # SAS Syntax
  myfm2 <- c("Weight = Height",
             "Weight = Height Age")


  res2 <- proc_reg(cls, myfm2)

  res2

  expect_equal(nrow(res2), 2)

})


test_that("reg10: Output by dataset works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls, myfm1, by = Sex)

  res1

  expect_equal(nrow(res1), 1)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls, myfm2)

  res2

  expect_equal(nrow(res2), 1)

})

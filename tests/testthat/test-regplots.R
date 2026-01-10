
cls <- read.table(header = TRUE, text = '
Name  Sex Age Height Weight
Alfred	M	14	69	112.5
Alice	F	13	56.5	84
Barbara	F	13	65.3	98
Carol	F	14	62.8	102.5
Henry	M	14	63.5	102.5
James	M	12	57.3	83
Jane	F	12	59.8	84.5
Janet	F	15	62.5	112.5
Jeffrey	M	13	62.5	84
John	M	12	59	99.5
Joyce	F	11	51.3	50.5
Judy	F	14	64.3	90
Louise	F	12	56.3	77
Mary	F	15	66.5	112
Philip	M	16	72	150
Robert	M	12	64.8	128
Ronald	M	15	67	133
Thomas	M	11	57.5	85
William	M	15	66.5	112')



options("logr.output" = FALSE)
options("procs.print" = FALSE)

dev <- FALSE



test_that("regplot1: regplot() object works as expected.", {

  res <- regplot()

  res

  expect_equal("regplot" %in% class(res), TRUE)
  expect_equal(res$stats, "default")
  expect_equal(res$panel, TRUE)
  expect_equal(res$type, c("diagnostics", "residuals", "fitplot"))



  res <- regplot(type = c("residuals", "fitplot"),
                 stats = "default",
                 panel = FALSE)

  res

  expect_equal("regplot" %in% class(res), TRUE)
  expect_equal(res$stats, "default")
  expect_equal(res$panel, FALSE)
  expect_equal(res$type, c("residuals", "fitplot"))

})



test_that("regplot2: proc_reg() works for residuals", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "residuals"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})

# Need to control statistics.  Otherwise, good.
test_that("regplot3: proc_reg() works for fitplot.", {

  # 95% Confidence
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "fitplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # 90% Confidence
  proc_reg(cls,
           model = "Weight = Height",
           output = report,
           plots = regplot(type = "fitplot"),
           options = v(alpha = .1))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Should produce no plot because of two independent variables
  res <- proc_reg(cls,
                  model = "Weight = Height Age",
                  output = report,
                  plots = regplot(type = "fitplot"))

  expect_equal(length(res), 4)

})


test_that("regplot4: proc_reg() works for multiple plots.", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = c("residuals", "fitplot")))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

})


# Dots not lining up exactly with SAS.  Close but not perfect.
# Try putting the line from corner to corner, instead of calculating
test_that("regplot5: proc_reg() works for qqplot", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "qqplot"),
                  stats = p)

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})


# OK
test_that("regplot6: proc_reg() works for rfplot plot", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "rfplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})

# Works
test_that("regplot7: proc_reg() panel = FALSE works", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "diagnostics", panel = FALSE))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)
  expect_equal(length(res[[5]]) > 1, TRUE)

})

# Good
test_that("regplot8: proc_reg() works for cooksd plot", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "cooksd"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Labels
  res2 <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "cooksd",
                                  label = TRUE, id = "Name"))

  expect_equal(length(res2), 5)
  expect_equal("plot_spec" %in% class(res2[[5]][[1]]), TRUE)


})


# Good
test_that("regplot9: proc_reg() works for residualbypredicted", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "residualbypredicted"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)



})


# Seems good.  Worry about whether it is always +2 and -2 for boundary lines.
# +2 and -2 appears to be standard deviation. It can be changed on the rstudent
# function, but no obvious parameter on SAS chart.  So probably OK.
test_that("regplot10: proc_reg() works for rstudentbypredicted", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "rstudentbypredicted"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Labels for outliers - Works!
  res2 <- proc_reg(iris,
                  model =  Sepal.Length ~ Petal.Length,
                  output = report,
                  plots = regplot(type = "rstudentbypredicted",
                                  label = TRUE))

  expect_equal(length(res2), 5)
  expect_equal("plot_spec" %in% class(res2[[5]][[1]]), TRUE)


  # Error check
  expect_error(proc_reg(iris,
                   model =  Sepal.Length ~ Petal.Length,
                   output = report,
                   plots = regplot(type = "rstudentbypredicted",
                                   label = TRUE, id = "fork")))

})

# Looks good.
test_that("regplot11: proc_reg() works for rstudentbyleverage", {

  # Class data
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "rstudentbyleverage"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # More data points  - Wow, great!
  res2 <- proc_reg(iris, model = Sepal.Length ~ Petal.Length,
                   output = report,
                   stats = p,
                   plots = regplot(type = "rstudentbyleverage"))


  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # More data
  res3 <- proc_reg(mtcars, model = mpg ~ disp,
                   output = report,
                   plots = regplot(type = "rstudentbyleverage"))


  expect_equal(length(res3), 5)
  expect_equal("plot_spec" %in% class(res3[[5]][[1]]), TRUE)


  # Two independent variables - Still works!
  res3 <- proc_reg(mtcars, model = mpg ~ hp + disp,
                   output = report,
                   plots = regplot(type = "rstudentbyleverage"))


  expect_equal(length(res3), 5)
  expect_equal("plot_spec" %in% class(res3[[5]][[1]]), TRUE)


})


test_that("regplot12: proc_reg() works for rstudentbyleverage with labels and id", {

  # Assigned label - Awesome!
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "rstudentbyleverage",
                                  label = TRUE, id = "Name"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Default label
  res2 <- proc_reg(iris, model = Sepal.Length ~ Petal.Length,
                   output = report,
                   plots = regplot(type = "rstudentbyleverage",
                                   label = TRUE))


  expect_equal(length(res2), 5)
  expect_equal("plot_spec" %in% class(res2[[5]][[1]]), TRUE)

  # Big labels
  cdt <- mtcars
  cdt$name <- rownames(mtcars)

  res3 <- proc_reg(cdt, model = mpg ~ disp,
                   output = report,
                   plots = regplot(type = "rstudentbyleverage",
                                   label = TRUE, id = "name"))


  expect_equal(length(res3), 5)
  expect_equal("plot_spec" %in% class(res3[[5]][[1]]), TRUE)


  # Two independent variables - Still works!
  res4 <- proc_reg(cdt, model = mpg ~ hp + disp,
                   output = report,
                   plots = regplot(type = "rstudentbyleverage",
                                   label = TRUE, id = "name"))


  expect_equal(length(res4), 5)
  expect_equal("plot_spec" %in% class(res4[[5]][[1]]), TRUE)


})


# X and Y Scales have to be the same.
# Margins also matter. Margins can change slope of line.
test_that("regplot13: proc_reg() works for observedbypredicted", {

  # OK
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "observedbypredicted"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # OK
  res <- proc_reg(cls,
                  model = "Weight = Height Age",
                  output = report,
                  plots = regplot(type = "observedbypredicted"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # OK
  res <- proc_reg(cls,
                  model = "Age = Weight",
                  output = report,
                  plots = regplot(type = "observedbypredicted"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # OK
  res <- proc_reg(cls,
                  model = "Age = Height",
                  output = report,
                  plots = regplot(type = "observedbypredicted"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})

test_that("regplot14: proc_reg() works for residualhistogram", {

  # OK
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "residualhistogram"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})


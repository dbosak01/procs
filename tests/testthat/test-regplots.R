
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

hrdat <- read.table(header = TRUE, text = '
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


#   This plot gets split if there are multiple independent variables.
test_that("regplot2: proc_reg() works for residuals", {

  # One independant variable
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "residuals"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Two independent variables
  res <- proc_reg(cls,
                  model = "Weight = Height Age",
                  output = report,
                  plots = regplot(type = "residuals"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  if (dev) {


    cls2 <- cls
    cls2$Sample1 <- round(runif(19, 50, 100))
    cls2$Sample2 <- round(runif(19, 50, 100))
    cls2$Sample3 <- round(runif(19, 50, 100))
    cls2$Sample4 <- round(runif(19, 50, 100))
    cls2$Sample5 <- round(runif(19, 50, 100))
    cls2$Sample6 <- round(runif(19, 50, 100))
    cls2$Sample7 <- round(runif(19, 50, 100))
    cls2$Sample8 <- round(runif(19, 50, 100))
    cls2$Sample9 <- round(runif(19, 50, 100))
    cls2$Sample10 <- round(runif(19, 50, 100))


    # Four independent variables
    res <- proc_reg(cls2,
                    model = "Weight = Height Age Sample1 Sample2",
                    output = report,
                    plots = regplot(type = "residuals"))

    expect_equal(length(res), 5)
    expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)



    # Seven independent variables
    res <- proc_reg(cls2,
                    model = "Weight = Height Age Sample1 Sample2 Sample3 Sample4 Sample5 Sample6",
                    output = report,
                    plots = regplot(type = "residuals"))

    expect_equal(length(res), 5)
    expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)



    # Twelve independent variables
    # Two charts
    mdl <- paste0("Weight = Height Age Sample1 Sample2 Sample3 Sample4 ",
                  "Sample5 Sample6 Sample7 Sample8 Sample9 Sample10")
    res <- proc_reg(cls2,
                    model = mdl,
                    output = report,
                    plots = regplot(type = "residuals"))

    expect_equal(length(res), 5)
    expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  }



})

# Good.
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

  # Custom statistics
  res <- proc_reg(cls,
           model = "Weight = Height",
           output = report,
           plots = regplot(type = "fitplot", stats =
                             c("nobs", "aic", "rsquare")),
           options = v(alpha = .1))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # No statistics
  res <- proc_reg(cls,
           model = "Weight = Height",
           output = report,
           plots = regplot(type = "fitplot", stats = "none"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

})


test_that("regplot4: proc_reg() works for multiple plots.", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = c("diagnostics",
                                           "residuals",
                                           "fitplot")))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

})


# Dots not lining up exactly with SAS.  Close but not perfect.
# Needs work.  Something still wrong.
test_that("regplot5: proc_reg() works for qqplot", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "qqplot") #,
                  # stats = p
                  )

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  res <- proc_reg(cls,
                  model = "Weight = Height Age",
                  output = report,
                  plots = regplot(type = "qqplot") #,
                  # stats = p
  )

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  res <- proc_reg(cls,
                  model = "Height = Weight",
                  output = report,
                  plots = regplot(type = "qqplot") #,
                  # stats = p
  )

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  res <- proc_reg(cls,
                  model = "Height = Weight Age",
                  output = report,
                  plots = regplot(type = "qqplot") #,
                  # stats = p
  )

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


  res <- proc_reg(cls,
                  model = "Weight = Height Age",
                  output = report,
                  plots = regplot(type = "rfplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  res <- proc_reg(cls,
                  model = "Height = Weight",
                  output = report,
                  plots = regplot(type = "rfplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  res <- proc_reg(cls,
                  model = "Height = Weight Age",
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

  res <- proc_reg(cls,
                  model = "Weight = Height Age",
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

# Bins will not match.  Using Sturges algoritm instead.
test_that("regplot14: proc_reg() works for residualhistogram", {

  #
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "residualhistogram"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  res <- proc_reg(cls,
                  model = "Weight = Height Age",
                  output = report,
                  plots = regplot(type = "residualhistogram"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  res <- proc_reg(cls,
                  model = "Height = Weight",
                  output = report,
                  plots = regplot(type = "residualhistogram"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  res <- proc_reg(cls,
                  model = "Height = Weight Age",
                  output = report,
                  plots = regplot(type = "residualhistogram"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)



})


test_that("regplot15: proc_reg() passing type strings works.", {

  # Pass quoted string type
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = "residualhistogram")

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Unquoted string
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = residualhistogram)

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # v function
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = v(fitplot, residuals, residualhistogram))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Vector of strings
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = c("fitplot", "residuals", "residualhistogram"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

})

# This is amazing
test_that("regplot16: proc_reg() works for diagnostics", {

  # One variable
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "diagnostics"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Two independent variables
  res <- proc_reg(cls,
                  model = "Weight = Height Age",
                  output = report,
                  plots = regplot(type = "diagnostics"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Control stats
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "diagnostics",
                                  stats = c("default", "aic")))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # No stats
  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "diagnostics",
                                  stats = "none"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})


test_that("regplot17: proc_reg() edge cases.", {


  # New data
  res <- proc_reg(iris,
                  model = "Sepal.Length = Petal.Length",
                  output = report,
                  plots = regplot(type = c("diagnostics", "residualhistogram",
                            "cooksd"), label = TRUE))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # New data
  res <- proc_reg(iris,
                  model = "Sepal.Length = Petal.Length",
                  output = report,
                  plots = regplot(type = c("diagnostics", "residualhistogram",
                                           "residuals"), label = TRUE,
                                  panel = FALSE))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Multiple models
  res <- proc_reg(iris, model = c("Sepal.Length = Petal.Length",
                                  "Sepal.Length = Sepal.Width",
                                  "Sepal.Length = Petal.Width"),
                   output = report,
                   plots = "diagnostics",
                   titles = "Iris Regression Statistics")

  expect_equal(length(res), 3) # 3 Models
  expect_equal("plot_spec" %in% class(res[[1]][[5]][[1]]), TRUE)

})


test_that("regplot18: regplot() statistics work as expected.", {

  # ADJRSQ, AIC, BIC, COEFFVAR, CP, DEFAULT, DEPMEAN, EDF, GMSEP, JP,
  # MSE, NOBS, NPARM, PC, RSQUARE, SBC, SP, SSE

  res <- proc_reg(cls,
                  model = Weight ~ Height,
                  output = report,
                  plots = regplot(type = "diagnostics",
                                  stats = c("nobs", "edf", "rsquare", "adjrsq",
                                            "nparm", "coeffvar", "mse", "depmean")))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  res <- proc_reg(cls,
                  model = Weight ~ Height,
                  output = report,
                  plots = regplot(type = "diagnostics",
                                  stats = c("nobs", "aic", "sse")))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  res <- proc_reg(cls,
                  model = Weight ~ Height,
                  output = report,
                  plots = regplot(type = "fitplot",
                                  stats = c("nobs", "aic", "sse")))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # fit <- lm(Weight ~ Age , cls)
  #
  # avl <- extractAIC(fit)
  #
  # avl
  #
  # bvl <- extractAIC(fit, k = log(nrow(cls)))
  #
  # bvl
  #
  # bvl2 <- BIC(fit)
  #
  # bvl3 <- extractAIC(fit, log(nrow(cls)))


})


# Works
test_that("regplot19: proc_reg() works for residualboxplot", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "residualboxplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  res <- proc_reg(cls,
                  model = "Weight = Height Age",
                  output = report,
                  plots = regplot(type = "residualboxplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

})




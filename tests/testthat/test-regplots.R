
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

# Needs option to change confidence interval
test_that("regplot3: proc_reg() works for fitplot.", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "fitplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

proc_reg(cls,
         model = "Weight = Height",
         output = report,
         plots = regplot(type = "fitplot"))

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
test_that("regplot5: proc_reg() works for qqplot", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "qqplot"),
                  stats = p)

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})


#
test_that("regplot6: proc_reg() works for spread plot", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "spreadplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})

test_that("regplot6: proc_reg() panel = FALSE works", {


  res <- proc_reg(cls,
                  model = "Weight = Height",
                  output = report,
                  plots = regplot(type = "diagnostics", panel = FALSE))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})


base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE

datm <- read.table(header = TRUE, text = '
LastName  Age PresentScore TasteScore Flavor Layers
Orlando     27 93 80  Vanilla    1
Ramey       32 84 72  Rum        2
Goldston    46 68 75  Vanilla    1
Roe         38 79 73  Vanilla    2
Larsen      23 77 84  Chocolate  3
Davis       51 86 91  Spice      3
Strickland  19 82 79  Chocolate  1
Nguyen      57 77 84  Vanilla    3
Hildenbrand 33 81 83  Chocolate  1
Byron       62 72 87  Vanilla    2
Sanders     26 56 79  Chocolate  1
Jaeger      43 66 74  NA         1
Davis       28 69 75  Chocolate  2
Conrad      69 85 94  Vanilla    1
Walters     55 67 72  Chocolate  2
Rossburger  28 78 81  Spice      2
Matthew     42 81 92  Chocolate  2
Becker      36 62 83  Spice      2
Anderson    27 87 85  Chocolate  1
Merritt     62 73 84  Chocolate  1
')

test_that("means0: get_summaries works as expected for 1 variable.", {


  res <- get_summaries(datm, var = "PresentScore",
                       stats = c("n", "mean", "min", "max", "range", "median"))

  res


  expect_equal(res[1, "N"], 20)
  expect_equal(res[1, "Mean"], 76.15)
  expect_equal(res[1, "Minimum"], 56)
  expect_equal(res[1, "Maximum"], 93)
  expect_equal(res[1, "Range"], 37)
  expect_equal(res[1, "Median"], 77.5)

})

test_that("means1: get_summaries works as expected for two variables.", {


  res <- get_summaries(datm, var = c("PresentScore", "TasteScore"),
                       stats = c("n", "mean", "min", "max", "range", "median"))

  res


  expect_equal(res[2, "N"], 20)
  expect_equal(res[2, "Mean"], 81.35)
  expect_equal(res[2, "Minimum"], 72)
  expect_equal(res[2, "Maximum"], 94)
  expect_equal(res[2, "Range"], 22)
  expect_equal(res[2, "Median"], 82)

})



test_that("means2: proc_means works as expected with two variables.", {

  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    stats = c("n", "mean", "max", "min",
                              "range", "median", "std"),
                    titles = "My first title for Means")

  res

  expect_equal(length(res), 1)
  expect_equal(nrow(res[[1]]), 2)
  expect_equal(ncol(res[[1]]), 8)

  res <- res[[1]]
  expect_equal(res[2, "N"], 20)
  expect_equal(res[2, "Mean"], 81.35)
  expect_equal(res[2, "Minimum"], 72)
  expect_equal(res[2, "Maximum"], 94)
  expect_equal(res[2, "Range"], 22)
  expect_equal(res[2, "Median"], 82)


})

test_that("means3: check more stats options and piped parameter.", {

  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    stats = c("sum", "nobs", "nmiss", "var", "css", "cv"),
                    titles = "My first title for Means",
                    piped = TRUE)

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 7)

  expect_equal(res[2, "Sum"], 1627)
  expect_equal(res[2, "Nobs"], 20)
  expect_equal(res[2, "NMiss"], 0)
  expect_equal(res[2, "CSS"], 830.55)
  expect_equal(res[2, "CV"] > 8, TRUE)

})

test_that("means4: proc_means with two variables and by group.", {

  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    by = "Layers",
                    stats = c("n", "mean", "max", "min",
                              "range", "median", "std"),
                    titles = "My first title for Means")

  res

  expect_equal(length(res), 3)
  expect_equal(nrow(res[[1]]), 2)
  expect_equal(ncol(res[[1]]), 8)



})

test_that("means5: proc_means with two variables and two by groups.", {

  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    by = c("Flavor", "Layers"),
                    stats = c("n", "mean", "max", "min",
                              "range", "median", "std"),
                    titles = "My first title for Means")

  res

  expect_equal(length(res), 12)
  expect_equal(nrow(res[[1]]), 2)
  expect_equal(ncol(res[[1]]), 8)

})


test_that("means6: get_summaries works as expected for two variables with v().", {


  res <- get_summaries(datm, var = v(PresentScore, TasteScore),
                       stats = v(n, mean, min, max, range, median))

  res


  expect_equal(res[2, "N"], 20)
  expect_equal(res[2, "Mean"], 81.35)
  expect_equal(res[2, "Minimum"], 72)
  expect_equal(res[2, "Maximum"], 94)
  expect_equal(res[2, "Range"], 22)
  expect_equal(res[2, "Median"], 82)

})


test_that("means7: proc_means with single parameter values.", {

  res <- proc_means(datm, var = "PresentScore",
                    stats = "mean",
                    titles = "My first title for Means")

  res

  expect_equal(length(res), 1)
  expect_equal(nrow(res[[1]]), 1)
  expect_equal(ncol(res[[1]]), 2)

})


test_that("means8: proc_means with unquoted parameter values.", {

  res <- proc_means(datm, var = PresentScore,
                    stats = mean,
                    titles = "My first title for Means")

  res

  expect_equal(length(res), 1)
  expect_equal(nrow(res[[1]]), 1)
  expect_equal(ncol(res[[1]]), 2)

})


test_that("means9: parameter checks work.", {

  expect_error(proc_means(datm, var = "Fork",
                    stats = "mean"))

  expect_error(proc_means(datm, var = "PresentScore",
                          stats = "Fork"))

  expect_error(proc_means(datm, by = "Fork",
                          var = "PresentScore",
                          stats = "mean"))

})



test_that("means10: proc_means with variable parameter values.", {

  var1 <- "PresentScore"
  var2 <- c("n", "mean", "min", "max")

  res <- proc_means(datm, var = var1,
                    stats = var2,
                    titles = "My first title for Means")

  res

  expect_equal(length(res), 1)
  expect_equal(nrow(res[[1]]), 1)
  expect_equal(ncol(res[[1]]), 5)

})


test_that("means10: proc_means with variable parameter values and v().", {

  var1 <- "PresentScore"
  var2 <- v(n, mean, min, max, median)

  res <- proc_means(datm, var = var1,
                    stats = var2,
                    titles = "My first title for Means")

  res

  expect_equal(length(res), 1)
  expect_equal(nrow(res[[1]]), 1)
  expect_equal(ncol(res[[1]]), 6)

})


test_that("means11: proc_means in function works.", {

  var1 <- c("PresentScore", "TasteScore")
  var2 <- v(n, mean, min, max, median)

  myfunc <- function(myvar1, myvar2) {

    myres <- proc_means(datm, var = var1,
                      stats = var2,
                      titles = "My first title for Means")

    return(myres)
  }

  res <- myfunc(var1, var2)

  expect_equal(length(res), 1)
  expect_equal(nrow(res[[1]]), 2)
  expect_equal(ncol(res[[1]]), 6)

})



test_that("means3: check more stats options and piped parameter.", {

  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    stats = c("nmiss", "median", "mode", "clm", "stderr"),
                    titles = "My first title for Means",
                    piped = TRUE)

  res

  mode(datm$TasteScore)

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 7)

  expect_equal(res[2, "NMiss"], 0)
  expect_equal(res[2, "Median"], 82)
  expect_equal(res[2, "Mode"], 84)
  expect_equal(res[2, "UCLM"], 84.44432)
  expect_equal(res[2, "LCLM"], 78.25568)
  expect_equal(res[2, "Std_Err"], 1.47839707)

})

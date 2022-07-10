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
                       stats = c("n", "mean", "min", "max", "range", "median"),
                       missing = FALSE)

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



test_that("means12: check more stats options and piped parameter.", {

  res <- proc_means(datm, var = v(PresentScore, TasteScore),
                    stats = v(nmiss, median, mode, clm, stderr),
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

test_that("means13: check missing parameter works.", {


  datm2 <- datm

  datm2[5, "PresentScore"] <- NA

  res <- proc_means(datm2, var = c("PresentScore", "TasteScore"),
                    stats = c("n", "nmiss", "mean", "median", "mode", "clm", "std"),
                    titles = "My first title for Means")[[1]]

  res


  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 9)


  expect_equal(res[1, "NMiss"], 1)
  expect_equal(res[1, "Mean"], 76.105263158)
  expect_equal(res[1, "Median"], 78)
  expect_equal(res[1, "Mode"], 81)
  expect_equal(res[1, "UCLM"], 80.747505)
  expect_equal(res[1, "LCLM"], 71.463022)
  expect_equal(res[1, "Std_Dev"], 9.6315150351)

  expect_equal(res[2, "NMiss"], 0)
  expect_equal(res[2, "Median"], 82)
  expect_equal(res[2, "Mode"], 84)
  expect_equal(res[2, "UCLM"], 84.44432)
  expect_equal(res[2, "LCLM"], 78.25568)
  expect_equal(res[2, "Std_Dev"], 6.6115926897)

})

# These match SAS.
test_that("means13: check missing parameter works.", {


  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    stats = c("n", "p1", "p5", "p10", "p20", "p25", "p30",
                              "p40", "p50",
                              "p60", "p70",
                              "p75", "p80", "p90", "p95", "p99", "q1", "q3",
                              "qrange"),
                    titles = "My first title for Means")[[1]]


  res
  expect_equal(res[1, "N"], 20)
  expect_equal(res[1, "P1"], 56)
  expect_equal(res[1, "P5"], 59)
  expect_equal(res[1, "P10"], 64)
  expect_equal(res[1, "P20"], 67.5)
  expect_equal(res[1, "P25"], 68.5)
  expect_equal(res[1, "P30"], 70.5)
  expect_equal(res[1, "P40"], 75)
  expect_equal(res[1, "P50"], 77.5)
  expect_equal(res[1, "P60"], 80)
  expect_equal(res[1, "P70"], 81.5)
  expect_equal(res[1, "P75"], 83)
  expect_equal(res[1, "P80"], 84.5)
  expect_equal(res[1, "P90"], 86.5)
  expect_equal(res[1, "P95"], 90)
  expect_equal(res[1, "P99"], 93)
  expect_equal(res[1, "Q1"], 68.5)
  expect_equal(res[1, "Q3"], 83)
  expect_equal(res[1, "QRange"], 14.5)

  expect_equal(res[2, "N"], 20)
  expect_equal(res[2, "P1"], 72)
  expect_equal(res[2, "P5"], 72)
  expect_equal(res[2, "P10"], 72.5)
  expect_equal(res[2, "P20"], 74.5)
  expect_equal(res[2, "P25"], 75)
  expect_equal(res[2, "P30"], 77)
  expect_equal(res[2, "P40"], 79.5)
  expect_equal(res[2, "P50"], 82)
  expect_equal(res[2, "P60"], 83.5)
  expect_equal(res[2, "P70"], 84)
  expect_equal(res[2, "P75"], 84.5)
  expect_equal(res[2, "P80"], 86)
  expect_equal(res[2, "P90"], 91.5)
  expect_equal(res[2, "P95"], 93)
  expect_equal(res[2, "P99"], 94)
  expect_equal(res[2, "Q1"], 75)
  expect_equal(res[2, "Q3"], 84.5)
  expect_equal(res[2, "QRange"], 9.5)

})

test_that("means14: default vars works.", {

  res <- proc_means(datm)

  res

  expect_equal(nrow(res[[1]]), 4)
  expect_equal(ncol(res[[1]]), 6)

})

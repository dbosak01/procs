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

options("logr.output" = FALSE)
options("procs.print" = FALSE)

test_that("means0: get_summaries works as expected for 1 variable.", {


  res <- get_summaries(datm, var = "PresentScore",
                       stats = c("n", "mean", "min", "max", "range", "median"),
                       missing = FALSE)

  res


  expect_equal(res[1, "N"], 20)
  expect_equal(res[1, "MEAN"], 76.15)
  expect_equal(res[1, "MIN"], 56)
  expect_equal(res[1, "MAX"], 93)
  expect_equal(res[1, "RANGE"], 37)
  expect_equal(res[1, "MEDIAN"], 77.5)

})

test_that("means1: get_summaries works as expected for two variables.", {


  res <- get_summaries(datm, var = c("PresentScore", "TasteScore"),
                       stats = c("n", "mean", "min", "max", "range", "median"))

  res


  expect_equal(res[2, "N"], 20)
  expect_equal(res[2, "MEAN"], 81.35)
  expect_equal(res[2, "MIN"], 72)
  expect_equal(res[2, "MAX"], 94)
  expect_equal(res[2, "RANGE"], 22)
  expect_equal(res[2, "MEDIAN"], 82)

})



test_that("means2: gen_report_means works as expected with two variables.", {

  res <- gen_report_means(datm, var = c("PresentScore", "TasteScore"),
                    stats = c("n", "mean", "max", "min",
                              "range", "median", "std"),
                    titles = "My first title for Means")

  res

  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res), 2)
  expect_equal(ncol(res), 8)

  expect_equal(res[2, "N"], 20)
  expect_equal(res[2, "MEAN"], 81.35)
  expect_equal(res[2, "MIN"], 72)
  expect_equal(res[2, "MAX"], 94)
  expect_equal(res[2, "RANGE"], 22)
  expect_equal(res[2, "MEDIAN"], 82)


})

# test_that("means3: check more stats options and piped parameter.", {
#
#   res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
#                     stats = c("sum", "nobs", "nmiss", "var", "css", "cv"),
#                     titles = "My first title for Means",
#                     piped = TRUE)
#
#   res
#
#   expect_equal("data.frame" %in% class(res), TRUE)
#   expect_equal(ncol(res), 7)
#
#   expect_equal(res[2, "Sum"], 1627)
#   expect_equal(res[2, "Nobs"], 20)
#   expect_equal(res[2, "NMISS"], 0)
#   expect_equal(res[2, "CSS"], 830.55)
#   expect_equal(res[2, "CV"] > 8, TRUE)
#
# })

test_that("means4: gen_report_means with two variables and by group.", {

  res <- gen_report_means(datm, var = c("PresentScore", "TasteScore"),
                    by = "Layers",
                    stats = c("n", "mean", "max", "min",
                              "range", "median", "std"),
                    titles = "My first title for Means")

  res

  expect_equal(length(res), 3)
  expect_equal(nrow(res[[1]]), 2)
  expect_equal(ncol(res[[1]]), 8)



})

test_that("means5: gen_report_means with two variables and two by groups.", {

  res <- gen_report_means(datm, var = c("PresentScore", "TasteScore"),
                    by = c("Flavor", "Layers"),
                    stats = c("n", "mean", "max", "min",
                              "range", "median", "std"),
                    titles = "My first title for Means")

  res

  expect_equal(length(res), 9)
  expect_equal(nrow(res[[1]]), 2)
  expect_equal(ncol(res[[1]]), 8)

})


test_that("means6: get_summaries works as expected for two variables with v().", {

  library(common)

  res <- get_summaries(datm, var = v(PresentScore, TasteScore),
                       stats = v(n, mean, min, max, range, median))

  res


  expect_equal(res[2, "N"], 20)
  expect_equal(res[2, "MEAN"], 81.35)
  expect_equal(res[2, "MIN"], 72)
  expect_equal(res[2, "MAX"], 94)
  expect_equal(res[2, "RANGE"], 22)
  expect_equal(res[2, "MEDIAN"], 82)

})


test_that("means7: gen_report_means with single parameter values.", {

  res <- gen_report_means(datm, var = "PresentScore",
                    stats = "mean",
                    titles = "My first title for Means")

  res

  expect_equal(is.data.frame(res), TRUE)
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 2)

})


test_that("means8: proc_means with unquoted parameter values.", {

  res <- proc_means(datm, var = PresentScore,
                    stats = mean,
                    titles = "My first title for Means")

  res


  expect_equal(is.null(res), FALSE)
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 4)

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

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 7)

})


test_that("means11: proc_means with variable parameter values and v().", {


  var1 <- "PresentScore"
  var2 <- v(n, mean, min, max, median)

  res <- proc_means(datm, var = var1,
                    stats = var2,
                    titles = "My first title for Means")

  res

  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 8)

})


test_that("means12: proc_means in function works.", {

  var1 <- c("PresentScore", "TasteScore")
  var2 <- v(n, mean, min, max, median)

  myfunc <- function(myvar1, myvar2) {

    myres <- proc_means(datm, var = var1,
                      stats = var2,
                      titles = "My first title for Means",
                      output = long)

    return(myres)
  }

  res <- myfunc(var1, var2)

  expect_equal(nrow(res), 5)
  expect_equal(ncol(res), 5)

})



test_that("means13: check more stats options", {

  res <- proc_means(datm, var = v(PresentScore, TasteScore),
                    stats = v(nmiss, median, mode, clm, stderr),
                    options = v(nonobs, notype),
                    titles = "My first title for Means")

  res


  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 7)

  expect_equal(res[2, "NMISS"], 0)
  expect_equal(res[2, "MEDIAN"], 82)
  expect_equal(res[2, "MODE"], 84)
  expect_equal(res[2, "UCLM"], 84.44432)
  expect_equal(res[2, "LCLM"], 78.25568)
  expect_equal(res[2, "STDERR"], 1.47839707)

})

test_that("means14: check missing value works.", {


  datm2 <- datm

  datm2[5, "PresentScore"] <- NA

  res <- proc_means(datm2, var = c("PresentScore", "TasteScore"),
                    stats = c("n", "nmiss", "mean", "median", "mode", "clm", "std"),
                    titles = "My first title for Means",
                    options = v(notype, nonobs))

  res


  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 9)


  expect_equal(res[1, "NMISS"], 1)
  expect_equal(res[1, "MEAN"], 76.105263158)
  expect_equal(res[1, "MEDIAN"], 78)
  expect_equal(res[1, "MODE"], 81)
  expect_equal(res[1, "UCLM"], 80.747505)
  expect_equal(res[1, "LCLM"], 71.463022)
  expect_equal(res[1, "STD"], 9.6315150351)

  expect_equal(res[2, "NMISS"], 0)
  expect_equal(res[2, "MEDIAN"], 82)
  expect_equal(res[2, "MODE"], 84)
  expect_equal(res[2, "UCLM"], 84.44432)
  expect_equal(res[2, "LCLM"], 78.25568)
  expect_equal(res[2, "STD"], 6.6115926897)

})

# These match SAS.
test_that("means15: check missing parameter works.", {


  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    stats = c("n", "p1", "p5", "p10", "p20", "p25", "p30",
                              "p40", "p50",
                              "p60", "p70",
                              "p75", "p80", "p90", "p95", "p99", "q1", "q3",
                              "qrange"),
                    titles = "My first title for Means")


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
  expect_equal(res[1, "QRANGE"], 14.5)

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
  expect_equal(res[2, "QRANGE"], 9.5)

})

test_that("means16: default vars works.", {

  res <- proc_means(datm, output = long)

  res

  expect_equal(nrow(res), 5)
  expect_equal(ncol(res), 7)

})

test_that("means17: get_summaries shape long works as expected.", {


  res <- get_summaries(datm, var = c("PresentScore", "TasteScore"),
                       stats = c("n", "mean", "min", "max", "range", "median"),
                       shape = "long")

  res


  expect_equal(res[1, "PresentScore"], 20)
  expect_equal(res[2, "PresentScore"], 76.15)
  expect_equal(res[3, "PresentScore"], 56)
  expect_equal(res[4, "PresentScore"], 93)
  expect_equal(res[5, "PresentScore"], 37)
  expect_equal(res[6, "PresentScore"], 77.5)

})


test_that("means18: get_output long works.", {

  res <- get_output(datm,  var = c("PresentScore", "TasteScore"),
                    stats = c("n", "mean", "min", "max", "range", "median"),
                    shape = "long", type = 0, freq = TRUE,
                    by = c(by1 = "By1"),
                    class = c(cls1 = "Class1", cls2 = "Class2"))

  res

  expect_equal(nrow(res), 6)
  expect_equal(res$BY[1], "By1")
  expect_equal(res$CLASS1[1], "Class1")
  expect_equal(res$CLASS2[1], "Class2")
  expect_equal(res$TYPE[1], 0)
  expect_equal(res$FREQ[1], 20)

})


test_that("means19: get_output wide works.", {

  res <- get_output(datm,  var = c("PresentScore", "TasteScore"),
                    stats = c("n", "mean", "min", "max", "range", "median"),
                    shape = "wide", type = 0, freq = TRUE,
                    by = c(by1 = "By1"),
                    class = c(cls1 = "Class1", cls2 = "Class2"))

  res

  expect_equal(nrow(res), 2)
  expect_equal(res$BY[1], "By1")
  expect_equal(res$CLASS1[1], "Class1")
  expect_equal(res$CLASS2[1], "Class2")
  expect_equal(res$TYPE[1], 0)
  expect_equal(res$FREQ[1], 20)

})


test_that("means20: get_output wide no by or class works.", {

  res <- get_output(datm,  var = c("PresentScore", "TasteScore"),
                    stats = c("n", "mean", "min", "max", "range", "median"),
                    shape = "wide", type = 0, freq = TRUE)

  res

  expect_equal(nrow(res), 2)
  expect_equal(res$TYPE[1], 0)
  expect_equal(res$FREQ[1], 20)

})


test_that("means21: get_output wide no extra works.", {

  res <- get_output(datm,  var = c("PresentScore", "TasteScore"),
                    stats = c("n", "mean", "min", "max", "range", "median"),
                    shape = "wide")

  res

  expect_equal(nrow(res), 2)
  expect_equal("TYPE" %in% names(res), FALSE)
  expect_equal("FREQ" %in% names(res), FALSE)

})



test_that("means22: gen_output_means works.", {

  res <- gen_output_means(datm,
                          var = c("PresentScore", "TasteScore"),
          output = list(out1 = out_spec(stats = c("n", "mean", "min", "max"),
                                   shape = "long"),
                        out2 = out_spec(stats = c("n", "mean", "std"),
                                   shape = "long")))

  res

  expect_equal(length(res), 2)

  res1 <- res[[1]]

  labels(res1) <- NULL
  expect_equal(res1[["STAT"]], c("N", "MEAN", "MIN", "MAX"))

  res2 <- res[[2]]
  labels(res2) <- NULL
  expect_equal(res2$STAT, c("N", "MEAN", "STD"))


})


test_that("means23: get_class works.", {

  res <- get_class_report(datm, var = c("PresentScore", "TasteScore"),
                   class = "Layers",
                   outp = out_spec(stats = c("n", "mean", "min", "max"),
                                 shape = "wide"))


  res

  expect_equal(nrow(res), 6)


  res <- get_class_output(datm, var = c("PresentScore", "TasteScore"),
                   class = "Layers",
                   outp = out_spec(stats = c("n", "mean", "min", "max"),
                              shape = "wide"))


  res

  expect_equal(nrow(res), 6)

})



test_that("means24: gen_output_means works.", {

  res <- gen_output_means(datm,
                          var = c("PresentScore", "TasteScore"),
                          class = "Layers",
                          output = list(out1 = out_spec(stats = c("n", "mean", "min", "max"),
                                                   shape = "long"),
                                        out2 = out_spec(stats = c("n", "mean", "std"),
                                                   shape = "long")))

  res

  expect_equal(length(res), 2)

  res1 <- res[[1]]
  expect_equal(res1$STAT[1:4], c("N", "MEAN", "MIN", "MAX"))

  res2 <- res[[2]]
  expect_equal(res2$STAT[1:3], c("N", "MEAN", "STD"))


})

# Matches SAS
test_that("means25: class parameter works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n", "min", "max", "mean", "std")

  res <- proc_means(datm, var = var1,
                    stats = var2,
                    class = Layers,
                    titles = "My first title for Means",
                    output = long)

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(nrow(res), 20)
  expect_equal(ncol(res), 7)
  expect_equal(res[1, 5], 20)
  expect_equal(res[2, 5], 19)
  expect_equal(res[3, 5], 69)
  expect_equal(res[4, 5], 40.2)
  expect_equal(res[5, 5], 14.827428354)
  expect_equal(res[6, 6], 9)
  expect_equal(res[7, 6], 56)
  expect_equal(res[8, 6], 93)
  expect_equal(res[9, 6], 76.777777778)
  expect_equal(res[10, 6], 11.829811683)
  expect_equal(res[11, 7], 8)
  expect_equal(res[12, 7], 72)
  expect_equal(res[13, 7], 92)
  expect_equal(res[14, 7], 79.375)
  expect_equal(res[15, 7], 7.5769858312)
  expect_equal(res[16, 7], 3)
  expect_equal(res[17, 7], 84)
  expect_equal(res[18, 7], 91)
  expect_equal(res[19, 7], 86.333333333)
  expect_equal(res[20, 7], 4.0414518843)
})


test_that("means26: get_output with by works", {
  var2 <- c("n", "min", "max", "mean", "std")

  res <- get_output(datm, var = "PresentScore", stats = var2,
             by = c("Layers" = 1), shape = "long")
  res

  expect_equal(nrow(res), 5)

})

test_that("means27: by parameter works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n",  "mean", "std", "min", "max")

  res <- proc_means(datm, var = var1,
                    stats = var2,
                    by = "Layers",
                  #  class = "Flavor",
                    titles = "My first title for Means",
                    output = long)

  res

  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 7)

})

# Need to work on this
test_that("means28: by with class works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n",  "mean", "std", "min", "max")

  res <- proc_means(datm, var = var1,
                    stats = var2,
                    by = "Layers",
                    class = "Flavor",
                    titles = "My first title for Means",
                    output = long)

  res

  expect_equal(nrow(res), 60)
  expect_equal(ncol(res), 8)

})


test_that("means29: 2 class vars works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n",  "mean", "std", "min", "max")

  res <- proc_means(datm, var = var1,
                    stats = var2,
                    #by = "Layers",
                    class = c("Layers", "Flavor"),
                    titles = "My first title for Means",
                    output = long)

  res

  expect_equal(nrow(res), 85)
  expect_equal(ncol(res), 8)

})



test_that("means30: by and 2 class vars works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n",  "mean", "std", "min", "max")
  datm2 <- datm
  datm2$Group <- c(rep("A", 10), rep("B", 10))

  res <- proc_means(datm2, var = var1,
                    stats = var2,
                    by = "Group",
                    class = c("Flavor", "Layers"),
                    titles = "My first title for Means",
                    output = long)

  res

  expect_equal(nrow(res), 125)
  expect_equal(ncol(res), 9)
  expect_equal(all(c("BY") %in% names(res)), TRUE)

})


# ?
test_that("means31: Outputs work as expected.", {

  options("procs.print" = FALSE)

  res <- proc_means(datm, output = none)

  res

  expect_equal(is.null(res), TRUE)


  res2 <- proc_means(datm, output = out)

  expect_equal(nrow(res2), 4)
  expect_equal(ncol(res2), 8)

  options("procs.print" = NULL)

})


test_that("means32: get_class works with empty data frame.", {

  dftmp <- datm[datm$Layers == 42, ]

  res <- get_class_report(dftmp, var = c("PresentScore", "TasteScore"),
                   class = "Layers",
                   outp = out_spec(stats = c("n", "mean", "min", "max"),
                              shape = "wide"))


  res

  expect_equal(nrow(res), 2)

  res <- get_class_output(dftmp, var = c("PresentScore", "TasteScore"),
                   class = "Layers",
                   outp = out_spec(stats = c("n", "mean", "min", "max"),
                              shape = "wide"))


  res

  expect_equal(nrow(res), 2)


})


test_that("means33: 2 by and 1 class vars works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n",  "mean", "std", "min", "max")
  datm2 <- datm
  datm2$Group <- c(rep("A", 10), rep("B", 10))

  res <- proc_means(datm2, var = var1,
                    stats = var2,
                    by = c("Group", "Layers"),
                    class = "Flavor",
                    titles = "My first title for Means",
                    output = long)

  res

  expect_equal(nrow(res), 80)
  expect_equal(ncol(res), 9)
  expect_equal(all(c("BY1", "BY2") %in% names(res)), TRUE)

})



test_that("means34: Report output works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")

  res <- proc_means(datm,
                    by = "Layers",
                    output = report)

  res

  expect_equal(length(res), 3)
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 6)
  expect_equal(nrow(res[[2]]), 3)
  expect_equal(ncol(res[[2]]), 6)
  expect_equal(nrow(res[[3]]), 3)
  expect_equal(ncol(res[[3]]), 6)

})

test_that("means35: Stacked output works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")

  res <- proc_means(datm,
                    by = "Layers",
                    output = stacked)

  res

  expect_equal(nrow(res), 45)
  expect_equal(ncol(res), 6)

})


test_that("means36: log_means() works as expected.", {


  res <- log_means(mtcars, var = c("mpg", "cyl"),
                   stats = c("n", "mean", "median"),
                  weight = "count", titles = c("here is my big long title",
                                               "Here is another title"),
                  by = "cyl", class="disp", outcnt = 6)

  res

  expect_equal(length(res), 9)

})


test_that("means37: warning on unknown parameter works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n", "min", "max", "mean", "std")

  expect_error(proc_means(datm, var = var1,
                    stats = var2,
                    class = Layers,
                    fork = TRUE,
                    titles = "My first title for Means"))

})



test_that("means38: view can be FALSE without error.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n", "min", "max", "mean", "std")

  res <- proc_means(datm, var = var1,
                    stats = var2,
                    class = Layers,
                    titles = "My first title for Means",
                    options = noprint)


  expect_equal(is.null(res), FALSE)

})


test_that("means39: check more confidence limit options", {

  # 2-sided 95%
  res <- proc_means(datm, var = v(PresentScore, TasteScore),
                    stats = v(mean, clm),
                    options = v(nonobs, notype, alpha = .05),
                    titles = "My first title for Means")

  res

  frt <- v(nonobs, notype, alpha = .05)

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 4)

  expect_equal(res[2, "MEAN"], 81.35)
  expect_equal(res[2, "UCLM"], 84.44432)
  expect_equal(res[2, "LCLM"], 78.25568)


  # 2-sided 90%
  res <- proc_means(datm, var = v(PresentScore, TasteScore),
                    stats = v(mean, clm),
                    options = v(nonobs, notype, alpha = .1),
                    titles = "My first title for Means")

  res


  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 4)

  expect_equal(res[2, "MEAN"], 81.35)
  expect_equal(res[2, "UCLM"], 83.906345)
  expect_equal(res[2, "LCLM"], 78.793655)


  # 1-sided 90% uclm
  res <- proc_means(datm, var = v(PresentScore, TasteScore),
                    stats = v(mean, uclm),
                    options = v(nonobs, notype, alpha = .1),
                    titles = "My first title for Means")

  res


  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 3)

  expect_equal(res[2, "MEAN"], 81.35)
  expect_equal(res[2, "UCLM"], 83.31291)


  # 1-sided 90% lclm
  res <- proc_means(datm, var = v(PresentScore, TasteScore),
                    stats = v(mean, lclm),
                    options = v(nonobs, notype, alpha = .1),
                    titles = "My first title for Means")

  res


  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 3)

  expect_equal(res[2, "MEAN"], 81.35)
  expect_equal(res[2, "LCLM"], 79.38709)



  # 2-sided 90% with individual stat keywords
  res <- proc_means(datm, var = v(PresentScore, TasteScore),
                    stats = v(mean, lclm, uclm),
                    options = v(nonobs, notype, alpha = .1),
                    titles = "My first title for Means")

  res


  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 4)

  expect_equal(res[2, "MEAN"], 81.35)
  expect_equal(res[2, "UCLM"], 83.906345)
  expect_equal(res[2, "LCLM"], 78.793655)

})


test_that("means40: check maxdec option", {

  # 2-sided 95%
  res <- proc_means(datm, var = v(PresentScore, TasteScore),
                    stats = v(mean, clm),
                    options = v(nonobs, notype, maxdec = 2),
                    titles = "My first title for Means")

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 4)

  expect_equal(res[2, "MEAN"], 81.35)
  expect_equal(res[2, "UCLM"], 84.44432)
  expect_equal(res[2, "LCLM"], 78.25568)

})


test_that("means41: t and probt work as expected.", {


  datp <- read.table(header = TRUE, text = '
  WBEFORE WAFTER
  200 190
  175 154
  188 176
  198 193
  197 198
  310 240
  245 204
  202 178')

  datp$WLOSS <- datp$WAFTER - datp$WBEFORE

  res <- proc_means(datp, var = WLOSS,
                    stats = v(n, mean, t, prt, probt, df),
                    titles = c("Paired t-test example"))

  expect_equal(res[1, "N"], 8)
  expect_equal(res[1, "MEAN"], -22.75)
  expect_equal(res[1, "T"], -2.7884739)
  expect_equal(res[1, "PRT"], 0.026967454)
  expect_equal(res[1, "PROBT"], 0.026967454)
  expect_equal(res[1, "DF"], 7)

})


test_that("means42: default statistics work as expected.", {

  res <- proc_means(datm, var = v(Age, PresentScore, TasteScore, Layers),
                    titles = c("Test"))

  expect_equal(names(res), c("TYPE", "FREQ", "VAR", "N", "MEAN", "STD", "MIN", "MAX" ))

})


test_that("means43: various statistics work as expected.", {

  res <- proc_means(datm, var = v(Age, PresentScore, TasteScore, Layers),
                    stats = v(n, mean,  uss, vari, sum, nobs, cv, css),
                    titles = c("Test"))

  expect_equal(res[1, "N"], 20)
  expect_equal(res[1, "MEAN"], 40.2)
  expect_equal(res[1, "USS"], 36498)
  expect_equal(res[1, "VARI"], 219.8526316)
  expect_equal(res[1, "SUM"], 804)
  expect_equal(res[1, "CV"], 36.8841501)
  expect_equal(res[1, "CSS"], 4177.2)
})

test_that("means44: skewness and kurtosis work as expected.", {


  res <- proc_means(datm, var = v(Age, PresentScore, TasteScore, Layers),
                    stats = v(n, mean,  skew, kurt),
                    titles = c("Test"),
                    options = c(maxdec = 8))


  expect_equal(res[2, "N"], 20)
  expect_equal(res[2, "MEAN"], 76.15)
  expect_equal(res[2, "SKEW"], -0.36762986)
  expect_equal(res[2, "KURT"], -0.30448351)




})


test_that("means45: default variables work as expected.", {

  res <- proc_means(datm,
                    titles = c("Test"))

  expect_equal(names(res), c("TYPE", "FREQ", "VAR", "N", "MEAN", "STD", "MIN", "MAX" ))
  expect_equal(as.character(res$VAR), c("Age", "PresentScore", "TasteScore", "Layers"))

})

test_that("means46: Multiple class vars match SAS.", {

  datmr <- read.table(header = TRUE, text = '
    LastName  Age PresentScore TasteScore Flavor Layers Region
    Orlando     27 93 80  Vanilla    1 1
    Ramey       32 84 72  Rum        2 1
    Goldston    46 68 75  Vanilla    1 1
    Roe         38 79 73  Vanilla    2 1
    Larsen      23 77 84  Chocolate  3 1
    Davis       51 86 91  Spice      3 1
    Strickland  19 82 79  Chocolate  1 1
    Nguyen      57 77 84  Vanilla    3 1
    Hildenbrand 33 81 83  Chocolate  1 1
    Byron       62 72 87  Vanilla    2 1
    Sanders     26 56 79  Chocolate  1 2
    Jaeger      43 66 74  NA         1 2
    Davis       28 69 75  Chocolate  2 2
    Conrad      69 85 94  Vanilla    1 2
    Walters     55 67 72  Chocolate  2 2
    Rossburger  28 78 81  Spice      2 2
    Matthew     42 81 92  Chocolate  2 2
    Becker      36 62 83  Spice      2 2
    Anderson    27 87 85  Chocolate  1 2
    Merritt     62 73 84  Chocolate  1 2
    ')



  res <- proc_means(datmr,
                    var = c("Age", "PresentScore", "TasteScore"),
                    stats = c("n", "min", "max", "mean", "std"),
                    output = c("out", "report", "long"),
                    class = c("Region", "Layers"),
                    titles = "My first title for Means",
                    options = c(maxdec = 4))

  res


  expect_equal(nrow(res$out), 55)
  expect_equal(ncol(res$out), 8)

  expect_equal(nrow(res$report), 15)
  expect_equal(ncol(res$report), 8)



  res <- proc_means(datmr,
                    var = c("Age", "PresentScore", "TasteScore"),
                    stats = c("n", "min", "max", "mean", "std"),
                    output = c("out", "report", "long"),
                    class = c("Region", "Layers", "Flavor"),
                    titles = "My first title for Means",
                    options = c( maxdec = 4))

  res


  expect_equal(nrow(res$out), 210)
  expect_equal(ncol(res$out), 9)

  expect_equal(nrow(res$report), 33)
  expect_equal(ncol(res$report), 9)


})


test_that("means47: check alternate keywords work.", {



  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    stats = c("n", "mean", "stddev", "skewness", "kurtosis"),
                    titles = "My first title for Means",
                    options = v(notype, nonobs))

  res


  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 6)

  expect_equal(res[2, "STD"], 6.6115926897)
  expect_equal(res[1, "SKEW"], -0.36762986)
  expect_equal(res[1, "KURT"], -0.30448351)

})


test_that("means48: nway option works.", {



  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    titles = "My first title for Means",
                    class = c("Layers", "Flavor"),
                    options = c("nway"))

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 10)
  expect_equal(nrow(res), 18)



  res <- proc_means(datm, var = c("PresentScore", "TasteScore"),
                    titles = "My first title for Means",
                    class = c("Layers", "Flavor"),
                    options = c("nway", "notype"))

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 9)
  expect_equal(nrow(res), 18)

})

test_that("means49: factors work as expected.", {

  datmr <- read.table(header = TRUE, text = '
    LastName  Age PresentScore TasteScore Flavor Layers Region
    Orlando     27 93 80  Vanilla    1 1
    Ramey       32 84 72  Rum        2 1
    Goldston    46 68 75  Vanilla    1 1
    Roe         38 79 73  Vanilla    2 1
    Larsen      23 77 84  Chocolate  3 1
    Davis       51 86 91  Spice      3 1
    Strickland  19 82 79  Chocolate  1 1
    Nguyen      57 77 84  Vanilla    3 1
    Hildenbrand 33 81 83  Chocolate  1 1
    Byron       62 72 87  Vanilla    2 1
    Sanders     26 56 79  Chocolate  1 2
    Jaeger      43 66 74  NA         1 2
    Davis       28 69 75  Chocolate  2 2
    Conrad      69 85 94  Vanilla    1 2
    Walters     55 67 72  Chocolate  2 2
    Rossburger  28 78 81  Spice      2 2
    Matthew     42 81 92  Chocolate  2 2
    Becker      36 62 83  Spice      2 2
    Anderson    27 87 85  Chocolate  1 2
    Merritt     62 73 84  Chocolate  1 2
    ')


  datmr$Flavor <- factor(datmr$Flavor, levels = c("Rum", "Vanilla", "Chocolate", "Spice"))
  suppressWarnings(attr(datmr[["Layers"]], "levels") <- c("Three", "Two", "One"))
  class(datmr$Layers) <- "factor"
  suppressWarnings(attr(datmr[["Region"]], "levels") <- c("South", "North"))
  class(datmr$Region) <- "factor"


  res <- proc_means(datmr,
                    var = c("Age", "PresentScore", "TasteScore"),
                    stats = c("n", "min", "max", "mean", "std"),
                    output = c("out", "report", "long"),
                    by = "Region",
                   #  class = c("Layers", "Flavor"),
                    titles = "My first title for Means",
                    options = c(maxdec = 4))

  res


  expect_equal(nrow(res$out), 10)
  expect_equal(ncol(res$out), 7)

  expect_equal(nrow(res$report[[1]]), 3)
  expect_equal(ncol(res$report[[1]]), 6)



  res <- proc_means(datmr,
                    var = c("Age", "PresentScore", "TasteScore"),
                    stats = c("n", "min", "max", "mean", "std"),
                    output = c("out", "report", "long"),
                    class = c("Region", "Layers", "Flavor"),
                    titles = "My first title for Means",
                    options = c(maxdec = 4))

  res


  expect_equal(nrow(res$out), 210)
  expect_equal(ncol(res$out), 9)

  expect_equal(nrow(res$report), 33)
  expect_equal(ncol(res$report), 9)


  res <- proc_means(datmr,
                    var = c("Age", "PresentScore", "TasteScore"),
                    stats = c("n", "min", "max", "mean", "std"),
                    output = c("out", "report", "long"),
                    by = "Region",
                    class = c("Layers", "Flavor"),
                    titles = "My first title for Means",
                    options = c(maxdec = 4))

  res


  expect_equal(nrow(res$out), 125)
  expect_equal(ncol(res$out), 9)

  expect_equal(nrow(res$report[[1]]), 21)
  expect_equal(ncol(res$report[[1]]), 8)

})


test_that("means50: two by as factors work.", {

  datsp <- datm
  datsp$Flavor <- factor(datsp$Flavor, levels = c("Vanilla", "Chocolate", "Spice", "Rum"))
  datsp$Layers <- factor(datsp$Layers, levels = c("3", "2", "1"))


  res1 <- proc_means(datsp, var = c("Age", "PresentScore", "TasteScore"),
                    output = out,
                    by = c("Layers", "Flavor"))

  res1


  expect_equal(nrow(res1), 27)
  expect_equal(ncol(res1), 10)

  res1 <- proc_means(datsp, var = c("Age", "PresentScore", "TasteScore"),
                     output = out,
                     by = c("Layers"),
                     class = "Flavor")

  res1

  #labels(res1)


  expect_equal(nrow(res1), 36)
  expect_equal(ncol(res1), 10)

})


test_that("means51: other statistics with by works.", {

  datsp <- datm
  datsp$Layers[1] <- 3

  res1 <- proc_means(datsp, var = c("Age", "PresentScore", "TasteScore"),
                     stats = c("kurtosis", "skew", "cv", "clm"),
                     output = out,
                     by = c("Layers"),
                     options = c(maxdec = 8)
                     )

  res1


  expect_equal(nrow(res1), 9)
  expect_equal(ncol(res1), 9)
  expect_equal(res1[1, 5], -0.95194732)
  expect_equal(res1[1, 6], 0.55802418)
  expect_equal(res1[1, 7], 43.85482852)
  expect_equal(res1[1, 8], 25.73043112)
  expect_equal(res1[1, 9], 55.51956888)


  # Not enough observations
  expect_error(proc_means(datsp, var = c("Age", "PresentScore", "TasteScore"),
                     stats = c("kurt", "skew", "cv", "clm"),
                     output = out,
                     by = c("Layers"),
                     class = "Flavor"))



})



test_that("means52: factor sort on class works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n",  "mean", "std", "min", "max")

  datsp <- datm

  datsp$Flavor <- factor(datsp$Flavor, levels = c("Vanilla", "Chocolate", "Spice", "Rum"))
  datsp$Layers <- factor(datsp$Layers, levels = c("4", "3", "2", "1"))

  res <- proc_means(datsp, var = var1,
                    stats = var2,
                    #by = "Layers",
                    class = c("Layers"),
                    titles = "My first title for Means",
                    output = c("out", "report", "long"))

  res

  expect_equal(nrow(res$report), 9)
  expect_equal(ncol(res$report), 7)
  expect_equal(as.character(unique(res$report$CLASS)), c("3", "2", "1"))

  expect_equal(nrow(res$out), 20)
  expect_equal(ncol(res$out), 7)
  expect_equal(as.character(unique(res$out$CLASS)), c(NA, "3", "2", "1"))

})

test_that("means53: factor sort on 2 classes works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n",  "mean", "std", "min", "max")

  datsp <- datm

  datsp$Flavor <- factor(datsp$Flavor, levels = c("Vanilla", "Chocolate", "Spice", "Rum"))
  datsp$Layers <- factor(datsp$Layers, levels = c("4", "3", "2", "1"))

  res <- proc_means(datsp, var = var1,
                    stats = var2,
                    #by = "Layers",
                    class = c("Layers", "Flavor"),
                    titles = "My first title for Means",
                    output = c("out", "report", "long"))

  res

  expect_equal(nrow(res$report), 27)
  expect_equal(ncol(res$report), 8)
  expect_equal(as.character(unique(res$report$CLASS1)), c("3", "2", "1"))
  expect_equal(as.character(unique(res$report$CLASS2)),  c("Vanilla", "Chocolate", "Spice", "Rum"))

  expect_equal(nrow(res$out), 85)
  expect_equal(ncol(res$out), 8)
  expect_equal(as.character(unique(res$out$CLASS1)), c(NA, "3", "2", "1"))
  expect_equal(as.character(unique(res$out$CLASS2)),  c(NA, "Vanilla", "Chocolate", "Spice", "Rum"))

})


test_that("means54: completetypes on one factor works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n",  "mean", "std", "min", "max")

  datsp <- datm
  datsp$Age[[1]] <- NA

  datsp$Flavor <- factor(datsp$Flavor, levels = c("Vanilla", "Chocolate", "Spice", "Rum", "Butter"))
  datsp$Layers <- factor(datsp$Layers, levels = c("4", "3", "2", "1"))

  res <- proc_means(datsp, var = var1,
                    stats = var2,
                    #by = "Layers",
                    class = c("Layers"),
                    titles = "My first title for Means",
                    options = "completetypes",
                    output = c("out", "report"))

  res

  res1 <- proc_means(datsp, var = var1,
                    stats = var2,
                    #by = "Layers",
                    class = c("Flavor"),
                    titles = "My first title for Means",
                    options = "completetypes",
                    output = c("out", "report"))

  res1


  expect_equal(nrow(res$out), 15)
  expect_equal(ncol(res$out), 9)
  expect_equal("4" %in% res$out$CLASS, TRUE)

  expect_equal(nrow(res1$out), 18)
  expect_equal(ncol(res1$out), 9)
  expect_equal("Butter" %in% res1$out$CLASS, TRUE)

  expect_equal("4" %in% res$report$CLASS, TRUE)
  expect_equal("Butter" %in% res1$report$CLASS, TRUE)

})




test_that("means29: completetypes option on two factors works.", {

  var1 <- c("Age", "PresentScore", "TasteScore")
  var2 <- c("n",  "mean", "std", "min", "max")

  datsp <- datm

  datsp$Flavor <- factor(datsp$Flavor, levels = c("Vanilla", "Chocolate", "Spice", "Rum", "Butter"))
  datsp$Layers <- factor(datsp$Layers, levels = c("4", "3", "2", "1"))


  res2 <- proc_means(datsp, var = var1,
                    stats = var2,
                    #by = "Layers",
                    class = c("Layers", "Flavor"),
                    titles = "My first title for Means",
                    options = "completetypes",
                    output = c("out", "report"))

  res2

  expect_equal(nrow(res2$out), 90)
  expect_equal(ncol(res2$out), 10)
  expect_equal("Butter" %in% res2$out$CLASS2, TRUE)
  expect_equal("4" %in% res2$out$CLASS1, TRUE)
  expect_equal("Butter" %in% res2$report$CLASS2, TRUE)
  expect_equal("4" %in% res2$report$CLASS1, TRUE)


})





# res3 <- proc_means(datm, var = v(Age, PresentScore, TasteScore, Layers),
#            stats = c("css", "cv", "lclm", "mode",  "nobs", "stddev"),
#            options = v(maxdec = 4),
#            titles = "Summary of Presentation and Taste Scores")
#
# proc_means(datm, var = v(Age, PresentScore, TasteScore, Layers),
#            stats = c("p1", "p5", "p10", "p20", "p25", "p30", "p40", "p50"),
#            options = v(maxdec = 4),
#            titles = "Summary of Presentation and Taste Scores")
#
# proc_means(datm, var = v(Age, PresentScore, TasteScore, Layers),
#            stats = c("p60", "p70", "p75", "p80", "p90", "p95", "p99"),
#            options = v(maxdec = 4),
#            titles = "Summary of Presentation and Taste Scores")
# proc_means(datm, var = v(Age, PresentScore, TasteScore, Layers),
#            stats = c("q1", "q3", "qrange", "range", "sum", "uclm", "vari"),
#            options = v(maxdec = 4),
#            titles = "Summary of Presentation and Taste Scores")



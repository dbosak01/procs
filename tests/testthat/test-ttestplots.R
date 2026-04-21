base_path <-  file.path(getwd(), "tests/testthat")
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
subject_id before after region
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
  expect_equal(res$type, c("default"))

  # Assign something
  res <- ttestplot(type = c("agreement", "boxplot", "histogram", 'interval', 'profiles'))

  res

  expect_equal("ttestplot" %in% class(res), TRUE)
  expect_equal(res$type, c("agreement", "boxplot", "histogram", 'interval', 'profiles'))

})


test_that("ttestplot2: basic plot types.", {

  # Histogram - OK
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("histogram"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # Histogram - OK
  res <- proc_ttest(cls,
                    var = c("Weight"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("histogram"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Boxplot - OK
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Interval - OK
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("interval"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # QQPlot - Line not correct.  Dots look good.
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("qqplot"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


})



# Nice!
test_that("ttestplot3: multiple plot types.", {

  # Combined
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot(c("histogram", "boxplot", "interval", "qqplot")))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


})

# OK
test_that("ttestplot4: summary plot type.", {

  # Summary
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("summary"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)



  # Summary - 90% Confidence
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.1),
                    output = report,
                    plots = ttestplot("summary"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Summary "unpacked"
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("summary", panel = FALSE))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[4]][[2]]), TRUE)



})

# OK
test_that("ttestplot5: summary plot type by groups.", {

  # Summary
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    by = Sex,
                    plots = ttestplot("summary"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 2)
  expect_equal("plot_spec" %in% class(res[[1]][[4]][[1]]), TRUE)


})


# OK
test_that("ttestplot6: multiple variables.", {

  # Summary
  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                 #   by = Sex,
                    plots = ttestplot("summary"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 8)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[8]][[1]]), TRUE)


  # Multiple vars with by
  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    by = Sex,
                    plots = ttestplot("summary"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 2)
  expect_equal("plot_spec" %in% class(res[[1]][[4]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[2]][[4]][[1]]), TRUE)


})



# OK
test_that("ttestplot7: class analysis.", {

  # Boxplot with class
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    class = Sex,
                    plots = ttestplot("boxplot"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Interval with class - Mean of Difference
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.1),
                    output = report,
                    class = Sex,
                    plots = ttestplot("interval"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Histogram with class
  res <- proc_ttest(cls,
                    var = c("Weight"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    class = Sex,
                    plots = ttestplot("histogram"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Boxplot with class
  res <- proc_ttest(cls,
                    var = c("Weight"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    class = Sex,
                    plots = ttestplot("histogram"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # QQPlot with class
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    class = Sex,
                    plots = ttestplot("qqplot"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Summary with class
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    class = Sex,
                    plots = ttestplot("summary"))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})

test_that("ttestplot8: boxplot outliers are working as expected.", {


  dt1 <- cls[cls$Sex == "F", ]

  # Single chart outliers
  res <- proc_ttest(dt1,
                    var = c("Weight"),
                    options = c("h0" = 65, "alpha" = 0.1),
                    output = report,
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)



  # Double chart outliers
  res <- proc_ttest(cls,
                    var = c("Weight"),
                    options = c("h0" = 65, "alpha" = 0.1),
                    class = "Sex",
                    output = report,
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

})

test_that("ttestplot8: edge cases.", {

  # Boxplot - 90% confidence, and possibly outliers
  res <- proc_ttest(cls,
                    var = c("Weight"),
                    options = c("h0" = 65, "alpha" = 0.1),
                    output = report,
                    plots = ttestplot("boxplot", showh0 = TRUE))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)



  # Summary2 scale off on this data - OK Now
  res <- proc_ttest(sleep,
                    var = extra,
                    class = group,
                    output = report,
                    plots = ttestplot("summary"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)



})


test_that("ttestplot9: Paired analysis.", {


  # Profiles
  res <- proc_ttest(cls,
                    paired = "Weight * Height",
                    output = report,
                    plots = ttestplot("profiles"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Agreement
  res <- proc_ttest(cls,
                    paired = "Weight * Height",
                    output = report,
                    plots = ttestplot("agreement"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Profiles - with by variables and multiple plot types
  res <- proc_ttest(cls,
                    paired = "Weight * Height",
                    output = report,
                    by = "Sex",
                    plots = ttestplot(c("profiles", "agreement")))

  expect_equal(length(res), 2)
  expect_equal("plot_spec" %in% class(res[[1]][[4]][[1]]), TRUE)


  # Summary
  res <- proc_ttest(cls,
                    paired = "Weight * Height",
                    output = report,
                    plots = ttestplot("summary"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Histogram
  res <- proc_ttest(cls,
                    paired = "Weight * Height",
                    output = report,
                    plots = ttestplot("histogram"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # Boxplot
  res <- proc_ttest(cls,
                    paired = "Weight * Height",
                    output = report,
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # Interval
  res <- proc_ttest(cls,
                    paired = "Weight * Height",
                    output = report,
                    plots = ttestplot("interval"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # QQPlot
  res <- proc_ttest(cls,
                    paired = "Weight * Height",
                    output = report,
                    plots = ttestplot("qqplot"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

})

test_that("ttestplot10: plots = TRUE.", {


  # One Sample
  res <- proc_ttest(cls,
                    var = "Weight",
                    options = c(h0 = 80),
                    output = report,
                    plots = TRUE)

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[4]][[2]]), TRUE)

  # Two Samples
  res <- proc_ttest(cls,
                    var = "Weight",
                    class = "Sex",
                    output = report,
                    plots = TRUE)

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)
  expect_equal("plot_spec" %in% class(res[[5]][[2]]), TRUE)


  # Paired
  res <- proc_ttest(cls,
                    paired = "Weight * Height",
                    output = report,
                    plots = TRUE)

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)



})

test_that("ttestplot11: plots = 'all'", {


  # One Sample
  res <- proc_ttest(cls,
                    var = "Weight",
                    options = c(h0 = 80),
                    output = report,
                    plots = "all")

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)
  expect_equal(length(res[[4]]), 5)

  # Two Samples
  res <- proc_ttest(cls,
                    var = "Weight",
                    class = "Sex",
                    output = report,
                    plots =  "all")

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)
  expect_equal(length(res[[5]]), 5)


  # Paired
  res <- proc_ttest(cls,
                    paired = "Height * Weight",
                    output = report,
                    plots = "all")

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)
  expect_equal(length(res[[4]]), 7)


})

test_that("ttestplot12: Documentation examples", {

  res <- proc_ttest(paird, paired = "before * after",
                    output = report,
                    plots = TRUE)

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)
  expect_equal(length(res[[4]]), 4)


})

# Also check that x scale adjusts if h0 is outsize confidence intervals
test_that("ttestplot13: showh0 parameter", {

  # Boxplot1
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 75, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("boxplot", showh0 = TRUE))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Interval 1
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 63, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("interval", showh0 = TRUE))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)



  # Summary 1
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 63, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("summary", showh0 = TRUE))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)



  # Interval2
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 0, "alpha" = 0.05),
                    class = "Sex",
                    output = report,
                    plots = ttestplot("interval", showh0 = TRUE))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

})


# Work on this
test_that("ttestplot14: troublesome plots match SAS", {

  # qqplot1 - Got it!
  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("qqplot"))


  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # qqplot2 - Hurray!
  res <- proc_ttest(cls,
             var = c("Height"),
             class = "Sex",
             output = report,
             plot = TRUE,
             options = c("alpha" = 0.1))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # histogram  - Good enough
  res <- proc_ttest(cls,
                    var = c("Age"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report,
                    plots = ttestplot("histogram"))


  # Boxplot1 - needs outlier.  Got it!
  res <- proc_ttest(cls,
                    paired = "Height * Weight",
                    output = report,
                    plot = "boxplot")

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Boxplot2 - needs outlier.  Got it!
  res <- proc_ttest(cls,
                    var = "Weight",
                    class = "Sex",
                    output = report,
                    plot = "boxplot")

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Different data with class - Fixed
  res <- proc_ttest(sleep,
                    var = "extra",
                    class = "group",
                    output = "report",
                    plots = c("summary", "histogram", "boxplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

})

# Good
test_that("ttestplot15: Label and ID parameters work as expected.", {

  # Boxplot1 - label FALSE
  res <- proc_ttest(cls,
                    paired = "Height * Weight",
                    output = report,
                    plot = ttestplot(c("summary", "boxplot"),
                                     label = FALSE))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Boxplot1 - label TRUE
  res <- proc_ttest(cls,
                    paired = "Height * Weight",
                    output = report,
                    plot = ttestplot(c("summary", "boxplot"),
                                     label = TRUE))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Boxplot1 - id value
  res <- proc_ttest(cls,
                    paired = "Height * Weight",
                    output = report,
                    plot = ttestplot(c("summary", "boxplot"),
                                     label = TRUE, id = "Name"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Boxplot2 - label FALSE
  res <- proc_ttest(cls,
                    var = "Weight",
                    class = "Sex",
                    output = report,
                    plot = ttestplot(c("summary", "boxplot"),
                                     label = FALSE))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Boxplot2 - label TRUE
  res <- proc_ttest(cls,
                    var = "Weight",
                    class = "Sex",
                    output = report,
                    plot = ttestplot(c("summary", "boxplot"),
                                     label = TRUE))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Boxplot2 - id value
  res <- proc_ttest(cls,
                    var = "Weight",
                    class = "Sex",
                    output = report,
                    plot = ttestplot(c("summary", "boxplot"),
                                     label = TRUE, id = "Name"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


})


test_that("ttestplot16: plots with freq parameter.", {

  # One sample with freq
  cls_freq <- cls
  cls_freq$FreqVar <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)

  res <- proc_ttest(cls_freq, var = "Height",
                    freq = "FreqVar",
                    options = c(h0 = 65),
                    output = "report",
                    plots = ttestplot("summary"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # Two sample (class) with freq
  res <- proc_ttest(cls_freq, var = "Height",
                    class = "Sex",
                    freq = "FreqVar",
                    output = "report",
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Paired with freq
  paird_freq <- paird
  paird_freq$FreqVar <- c(1,2,3,1,2,3,1,2,3,1)

  res <- proc_ttest(paird_freq,
                    paired = "before * after",
                    freq = "FreqVar",
                    output = "report",
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)
})


test_that("ttestplot17: plots with weight parameter.", {

  # One sample with weight
  cls_wgt <- cls
  cls_wgt$WgtVar <- c(1.5, 2.0, 0.8, 1.2, 1.0, 1.5, 2.0, 0.8, 1.2, 1.0,
                      1.5, 2.0, 0.8, 1.2, 1.0, 1.5, 2.0, 0.8, 1.2)

  res <- proc_ttest(cls_wgt, var = "Height",
                    weight = "WgtVar",
                    options = c(h0 = 65),
                    output = "report",
                    plots = ttestplot("summary"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Two sample (class) with weight
  res <- proc_ttest(cls_wgt, var = "Height",
                    class = "Sex",
                    weight = "WgtVar",
                    output = "report",
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)


  # Paired with weight
  paird_wgt <- paird
  paird_wgt$WgtVar <- c(1.5, 2.0, 0.8, 1.2, 1.0, 1.5, 2.0, 0.8, 1.2, 1.0)

  res <- proc_ttest(paird_wgt,
                    paired = "before * after",
                    weight = "WgtVar",
                    output = "report",
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)


  # Multiple plot types with weight
  res <- proc_ttest(cls_wgt, var = "Height",
                    weight = "WgtVar",
                    options = c(h0 = 65),
                    output = "report",
                    plots = ttestplot(c("summary", "histogram", "boxplot")))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

})
test_that("ttestplot18: plots with sides parameter (one-sample).", {

  # Summary plot with sides = 'U'
  res <- proc_ttest(cls, var = "Height",
                    options = c(h0 = 65, sides = "U"),
                    output = "report",
                    plots = ttestplot("summary"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # Summary plot with sides = 'L'
  res <- proc_ttest(cls, var = "Height",
                    options = c(h0 = 65, sides = "L"),
                    output = "report",
                    plots = ttestplot("summary"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # Interval plot with sides option
  res <- proc_ttest(cls, var = "Height",
                    options = c(h0 = 65, sides = "U"),
                    output = "report",
                    plots = ttestplot("interval"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)
})


test_that("ttestplot19: plots with sides parameter (two-sample).", {

  # Summary plot with sides = 'U' and class
  res <- proc_ttest(cls, var = "Height",
                    class = "Sex",
                    options = c(sides = "U"),
                    output = "report",
                    plots = ttestplot("summary"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Boxplot with sides = 'L' and class
  res <- proc_ttest(cls, var = "Height",
                    class = "Sex",
                    options = c(sides = "L"),
                    output = "report",
                    plots = ttestplot("boxplot"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)

  # Interval plot with sides and class
  res <- proc_ttest(cls, var = "Height",
                    class = "Sex",
                    options = c(sides = "U", alpha = 0.1),
                    output = "report",
                    plots = ttestplot("interval"))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)
})


test_that("ttestplot20: plots with sides parameter (paired).", {

  # Profiles plot with sides = 'U'
  res <- proc_ttest(paird, paired = "before * after",
                    options = c(sides = "U"),
                    output = "report",
                    plots = ttestplot("profiles"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # Agreement plot with sides = 'L'
  res <- proc_ttest(paird, paired = "before * after",
                    options = c(sides = "L"),
                    output = "report",
                    plots = ttestplot("agreement"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # Summary with paired and sides
  res <- proc_ttest(paird, paired = "before * after",
                    options = c(sides = "U"),
                    output = "report",
                    plots = ttestplot("summary"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)
})


test_that("ttestplot21: plots with freq and sides together.", {

  cls_freq <- cls
  cls_freq$FreqVar <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)

  # One sample with freq and sides
  res <- proc_ttest(cls_freq, var = "Height",
                    freq = "FreqVar",
                    options = c(h0 = 65, sides = "U"),
                    output = "report",
                    plots = ttestplot("summary"))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # Two sample with freq and sides
  res <- proc_ttest(cls_freq, var = "Height",
                    class = "Sex",
                    freq = "FreqVar",
                    options = c(sides = "L"),
                    output = "report",
                    plots = ttestplot(c("summary", "boxplot")))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)
})


test_that("ttestplot22: multiple plot types with freq and sides.", {

  cls_freq <- cls
  cls_freq$FreqVar <- c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1)

  # Multiple plots with freq
  res <- proc_ttest(cls_freq, var = "Height",
                    freq = "FreqVar",
                    options = c(h0 = 65),
                    output = "report",
                    plots = ttestplot(c("summary", "histogram", "boxplot")))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # All plots with sides
  res <- proc_ttest(cls, var = "Height",
                    options = c(h0 = 65, sides = "U"),
                    output = "report",
                    plots = "all")

  expect_equal(length(res), 4)
  expect_equal(length(res[[4]]), 5)
})


test_that("ttestplot23: plots with showh0 and sides together.", {

  # showh0 with sides = 'U'
  res <- proc_ttest(cls, var = "Height",
                    options = c(h0 = 70, sides = "U"),
                    output = "report",
                    plots = ttestplot("summary", showh0 = TRUE))

  expect_equal(length(res), 4)
  expect_equal("plot_spec" %in% class(res[[4]][[1]]), TRUE)

  # showh0 with sides = 'L' and class
  res <- proc_ttest(cls, var = "Height",
                    class = "Sex",
                    options = c(h0 = 60, sides = "L"),
                    output = "report",
                    plots = ttestplot("interval", showh0 = TRUE))

  expect_equal(length(res), 5)
  expect_equal("plot_spec" %in% class(res[[5]][[1]]), TRUE)
})

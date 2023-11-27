base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."


cls <- read.table(header = TRUE, text = '
Name Sex Age Height Weight
Alfred   M  14   69.0  112.5
Alice   F  13   56.5   84.0
Barbara   F  13   65.3   98.0
Carol   F  14   62.8  102.5
Henry   M  14   63.5  102.5
James   M  12   57.3   83.0
Jane   F  12   59.8   84.5
Janet   F  15   62.5  112.5
Jeffrey   M  13   62.5   84.0
John   M  12   59.0   99.5
Joyce   F  11   51.3   50.5
Judy   F  14   64.3   90.0
Louise   F  12   56.3   77.0
Mary   F  15   66.5  112.0
Philip   M  16   72.0  150.0
Robert   M  12   64.8  128.0
Ronald   M  15   67.0  133.0
Thomas   M  11   57.5   85.0
William   M  15   66.5  112.0')

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
#options("procs.print" = NULL)

dev <- FALSE

test_that("ttest1: Get Output Specs works as expected.", {




  res <- get_output_specs_ttest(mtcars, var = "mpg", paired = NULL, class = NULL,
                                opts = c("h0" = 20), output = "out" )



  expect_equal(is.null(res), FALSE)
  expect_equal("..var" %in% names(res$data), TRUE)
  expect_equal(length(res$outreq) > 0, TRUE)

})

test_that("ttest2: Simple proc_ttest one variable works.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)

  expect_equal(as.numeric(res$Statistics$MEAN), 62.336842)
  expect_equal(as.numeric(res$Statistics$STD), 5.1270752)
  expect_equal(as.numeric(res$Statistics$STDERR), 1.17623173)
  expect_equal(as.numeric(res$ConfLimits$LCLM), 59.8656709)
  expect_equal(as.numeric(res$ConfLimits$UCLM), 64.808013)
  expect_equal(as.numeric(res$TTests$DF), 18)
  expect_equal(as.numeric(res$TTests$T), -2.2641439)
  expect_equal(as.numeric(res$TTests$PROBT), 0.036152218)

})

#
# test_that("ttest3: Simple proc_ttest with class works.", {
#
#   # proc ttest data=sashelp.class alpha=0.05;
#   # class sex /* Grouping Variable */;
#   # var height;
#   # run;
#
#
#
#   res <- proc_ttest(cls,
#                     class = "Sex",
#                     options = c("alpha" = 0.05),
#                     titles = "My first TTest Table")
#
#   res
#
#   expect_equal(is.null(res), FALSE)
#
# })


test_that("ttest4: Simple proc_ttest with paired variables works.", {

  # data PairedData;
  # input subject_id before_measure after_measure;
  # datalines;
  # 1 12 15
  # 2 14 16
  # 3 10 11
  # 4 15 18
  # 5 18 20
  # 6 20 22
  # 7 11 12
  # 8 13 14
  # 9 16 17
  # 10 9 13
  # ;
  # run;
  #
  # proc ttest data=PairedData;
  # paired before_measure * after_measure;
  # run;

  res <- proc_ttest(paird,
                    paired = c("before_measure * after_measure"),
                    titles = "My first TTest Table")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)

  expect_equal(as.numeric(res$Statistics$N), 10)
  expect_equal(as.numeric(res$Statistics$MEAN), -2)
  expect_equal(as.numeric(res$Statistics$STD), 1.05409255)
  expect_equal(as.numeric(res$Statistics$STDERR), 0.33333333)
  expect_equal(as.numeric(res$Statistics$MIN), -4)
  expect_equal(as.numeric(res$Statistics$MAX), -1)
  expect_equal(as.numeric(res$ConfLimits$UCLM), -1.2459476)
  expect_equal(as.numeric(res$ConfLimits$LCLM), -2.7540524)
  expect_equal(as.numeric(res$TTests$DF), 9)
  expect_equal(as.numeric(res$TTests$T), -6)
  expect_equal(as.numeric(res$TTests$PROBT), .000202499322)
})


test_that("ttest5: Simple proc_ttest weight var works.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   run;


  res <- proc_ttest(cls,
                    var = c("Weight"),
                    options = c("h0" = 100, "alpha" = 0.1),
                    titles = "My first Frequency Table")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)

  expect_equal(as.numeric(res$Statistics$MEAN), 100.026316)
  expect_equal(as.numeric(res$Statistics$STD), 22.7739335)
  expect_equal(as.numeric(res$Statistics$STDERR), 5.2246987)
  expect_equal(as.numeric(res$ConfLimits$LCLM), 90.966356)
  expect_equal(as.numeric(res$ConfLimits$UCLM), 109.086276)
  expect_equal(as.numeric(res$TTests$DF), 18)
  expect_equal(as.numeric(res$TTests$T), 0.00503680521)
  expect_equal(as.numeric(res$TTests$PROBT), 0.99603663)

})




test_that("ttest6: Simple proc_ttest log with one variable works.", {

  if (dev) {
    # proc ttest data=sashelp.class
    # h0=65 /* Null hypothesis mean of 65 */
    #   alpha=0.05;  /* Significance level of 0.05 */
    #   var height; /* Specify the variable to test */
    #   run;

    library(logr)

    options("logr.output" = TRUE)
    options("logr.autolog" = TRUE)

    lf <- log_open(file.path(base_path, "test.log"))

    res <- proc_ttest(cls,
                      var = c("Height"),
                      options = c("h0" = 65, "alpha" = 0.05),
                      titles = "My first Frequency Table")

    res

    log_close()

    expect_equal(is.null(res), FALSE)
    expect_equal(length(res), 3)
    expect_equal(file.exists(lf), TRUE)

    #file.show(lf)

    options("logr.output" = FALSE)
    options("logr.autolog" = FALSE)

  } else {

   expect_equal(TRUE, TRUE)
  }

})

test_that("ttest7: Simple proc_ttest with by and noprint works.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   by sex;
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height"), by = "Sex",
                    options = c("h0" = 65, "alpha" = 0.05, "noprint"),
                    titles = "My first Frequency Table")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)

  expect_equal(as.numeric(res$Statistics$MEAN[1]), 60.5888888888889)
  expect_equal(as.numeric(res$Statistics$STD[1]), 5.0183275)
  expect_equal(as.numeric(res$Statistics$STDERR[1]), 1.67277584)
  expect_equal(as.numeric(res$ConfLimits$LCLM[1]), 56.7314609)
  expect_equal(as.numeric(res$ConfLimits$UCLM[1]), 64.446317)
  expect_equal(as.numeric(res$TTests$DF[1]), 8)
  expect_equal(as.numeric(res$TTests$T[1]), -2.6370007)
  expect_equal(as.numeric(res$TTests$PROBT[1]), 0.02985198)

})


test_that("ttest8: Simple proc_ttest with report works.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   by sex;
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height"), output = "report",
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)

  # expect_equal(as.numeric(res$Statistics$MEAN[1]), 60.5888888888889)
  # expect_equal(as.numeric(res$Statistics$STD[1]), 5.0183275)
  # expect_equal(as.numeric(res$Statistics$STDERR[1]), 1.67277584)
  # expect_equal(as.numeric(res$ConfLimits$LCLM[1]), 56.7314609)
  # expect_equal(as.numeric(res$ConfLimits$UCLM[1]), 64.446317)
  # expect_equal(as.numeric(res$TTests$DF[1]), 8)
  # expect_equal(as.numeric(res$TTests$T[1]), -2.6370007)
  # expect_equal(as.numeric(res$TTests$PROBT[1]), 0.02985198)

})


test_that("ttest9: Simple proc_ttest with by and print works.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height"), by = "Sex",
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)

  # expect_equal(as.numeric(res$Statistics$MEAN), 62.336842)
  # expect_equal(as.numeric(res$Statistics$STD), 5.1270752)
  # expect_equal(as.numeric(res$Statistics$STDERR), 1.17623173)
  # expect_equal(as.numeric(res$ConfLimits$LCLM), 59.8656709)
  # expect_equal(as.numeric(res$ConfLimits$UCLM), 64.808013)
  # expect_equal(as.numeric(res$TTests$DF), 18)
  # expect_equal(as.numeric(res$TTests$T), -2.2641439)
  # expect_equal(as.numeric(res$TTests$PROBT), 0.036152218)

})



test_that("ttest10: Simple proc_ttest with report and by works noprint.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   by sex;
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height"), output = "report", by = "Sex",
                    options = c("h0" = 65, "alpha" = 0.05, "noprint"),
                    titles = "My first Frequency Table")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 2)

  # expect_equal(as.numeric(res$Statistics$MEAN[1]), 60.5888888888889)
  # expect_equal(as.numeric(res$Statistics$STD[1]), 5.0183275)
  # expect_equal(as.numeric(res$Statistics$STDERR[1]), 1.67277584)
  # expect_equal(as.numeric(res$ConfLimits$LCLM[1]), 56.7314609)
  # expect_equal(as.numeric(res$ConfLimits$UCLM[1]), 64.446317)
  # expect_equal(as.numeric(res$TTests$DF[1]), 8)
  # expect_equal(as.numeric(res$TTests$T[1]), -2.6370007)
  # expect_equal(as.numeric(res$TTests$PROBT[1]), 0.02985198)

})

test_that("ttest11: Simple proc_ttest with report and by works with print", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   by sex;
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height"), output = "report", by = "Sex",
                    options = c("h0" = 65, "alpha" = 0.05))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 2)

  # expect_equal(as.numeric(res$Statistics$MEAN[1]), 60.5888888888889)
  # expect_equal(as.numeric(res$Statistics$STD[1]), 5.0183275)
  # expect_equal(as.numeric(res$Statistics$STDERR[1]), 1.67277584)
  # expect_equal(as.numeric(res$ConfLimits$LCLM[1]), 56.7314609)
  # expect_equal(as.numeric(res$ConfLimits$UCLM[1]), 64.446317)
  # expect_equal(as.numeric(res$TTests$DF[1]), 8)
  # expect_equal(as.numeric(res$TTests$T[1]), -2.6370007)
  # expect_equal(as.numeric(res$TTests$PROBT[1]), 0.02985198)

})

test_that("ttest12: proc_ttest with paired and by variables works.", {

  # data PairedData;
  # input subject_id before_measure after_measure region $9;
  # datalines;
  # 1 12 15 A
  # 2 14 16 A
  # 3 10 11 A
  # 4 15 18 A
  # 5 18 20 A
  # 6 20 22 B
  # 7 11 12 B
  # 8 13 14 B
  # 9 16 17 B
  # 10 9 13 B
  # ;
  # run;
  #
  # proc ttest data=PairedData;
  # paired before_measure * after_measure;
  # run;

  res <- proc_ttest(paird,
                    paired = c("before_measure * after_measure"),
                    by = region)

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)

  expect_equal(as.numeric(res$Statistics$N[2]), 5)
  expect_equal(as.numeric(res$Statistics$MEAN[2]), -1.8)
  expect_equal(as.numeric(res$Statistics$STD[2]), 1.303840481)
  expect_equal(as.numeric(res$Statistics$STDERR[2]),  0.583095189)
  expect_equal(as.numeric(res$Statistics$MIN[2]), -4)
  expect_equal(as.numeric(res$Statistics$MAX[2]), -1)
  expect_equal(as.numeric(res$ConfLimits$UCLM[2]), -0.181068215)
  expect_equal(as.numeric(res$ConfLimits$LCLM[2]), -3.41893178)
  expect_equal(as.numeric(res$TTests$DF[2]), 4)
  expect_equal(as.numeric(res$TTests$T[2]), -3.08697453)
  expect_equal(as.numeric(res$TTests$PROBT[2]), 0.03668198940)
})

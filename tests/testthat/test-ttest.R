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
  expect_equal("..mpg" %in% names(res$data), TRUE)
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


test_that("ttest3: Simple proc_ttest with class works.", {

  # proc ttest data=sashelp.class alpha=0.05;
  # class sex /* Grouping Variable */;
  # var height;
  # run;

  res <- proc_ttest(cls,
                    var = "Height",
                    class = "Sex")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)

})


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
                    var = c("Height"), by = c("Sex"),
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


test_that("ttest13: get_class_ttest with paired and by variables works.", {


  res <- get_class_ttest(cls, "Height", "Sex")

  res

  expect_equal(length(res), 4)
  expect_equal(names(res), c("Statistics", "ConfLimits", "TTests", "Equality"))


  res <- get_class_ttest(cls, "Height", "Sex", report = FALSE)

  res

  expect_equal(length(res), 4)
  expect_equal("VAR" %in% names(res$Statistics), TRUE)


})



test_that("ttest14: Simple proc_ttest with class and by works.", {

  # proc ttest data=sashelp.class alpha=0.05;
  # class sex /* Grouping Variable */;
  # var height;
  # run;


  res <- proc_ttest(cls,
                    by = "region",
                    var = "Height",
                    class = "Sex",
                    options = c(alpha = 0.1))

  res

  expect_equal(length(res), 4)
  expect_equal(nrow(res$Statistics), 8)
  expect_equal(nrow(res$ConfLimits), 8)
  expect_equal(nrow(res$TTests), 4)
  expect_equal(nrow(res$Equality), 2)
})


# test_that("test15: One sample test with sides parameter.", {
#
#
#   # data time;
#   # input time @@;
#   # datalines;
#   # 43  90  84  87  116   95  86   99   93  92
#   # 121  71  66  98   79  102  60  112  105  98
#   # ;
#   #
#   # proc ttest h0=80 plots(showh0) sides=u alpha=0.1;
#   # var time;
#   # run;
#
#   v1 <- c(43,  90,  84,  87,  116,   95,  86,   99,   93,  92,
#           121,  71,  66,  98,   79,  102,  60,  112,  105,  98)
#
#   dat <- data.frame(time = v1, stringsAsFactors = FALSE)
#
#   res <- proc_ttest(dat, var = time, options = c("h0"=80, alpha = .1))
#
#   expect_equal(res$Statistics
#
#
# })


test_that("ttest16: proc_ttest output options work.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = report)

  res

  expect_equal("VAR" %in% names(res$Statistics), FALSE)
  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 1)

  res <- proc_ttest(cls,
                    var = c("Height"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = long)

  res

  expect_equal(nrow(res$Statistics), 6)
  expect_equal(ncol(res$Statistics), 2)


  res <- proc_ttest(cls,
                    var = c("Height"),
                    by = Sex,
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = long)

  res

  expect_equal(nrow(res$Statistics), 12)
  expect_equal(ncol(res$Statistics), 3)


  res <- proc_ttest(cls,
                    var = c("Height"),
                    by = Sex,
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = c("out", "report"))

  res

  expect_equal(names(res), c("out", "report"))
  expect_equal(nrow(res$out$Statistics), 2)
  expect_equal(nrow(res$report$`Sex=F`$Statistics), 1)




})


test_that("ttest17: Multiple variables work with shaping options", {

  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    #by = Sex,
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = out) #v(out, report, wide))

  res

  expect_equal(nrow(res$Statistics), 2)
  expect_equal(ncol(res$Statistics), 7)
  expect_equal(res$Statistics$VAR[1], "Height")
  expect_equal(res$Statistics$VAR[2], "Weight")



  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    by = Sex,
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = out) #v(out, report, wide))

  res

  expect_equal(nrow(res$Statistics), 4)
  expect_equal(ncol(res$Statistics), 8)
  expect_equal(res$Statistics$VAR[1], "Height")
  expect_equal(res$Statistics$VAR[2], "Weight")



  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    by = Sex,
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = c("out", "stacked"))

  res

  expect_equal(nrow(res$Statistics), 24)


  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    by = Sex,
                    options = c("h0" = 65, "alpha" = 0.05),
                    output = c("out", "long"))

  res

  expect_equal(nrow(res$Statistics), 12)

})



test_that("ttest18: Two by variables one var works.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   by sex;
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height"), by = c("region", "Sex"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 4)
  expect_equal(all(c("BY1", "BY2") %in% names(res$Statistics)), TRUE)


  # Test report
  res <- proc_ttest(cls,
                    var = c("Height"), by = c("region", "Sex"), output = "report",
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table")

  res

  expect_equal(is.null(res), FALSE)
  # expect_equal(length(res), 3)
  # expect_equal(nrow(res$Statistics), 4)
  # expect_equal(all(c("BY1", "BY2") %in% names(res$Statistics)), TRUE)


})


test_that("ttest19: One by variable two vars works.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   by sex;
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height", "Weight"), by = c("Sex"),
                    options = c("h0" = 65, "alpha" = 0.05))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 4)
  expect_equal(all(c("BY") %in% names(res$Statistics)), TRUE)


  res <- proc_ttest(cls,
                    var = c("Height", "Weight"), by = c("Sex"), output = report,
                    options = c("h0" = 65, "alpha" = 0.05))

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]][[1]]), 1)

})

test_that("ttest20: Two by variables two vars works.", {

  # proc ttest data=sashelp.class
  # h0=65 /* Null hypothesis mean of 65 */
  #   alpha=0.05;  /* Significance level of 0.05 */
  #   var height; /* Specify the variable to test */
  #   by sex;
  #   run;


  res <- proc_ttest(cls,
                    var = c("Height", "Weight"), by = c("region", "Sex"),
                    options = c("h0" = 65, "alpha" = 0.05),
                    titles = "My first Frequency Table")


  res

  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 8)

  res <- proc_ttest(cls,
                    var = c("Height", "Weight"), by = c("region", "Sex"),
                    options = c("h0" = 65, "alpha" = 0.05), output = report,
                    titles = "My first Frequency Table")


  res

  expect_equal(length(res), 4)
  expect_equal(nrow(res[[1]][[1]]), 1)

  res <- proc_ttest(cls,
                    var = c("Height", "Weight"), by = c("region", "Sex"),
                    options = c("h0" = 65, "alpha" = 0.05), output = long,
                    titles = "My first Frequency Table")


  res

  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 24)


  res <- proc_ttest(cls,
                    var = c("Height", "Weight"), by = c("region", "Sex"),
                    options = c("h0" = 65, "alpha" = 0.05), output = stacked,
                    titles = "My first Frequency Table")


  res

  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 48)

})


test_that("ttest21: proc_ttest with two paired variables works.", {

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
  # paired before_measure * after_measure after_measure * before_measure;
  # run;

  res <- proc_ttest(paird,
                    paired = c("before_measure * after_measure",
                               "after_measure * before_measure"),
                    options = c(alpha = .1)
                    )

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 2)


  res <- proc_ttest(paird,
                    paired = c("before_measure * after_measure",
                               "after_measure * before_measure"),
                    by = region,
                    options = c(alpha = .1)
  )


  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 4)


  res <- proc_ttest(paird,
                    paired = c("before_measure * after_measure",
                               "after_measure * before_measure"),
                    by = region,
                    options = c(alpha = .1),
                    output = long
  )

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 12)


  res <- proc_ttest(paird,
                    paired = c("before_measure * after_measure",
                               "after_measure * before_measure"),
                    by = region,
                    options = c(alpha = .1),
                    output = stacked
  )


  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 3)
  expect_equal(nrow(res$Statistics), 24)




})


test_that("ttest22: proc_ttest with two vars and class works.", {

  # proc ttest data=sashelp.class alpha=0.05;
  # class sex /* Grouping Variable */;
  # var height;
  # run;

  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    class = "Sex",
                   # options = "noprint",
                    output = "report")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 8)
  expect_equal(nrow(res$`Height:Statistics`), 4)


  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    class = "Sex",
                    output = "out")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal(nrow(res$Statistics), 8)

  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    class = "Sex",
                    output = "long")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal(nrow(res$Statistics), 16)


  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    class = "Sex",
                    output = "stacked")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal(nrow(res$Statistics), 32)
  expect_equal(nrow(res$Equality), 8)



  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    class = "Sex",
                    by = "region",
                    output = "out")

  res

  expect_equal(length(res), 4)
  expect_equal(nrow(res$Statistics), 16)


  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    class = "Sex",
                    by = "region",
                    output = "long")

  res

  expect_equal(length(res), 4)
  expect_equal(nrow(res$Statistics), 32)


  res <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    class = "Sex",
                    by = "region",
                    output = "stacked")

  res

  expect_equal(is.null(res), FALSE)
  expect_equal(length(res), 4)
  expect_equal(nrow(res$Statistics), 64)
  expect_equal(nrow(res$Equality), 16)


})


test_that("ttest23: shape_ttest_data works as expected.", {


  r1 <- proc_ttest(cls,
                   var = c("Height", "Weight"),
                   class = "Sex",
                   output = "wide")


  dat1 <- r1$Statistics[c(3, 4), ]


  res1 <- shape_ttest_data(dat1, "long")

  res1

  expect_equal(nrow(res1), 12)


  dat2 <- r1$Statistics[c(3, 4, 7, 8), ]

  res2 <- shape_ttest_data(dat2, "stacked")

  res2

  expect_equal(nrow(res2), 24)

})

test_that("ttest24: test parameter checks.", {


  expect_error(proc_ttest(cls, var = "fork"))
  expect_error(proc_ttest(cls, var = "Height", options = "bork"))
  expect_error(proc_ttest(cls, var = "Height", options = c("bork"= 2)))
  expect_error(proc_ttest(cls, var = "Height", options = c("bork"= 2, "spork")))
  expect_error(proc_ttest(cls, paired = "bork * spork"))
  expect_error(proc_ttest(cls, paired = "bork * Height"))
  expect_error(proc_ttest(cls, paired = c("Weight * Height", "spork * Weight")))
  expect_error(proc_ttest(cls, var = "Height", class = c("Sex", "region")))

})

test_that("ttest25: Internal consistency checks with class.", {

  # proc ttest data=sashelp.class alpha=0.05;
  # class sex /* Grouping Variable */;
  # var height;
  # run;

  res1 <- proc_ttest(cls,
                    var = c("Height", "Weight"),
                    class = "Sex",
                    # options = "noprint",
                    output = c("out", "report"))

  res1

  expect_equal(res1$out$TTests$PROBT[1], res1$report$`Height:TTests`$PROBT[1])
  expect_equal(res1$out$TTests$PROBT[3], res1$report$`Weight:TTests`$PROBT[1])

  res2 <- proc_ttest(cls,
                     var = c("Height", "Weight"),
                     class = "Sex",
                     # options = "noprint",
                     output = c("out", "long"))

  res2

  expect_equal(res1$out$TTests$PROBT[1], res2$TTests$Height[3])
  expect_equal(res1$out$TTests$PROBT[3], res2$TTests$Weight[3])


  res3 <- proc_ttest(cls,
                     var = c("Height", "Weight"),
                     class = "Sex",
                     # options = "noprint",
                     output = c("out", "stacked"))

  res3

  expect_equal(res1$out$TTests$PROBT[1], res3$TTests$VALUES[3])
  expect_equal(res1$out$TTests$PROBT[3], res3$TTests$VALUES[9])

})

test_that("ttest25: Internal consistency checks with single variable.", {

  # proc ttest data=sashelp.class alpha=0.05;
  # class sex /* Grouping Variable */;
  # var height;
  # run;

  res1 <- proc_ttest(cls,
                     var = c("Height", "Weight"),
                     # options = "noprint",
                     output = c("out", "report"),
                     options = c(h0 = 65))

  res1

  expect_equal(res1$out$TTests$PROBT[1], res1$report$`Height:TTests`$PROBT[1])
  expect_equal(res1$out$TTests$PROBT[2], res1$report$`Weight:TTests`$PROBT[1])

  res2 <- proc_ttest(cls,
                     var = c("Height", "Weight"),
                     # options = "noprint",
                     output = c("out", "long"),
                     options = c(h0 = 65))

  res2

  expect_equal(res1$out$TTests$PROBT[1], res2$TTests$Height[3])
  expect_equal(res1$out$TTests$PROBT[2], res2$TTests$Weight[3])


  res3 <- proc_ttest(cls,
                     var = c("Height", "Weight"),  # VAR bad on all
                     # options = "noprint",
                     output = c("out", "stacked"),
                     options = c(h0 = 65))

  res3

  expect_equal(res1$out$TTests$PROBT[1], res3$TTests$VALUES[3])
  expect_equal(res1$out$TTests$PROBT[3], res3$TTests$VALUES[9])

})

test_that("ttest26: Internal consistency checks with paired", {

  # proc ttest data=sashelp.class alpha=0.05;
  # class sex /* Grouping Variable */;
  # var height;
  # run;

  res1 <- proc_ttest(paird,
                    paired = c("before_measure * after_measure"),
                    titles = "My first TTest Table",
                    output = c("out", "report"))


  res1

  expect_equal(res1$out$TTests$PROBT[1], res1$report$`diff1:TTests`$PROBT[1])


  res2 <- proc_ttest(paird,
                     paired = c("before_measure * after_measure"),
                     titles = "My first TTest Table",
                     output = c("out", "long"))

  res2

  expect_equal(res1$out$TTests$PROBT[1], res2$TTests$DIFF1[3])


  res3 <- proc_ttest(paird,
                     paired = c("before_measure * after_measure"),
                     titles = "My first TTest Table",
                     output = c("out", "stacked"))

  res3

  expect_equal(res1$out$TTests$PROBT[1], res3$TTests$VALUES[3])
  expect_equal(res1$out$TTests$PROBT[3], res3$TTests$VALUES[9])

})

test_that("ttest27: Check F Values for different var and class combinations.", {

  # SAS appears to pick the different variables for test or control
  # depending on what is going on in the data.  Best guess is that it is
  # picking the combination with highest F Value.  This is to run a few
  # comparisons against SAS and just make sure everything is coming out OK.

  # proc ttest data=sashelp.class alpha=0.05;
  # class sex /* Grouping Variable */;
  # var height;
  # run;


  res1 <- proc_ttest(cls,
                    by = "region",
                    var = "Height",
                    class = "Sex",
                    options = c(alpha = 0.1))

  res1

  expect_equal(as.numeric(res1$Equality$NDF), c(3, 3))
  expect_equal(as.numeric(res1$Equality$DDF), c(4, 5))
  expect_equal(as.numeric(res1$Equality$FVAL), c(2.04584557, 1.70834286))



  res2 <- proc_ttest(cls,
                     by = "region",
                     var = "Weight",
                     class = "Sex",
                     options = c(alpha = 0.1))

  res2

  expect_equal(as.numeric(res2$Equality$NDF), c(3, 3))
  expect_equal(as.numeric(res2$Equality$DDF), c(4, 5))
  expect_equal(as.numeric(res2$Equality$FVAL), c(1.4055752, 1.1721952))

  res3 <- proc_ttest(cls,
                     by = "region",
                     var = "Age",
                     class = "Sex",
                     options = c(alpha = 0.1))

  res3

  expect_equal(as.numeric(res3$Equality$NDF), c(4, 5))
  expect_equal(as.numeric(res3$Equality$DDF), c(3, 3))
  expect_equal(as.numeric(res3$Equality$FVAL), c(1.41818182, 1.290000))

  res4 <- proc_ttest(cls,
                     by = "region",
                     var = c("Height", "Weight", "Age"),
                     class = "Sex",
                     options = c(alpha = 0.1))

  res4



})

base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dat <- read.table(header = TRUE, text = '
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


prt <- read.table(header = TRUE, text = '
  sex internship enrollment count
  1  boys        yes        yes    35
  2  boys         no        yes    14
  3 girls        yes        yes    32
  4 girls         no        yes    53
  5  boys        yes         no    29
  6  boys         no         no    27
  7 girls        yes         no    10
  8 girls         no         no    23')



test_that("freq1: Simple proc_freq test works.", {



  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   titles = "My first Frequency Table",
                   view = FALSE)

  res

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)

})


test_that("freq2: Simple proc_freq test with report_type html works.", {



  fl <- file.path(base_path, "freq/freq2.html")


  res <- proc_freq(dat, tables = c("Eyes"),
                   titles = "My first Frequency Table",
                   report_type = "HTML",
                   report_location = fl)

  res
  ex <- file.exists(fl)

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq3: Two table proc_freq test with report_type html works.", {



  fl <- file.path(base_path, "freq/freq3.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", HairCount = "Hair"),
                   titles = "My first Frequency Table",
                   report_type = "HTML",
                   report_location = fl, view = TRUE)

  res

  res$Eyes

  ex <- file.exists(fl)

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq4: Simple proc_freq test with weight works.", {



  fl <- file.path(base_path, "freq/freq4.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = "HTML",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq5: Two var proc_freq with weight works.", {



  fl <- file.path(base_path, "freq/freq5.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   report_type = "HTML",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})


test_that("freq6: Simple proc_freq in docx works.", {



  fl <- file.path(base_path, "freq/freq6.docx")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = "DOCX",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})

test_that("freq7: Simple proc_freq in pdf works.", {



  fl <- file.path(base_path, "freq/freq7.pdf")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = "PDF",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})


test_that("freq8: Simple proc_freq in multiple outputs works.", {



  fl <- file.path(base_path, "freq/freq8")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = c("TXT", "RTF", "PDF"),
                   report_location = fl)

  res
  ex1 <- file.exists(paste0(fl, ".txt"))
  ex2 <- file.exists(paste0(fl, ".rtf"))
  ex3 <- file.exists(paste0(fl, ".pdf"))


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex1, TRUE)
  expect_equal(ex2, TRUE)
  expect_equal(ex3, TRUE)

})

test_that("freq9: Simple proc_freq with no file name works.", {



  fl <- file.path(base_path, "freq")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   report_type = "TXT",
                   report_location = fl)

  res
  ex <- file.exists(file.path(fl, "freq.txt"))


  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(ex, TRUE)

})


test_that("freq10: Two way proc_freq works.", {



  fl <- file.path(base_path, "freq/freq10.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c(FreqCount = "Eyes * Hair"),
                   table_options = list(out = "MyFreq"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   report_type = "HTML",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 14)
  expect_equal(ncol(res[[1]]), 8)
  expect_equal(ex, TRUE)

})


test_that("freq11: Two way proc_freq no weight works.", {



  fl <- file.path(base_path, "freq/freq11.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c(FreqCount = "Eyes * Hair"),
                   titles = "Eye and Hair Color of European Children",
                   report_type = "HTML",
                   report_location = fl)

  res
  ex <- file.exists(fl)


  expect_equal(nrow(res[[1]]), 14)
  expect_equal(ncol(res[[1]]), 8)
  expect_equal(ex, TRUE)

})

test_that("freq12: One way and two way proc_freq works.", {



  fl <- file.path(base_path, "freq/freq12.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair", FreqCount = "Eyes * Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   report_type = "HTML",
                   report_location = fl)

  res

  ex <- file.exists(fl)

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(ncol(res[[2]]), 5)
  expect_equal(nrow(res[[3]]), 14)
  expect_equal(ncol(res[[3]]), 8)
  expect_equal(ex, TRUE)

})




test_that("freq13: Cumsum and Cumpct options work as expected.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   table_options = list(cumsum = FALSE,
                                        cumpct = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res[[1]])


  expect_equal("Cum_Sum" %in% d, FALSE)
  expect_equal("Cum_Pct" %in% d, FALSE)


})


test_that("freq14: Out = options on table.", {


  res <- proc_freq(dat, tables = c("Eyes", "Eyes * Hair"),
                   table_options = list(out = "FreqCount"),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal(d[1], "Eyes")
  expect_equal(d[2], "Eyes * Hair")
  expect_equal(d[3], "FreqCount")


})


test_that("freq15: Outcum option works as expected.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   table_options = list(out = "fork",
                                        outcum = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res[["fork"]])


  expect_equal("Cum_Freq" %in% d, FALSE)
  expect_equal("Cum_Pct" %in% d, FALSE)

  res <- proc_freq(dat, tables = c("Eyes"),
                   table_options = list(outcum = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res[["Eyes"]])


  expect_equal("Cum_Freq" %in% d, TRUE)
  expect_equal("Cum_Pct" %in% d, TRUE)



})


test_that("freq16: Freq and Pct options works as expected.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   table_options = list(freq = FALSE,
                                        pct = TRUE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res[["Eyes"]])


  expect_equal("Frequency" %in% d, FALSE)
  expect_equal("Percent" %in% d, TRUE)

  res <- proc_freq(dat, tables = c("Eyes"),
                   table_options = list(freq = TRUE,
                                        pct = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res[["Eyes"]])


  expect_equal("Frequency" %in% d, TRUE)
  expect_equal("Percent" %in% d, FALSE)



})



test_that("freq17: Sparse option works as expected.", {


  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   table_options = list(out = "freqtbl", sparse = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res


  expect_equal(nrow(res[[2]]), 14)
  expect_equal(ncol(res[[2]]), 4)

})

test_that("freq18: Crosstab works.", {

  library(fmtr)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  freqdata <- res[[1]]



  expect_equal(nrow(res[[1]]), 14)
  expect_equal(ncol(res[[1]]), 8)

})


test_that("freq19: Format options on table.", {

  library(fmtr)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   table_options = list(format = "%.3f%%"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  crs <- res[[1]]$dark

  fmt <- attr(crs, "format")

  expect_equal("fmt_lst" %in% class(fmt), TRUE)



})

test_that("freq20: SAS replication of one way tables works.", {

  library(fmtr)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair"),
                   titles = "Eye and Hair Color of European Children",
                   weight = "Count",
                   view = TRUE)

  res

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(ncol(res[[2]]), 5)

})

test_that("freq21: Rowpct and Colpct options on table work.", {

  library(fmtr)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   table_options = list(rowpct = FALSE, colpct = FALSE),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(nrow(res[[1]]), 8)
  expect_equal(ncol(res[[1]]), 8)


})

test_that("freq22: Crosstab option to turn off totals works.", {

  library(fmtr)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair", "Eyes * Hair"),
                   table_options = list(totcol = FALSE, totrow = FALSE),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(nrow(res[[3]]), 12)
  expect_equal(ncol(res[[3]]), 7)

})

test_that("freq23: Piped option works as expected.", {

  library(fmtr)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   table_options = list(out = "FreqTbl"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   piped = TRUE)

  res

  expect_equal("data.frame" %in% class(res), TRUE)


})


test_that("freq24: report_style parameter works as expected.", {

  library(fmtr)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   table_options = list(out = "FreqTbl"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   report_style = "MidnightBlue")

  res

  expect_equal(length(res), 2)


})

#
# lst <- unclass(mtcars)[c("mpg", "cyl")]
#
# f1 <- as.factor(mtcars$vs)
# f2 <- as.factor(mtcars$am)
#
# flst <- list(vs = f1, am = f2)
# dtlst <- split(mtcars, flst, sep = " by ")
#
# dtlst

test_that("freq25: Single by group on single table works.", {

  library(fmtr)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")


  res <- proc_freq(dat, tables = c("Eyes"), by = "Region",
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 3)


})

test_that("freq26: Single by group on double table works.", {

  library(fmtr)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")


  res <- proc_freq(dat, tables = c("Eyes", "Hair"), by = c("Region"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 4)
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(nrow(res[[2]]), 5)

})


test_that("freq27: Double by group on double table works.", {

  library(fmtr)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")


  res <- proc_freq(spdat, tables = c("Eyes", "Hair"), by = c("Sex", "Region"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 8)
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(names(res)[1], "Sex=F, Region=1, Eyes")


})

test_that("freq28: Double by group on double table with table names works.", {

  library(fmtr)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                        Hair = "Hair Color",
                        Region = "Geographic Region")


  res <- proc_freq(spdat, tables = c(EyeTbl = "Eyes", HairTbl ="Hair"), by = c("Sex", "Region"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 8)
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(names(res)[1], "Sex=F, Region=1, EyeTbl")
  expect_equal(names(res)[2], "Sex=F, Region=1, HairTbl")


})


test_that("freq29: Double by group on double table no labels works.", {

  library(fmtr)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- NULL

  res <- proc_freq(spdat, tables = c(EyeTbl = "Eyes", HairTbl ="Hair"),
                   by = c("Sex", "Region"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 8)
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(names(res)[1], "Sex=F, Region=1, EyeTbl")
  expect_equal(names(res)[2], "Sex=F, Region=1, HairTbl")


})

test_that("freq30: Crosstab with by works.", {

  library(fmtr)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                        Hair = "Hair Color",
                        Region = "Geographic Region")

  res <- proc_freq(spdat, tables = c("Eyes * Hair"),
                 #  table_options = list(out = "FreqTable"),
                   by = c("Sex"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 14)
  expect_equal(names(res)[1], "Sex=F, Eyes * Hair")
  expect_equal(names(res)[2], "Sex=M, Eyes * Hair")


})

test_that("freq30: Crosstab with by and out works.", {

  library(fmtr)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                        Hair = "Hair Color",
                        Region = "Geographic Region")

  res <- proc_freq(spdat, tables = c("Eyes * Hair"),
                    table_options = list(out = "FreqTable"),
                   by = c("Sex"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 4)
  expect_equal(nrow(res[[1]]), 14)
  expect_equal(names(res)[1], "Sex=F, Eyes * Hair")
  expect_equal(names(res)[2], "Sex=F, FreqTable")


})


test_that("freq31: Parameter checks work.", {


  expect_error(proc_freq(dat, tables = c("Fork", "Eyes", "Bork")))
  expect_error(proc_freq(dat, by = "Fork", tables = "Eye"))


})



test_that("freq32: chi sqr works with weight.", {


  # fp <- file.path(base_path, "/data/treatment.csv")
  # csv <- read.csv(fp)


  res <- proc_freq(prt, tables = "internship * enrollment",
                   table_options = list(ChiSq = TRUE),
                   weight = "count")

  res

  expect_equal(length(res), 2)

  res2 <- res[[2]]
  expect_equal(res2[1, 2], 0.8189423)
  expect_equal(res2[2, 2], 1)
  expect_equal(res2[3, 2], 0.365489592)
  #expect_equal(nrow(res[[2]]), 14)

})


test_that("freq33: fisher's works with weight.", {

  res <- proc_freq(prt, tables = "internship * enrollment",
                   table_options = list(Fisher = TRUE),
                   weight = "count")

  res

  expect_equal(length(res), 2)

  res2 <- res[[2]]
  expect_equal(res2[1, 2],67)
  expect_equal(res2[2, 2], 0.85127668)
  expect_equal(res2[3, 2], 0.22133142)
  expect_equal(res2[4, 2], 0.41215159)

  #expect_equal(nrow(res[[1]]), 14)

})

test_that("freq34: fisher's works with weight and by.", {

  res <- proc_freq(prt, tables = "internship * enrollment",
                   table_options = list(Fisher = TRUE),
                   by = "sex",
                   weight = "count")

  res

  expect_equal(length(res), 4)

  res2 <- res[[2]]
  expect_equal(res2[1, 2], 35)
  expect_equal(res2[2, 2], 0.98846024)
  expect_equal(res2[3, 2], 0.03111341)
  expect_equal(res2[4, 2], 0.046665258)

  res4 <- res[[4]]
  expect_equal(res4[1, 2], 32)
  expect_equal(res4[2, 2], 0.83173972)
  expect_equal(res4[3, 2], 0.29935132)
  expect_equal(res4[4, 2], 0.524477809)

  #expect_equal(nrow(res[[1]]), 14)

})


test_that("freq35: chi sqr works with weight and by.", {


  # fp <- file.path(base_path, "/data/treatment.csv")
  # csv <- read.csv(fp)


  res <- proc_freq(prt, tables = "internship * enrollment",
                   table_options = list(ChiSq = TRUE),
                   by = "sex",
                   weight = "count")

  res

  expect_equal(length(res), 4)

  res2 <- res[[2]]
  expect_equal(res2[1, 2], 4.23661395)
  expect_equal(res2[2, 2], 1)
  expect_equal(res2[3, 2], 0.039560993)


  res4 <- res[[4]]
  expect_equal(res4[1, 2], 0.55926894)
  expect_equal(res4[2, 2], 1)
  expect_equal(res4[3, 2], 0.45455495)


})


test_that("freq36: 2 way table is sorted properly.", {



  res <- proc_freq(prt, tables = "internship * enrollment",
                   weight = "count")

  res
  expect_equal(res[[1]][9, 1], "Total")
  expect_equal(res[[1]][10, 1], "Total")
})



test_that("freq37: Crosstab works with factors.", {

  prt2 <- prt

  prt2$internship <- as.factor(prt2$internship)
  prt2$enrollment <- as.factor(prt2$enrollment)


  res <- proc_freq(prt2, tables = c("sex", "internship * enrollment"),
                   table_options = list(out = "FreqCounts"),
                   weight = "count")

  res
  expect_equal(nrow(res[[1]]), 2)
  expect_equal(ncol(res[[1]]), 5)
  expect_equal(nrow(res[[2]]), 10)
  expect_equal(ncol(res[[2]]), 5)
  expect_equal(nrow(res[[3]]), 4)
  expect_equal(ncol(res[[3]]), 4)
})



# test_that("freq35: CMH works with weight.", {
#
#
#   # fp <- file.path(base_path, "/data/treatment.csv")
#   # csv <- read.csv(fp)
#
#   mantelhaen.test()
#
# })

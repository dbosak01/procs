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


prt2 <- read.table(header = TRUE, text = '
      sex internship enrollment count  group
  1  boys        yes        yes    35      1
  2  boys         no        yes    14      1
  3 girls        yes        yes    32      1
  4 girls         no        yes    53      1
  5  boys        yes         no    29      2
  6  boys         no         no    27      2
  7 girls        yes         no    10      2
  8 girls         no         no    23      2')


options("logr.output" = FALSE)

test_that("freq1: Simple proc_freq test works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   titles = "My first Frequency Table",
                   view = TRUE)

  res

  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 7)

})

test_that("freq2: proc_freq with label and format options works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   titles = "My first Frequency Table",
                   view = TRUE,
                   out = out(label = c(VAR = "Variable", CAT = "Category"),
                             format = list(CUMPCT = "%.3f")))

  res

  # proc_print(res)

  a1 <- attributes(res$VAR)
  a2 <- attributes(res$CUMPCT)

  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 7)

  expect_equal(a1$label, "Variable")
  expect_equal(a2$format, "%.3f")

})



test_that("freq3: Two table proc_freq test with output works.", {

  library(common)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", HairCount = "Hair"),
                   titles = "My first Frequency Table", view = TRUE)

  res

  res$Eyes

  expect_equal(length(res), 2)
  expect_equal(names(res), c("Eyes", "HairCount"))
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 7)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(ncol(res[[2]]), 7)

})

test_that("freq4: Simple proc_freq test with weight works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table")

  res


  expect_equal(res$CNT[1], 222)
  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 7)


})

test_that("freq5: Two var proc_freq with weight works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res
  #ex <- file.exists(fl)


  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 7)
  expect_equal(res[[1]]$CNT[1], 222)
  expect_equal(res[[2]]$CNT[5], 113)

})


test_that("freq6: Simple proc_freq with output long works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   out = out(shape = "long"))

  res

  expect_equal(nrow(res), 5)
  expect_equal(ncol(res), 5)


})

test_that("freq7: Simple proc_freq with 2 way works.", {

  library(common)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   weight = "Count",
                   view = TRUE,
                   titles = "My first Frequency Table")

  res

  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 9)

})


test_that("freq8: Simple proc_freq in multiple outputs works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat,
                   weight = "Count",
                   titles = "My first Frequency Table",
                   out1 = out(table = "Eyes"),
                   out2 = out(table = "Hair"),
                   out3 = out(table = "Eyes * Hair")
                   )

  res

  expect_equal(length(res), 3)
  expect_equal(names(res), c("out1", "out2", "out3"))
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 7)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(ncol(res[[2]]), 7)
  expect_equal(nrow(res[[3]]), 15)
  expect_equal(ncol(res[[3]]), 9)

})

test_that("freq9: Simple proc_freq 1 way by variable works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   by = "Region")

  res

  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 8)

})


test_that("freq10: Two way proc_freq works.", {

  library(common)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c(FreqCount = "Eyes * Hair"),
                   options = opts(out = "MyFreq", cumsum = FALSE,
                                        cumpct = FALSE),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 7)

})


test_that("freq11: Two way proc_freq no weight works.", {

  library(common)

  fl <- file.path(base_path, "freq/freq11.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c(FreqCount = "Eyes * Hair"),
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 9)

})

test_that("freq12: One way and two way proc_freq works.", {

  library(common)

  fl <- file.path(base_path, "freq/freq12.html")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair", FreqCount = "Eyes * Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 7)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(ncol(res[[2]]), 7)
  expect_equal(nrow(res[[3]]), 15)
  expect_equal(ncol(res[[3]]), 9)


})




test_that("freq13: Cumsum and Cumpct options work as expected.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   options = opts(cumsum = FALSE,
                                        cumpct = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CUMSUM" %in% d, FALSE)
  expect_equal("CUMPCT" %in% d, FALSE)


})


test_that("freq14: Out = options on table.", {


  res <- proc_freq(dat, tables = c("Eyes", "Eyes * Hair"),
                   options = opts(out = "FreqCount"),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal(d[1], "Eyes")
  expect_equal(d[2], "Eyes * Hair")


})


test_that("freq15: Outcum option works as expected.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   options = opts(out = "fork",
                                        outcum = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CUMSUM" %in% d, FALSE)
  expect_equal("CUMPCT" %in% d, FALSE)

  res <- proc_freq(dat, tables = c("Eyes"),
                   options = opts(outcum = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CUMSUM" %in% d, FALSE)
  expect_equal("CUMPCT" %in% d, FALSE)



})


test_that("freq16: Freq and Pct options works as expected.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   options = opts(freq = FALSE,
                                        pct = TRUE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CNT" %in% d, FALSE)
  expect_equal("PCT" %in% d, TRUE)

  res <- proc_freq(dat, tables = c("Eyes"),
                   options = opts(freq = TRUE,
                                        pct = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CNT" %in% d, TRUE)
  expect_equal("PCT" %in% d, FALSE)



})



test_that("freq17: Sparse option works as expected.", {


  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   options = opts(out = "freqtbl", sparse = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res


  expect_equal(nrow(res), 14)
  expect_equal(ncol(res), 9)

})

test_that("freq18: Crosstab works.", {

  library(common)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 9)

})


test_that("freq19: Format options on table.", {

  library(common)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   options = opts(format = "%.3f%%"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   out = out(report = TRUE))

  crs <- res$dark

  fmt <- attr(crs, "format")

  expect_equal("fmt_lst" %in% class(fmt), TRUE)



})

test_that("freq20: SAS replication of one way tables works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair"),
                   titles = "Eye and Hair Color of European Children",
                   weight = "Count",
                   view = TRUE)

  res

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 7)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(ncol(res[[2]]), 7)

})

test_that("freq21: Rowpct and Colpct options on table work.", {

  library(common)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   options = opts(rowpct = FALSE, colpct = FALSE),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   out = out(report = TRUE))

  res

  expect_equal(nrow(res), 8)
  expect_equal(ncol(res), 8)


})

test_that("freq22: Crosstab option to turn off totals works.", {

  library(common)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair", "Eyes * Hair"),
                   options = opts(totcol = FALSE, totrow = FALSE),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   out = out(report = TRUE))

  res

  expect_equal(nrow(res[[3]]), 12)
  expect_equal(ncol(res[[3]]), 7)

})

test_that("freq23: proc_freq with drop, keep and rename options works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   titles = "My first Frequency Table",
                   view = TRUE,
                   out = out(drop = "CUMPCT",
                             keep = c("CAT", "VAR", "N", "CNT", "PCT"),
                             rename = c(VAR = "BLOCK")))

  res

  # proc_print(res)

  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 5)
  expect_equal(names(res), c("BLOCK", "CAT", "N", "CNT", "PCT"))

})

test_that("freq24: proc_freq with where output option works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   titles = "My first Frequency Table",
                   view = TRUE,
                   out = out(where = expression(CAT == "green")))

  res

  # proc_print(res)


  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 7)

})


test_that("freq25: Single by group on single table works.", {

  library(common)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")


  res <- proc_freq(dat, tables = c("Eyes"), by = "Region",
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 8)


})

test_that("freq26: Single by group on double table works.", {

  library(common)


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")


  res <- proc_freq(dat, tables = c("Eyes", "Hair"), by = c("Region"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 6)
  expect_equal(nrow(res[[2]]), 10)

})


test_that("freq27: Double by group on double table works.", {

  library(common)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")


  res <- proc_freq(spdat, tables = c("Eyes", "Hair"), by = c("Sex", "Region"),
                   weight = "Count",
                   options = opts(sparse = FALSE),
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 12)
  expect_equal(nrow(res[[2]]), 17)
  expect_equal(sum(spdat$Count), sum(res$Eyes$CNT))
  expect_equal(sum(spdat$Count), sum(res$Hair$CNT))


  res <- proc_freq(spdat, tables = c("Eyes", "Hair"), by = c("Sex", "Region"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 12)
  expect_equal(nrow(res[[2]]), 20)
  expect_equal(sum(spdat$Count), sum(res$Eyes$CNT))
  expect_equal(sum(spdat$Count), sum(res$Hair$CNT))


})

test_that("freq28: Double by group on double table with table names works.", {

  library(common)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                        Hair = "Hair Color",
                        Region = "Geographic Region")


  res <- proc_freq(spdat, tables = c(EyeTbl = "Eyes", HairTbl ="Hair"), by = c("Sex", "Region"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 12)
  expect_equal(nrow(res[[2]]), 20)
  expect_equal(names(res)[1], "EyeTbl")
  expect_equal(names(res)[2], "HairTbl")


})


test_that("freq29: Double by group on double table no labels works.", {

  library(common)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- NULL

  res <- proc_freq(spdat, tables = c(EyeTbl = "Eyes", HairTbl ="Hair"),
                   by = c("Sex", "Region"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 12)
  expect_equal(nrow(res[[2]]), 20)
  expect_equal(names(res)[1], "EyeTbl")
  expect_equal(names(res)[2], "HairTbl")


})

test_that("freq30: Crosstab with by works.", {

  library(common)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                        Hair = "Hair Color",
                        Region = "Geographic Region")

  res <- proc_freq(spdat, tables = c("Eyes * Hair"),
                   by = c("Sex"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res


  expect_equal(nrow(res), 30)
  expect_equal(ncol(res), 10)


})

test_that("freq30: Crosstab with by and out works.", {

  library(common)

  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                        Hair = "Hair Color",
                        Region = "Geographic Region")

  res <- proc_freq(spdat, tables = c("Eyes * Hair"),
                    options = opts(out = "FreqTable"),
                   by = c("Sex"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   out = out(report = TRUE))

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
                   options = opts(ChiSq = TRUE),
                   weight = "count",
                   out = out(report = TRUE))

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
                   options = opts(Fisher = TRUE),
                   weight = "count",
                   out = out(report = TRUE))

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
                   options = opts(Fisher = TRUE),
                   by = "sex",
                   weight = "count",
                   out = out(report = TRUE))

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
                   options = opts(ChiSq = TRUE),
                   by = "sex",
                   weight = "count",
                   out = out(report = TRUE))

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
                   weight = "count",
                   out = out(report = TRUE))

  res
  expect_equal(res[9, 1], "Total")
  expect_equal(res[10, 1], "Total")
})



test_that("freq37: Crosstab works with factors.", {

  prt2 <- prt

  prt2$internship <- as.factor(prt2$internship)
  prt2$enrollment <- as.factor(prt2$enrollment)


  res <- proc_freq(prt2, tables = c("sex", "internship * enrollment"),
                   options = opts(out = "FreqCounts"),
                   weight = "count",
                   out = out(report = TRUE))

  res
  expect_equal(nrow(res[[1]]), 2)
  expect_equal(ncol(res[[1]]), 6)
  expect_equal(nrow(res[[2]]), 10)
  expect_equal(ncol(res[[2]]), 5)
  expect_equal(nrow(res[[3]]), 4)
  expect_equal(ncol(res[[3]]), 4)
})

test_that("freq38: get_output_specs works as expected.", {


  res1 <- get_output_specs(c("A", "B", "A * B"), list())

  res1
  expect_equal(length(res1), 3)
  expect_equal(res1[[1]]$table, "A")
  expect_equal(res1[[2]]$table, "B")
  expect_equal(res1[[3]]$table, "A * B")


  res2 <- get_output_specs(c(tab1 = "A", "B", tab3 = "A * B"), list())

  res2
  expect_equal(length(res2), 3)
  expect_equal(names(res2), c("tab1", "B", "tab3"))
  expect_equal(res2[[1]]$table, "A")
  expect_equal(res2[[2]]$table, "B")
  expect_equal(res2[[3]]$table, "A * B")

  ot <- list(out = out(stats = c("n", "pct"), shape = "wide"))
  res3 <- get_output_specs(c(tab1 = "A", "B", tab3 = "A * B"), ot)

  res3
  expect_equal(length(res3), 3)
  expect_equal(names(res3), c("tab1", "B", "tab3"))
  expect_equal(res3[[1]]$table, "A")
  expect_equal(res3[[2]]$table, "B")
  expect_equal(res3[[3]]$table, "A * B")



  ot <- list(out1 = out(table = "A", stats = c("n", "pct"), shape = "wide"),
             out2 = out(table = "B", stats = c("n", "pct"), shape = "wide"),
             out3 = out(table = "A * B", stats = c("n", "pct"), shape = "wide")

             )
  res4 <- get_output_specs(NULL, ot)

  res4
  expect_equal(length(res4), 3)
  expect_equal(names(res4), c("out1", "out2", "out3"))
  expect_equal(res4[[1]]$table, "A")
  expect_equal(res4[[2]]$table, "B")
  expect_equal(res4[[3]]$table, "A * B")


  ot <- list(out1 = out(stats = c("n", "pct"), shape = "wide"),
             out2 = out(table = "A * B", stats = c("chisq"), shape = "wide")

  )
  res5 <- get_output_specs(c(tab1 = "A", "B", tab3 = "A * B"), ot)

  res5
  expect_equal(length(res5), 4)
  expect_equal(names(res5), c("tab1", "B", "tab3", "out2"))
  expect_equal(res5[[1]]$table, "A")
  expect_equal(res5[[2]]$table, "B")
  expect_equal(res5[[3]]$table, "A * B")
  expect_equal(res5[[4]]$table, "A * B")
  expect_equal(res5[[4]]$stats, "chisq")

})


test_that("freq39: get_output_oneway() works as expected.", {


  res1 <- get_output_oneway(prt, "internship", "count", NULL, by = c(am = 1),
                            shape = "wide")

  res1


  expect_equal(nrow(res1), 2)
  expect_equal(ncol(res1), 8)

  res2 <- get_output_oneway(prt, "internship", "count", NULL, by = c(am = "A",
                                                                     pm = "B"))

  res2


  expect_equal(nrow(res2), 2)
  expect_equal(ncol(res2), 9)

})


test_that("freq40: get_output_oneway() long works as expected.", {


  res1 <- get_output_oneway(prt, "internship", "count", NULL, by = c(am = 1),
                            shape = "long")

  res1


  expect_equal(nrow(res1), 5)
  expect_equal(ncol(res1), 5)

  res2 <- get_output_oneway(prt, "internship", "count", NULL,
                            by = c(am = "A", pm = "B"), shape = "long")

  res2


  expect_equal(nrow(res2), 5)
  expect_equal(ncol(res2), 6)

})

test_that("freq41: get_output_twoway() works as expected.", {


  res1 <- get_output_twoway(prt, "internship", "enrollment", "count", NULL,
                            FALSE, by = c(by1 = 1), shape = "wide")

  res1

  expect_equal(nrow(res1), 4)
  expect_equal(ncol(res1), 7)

  res2 <- get_output_twoway(prt, "internship", "enrollment", "count",  NULL,
                            FALSE, by = c(by1 = "A", by2 = "B"))

  res2


  expect_equal(nrow(res2), 4)
  expect_equal(ncol(res2), 8)

})

test_that("freq42: get_output_twoway() long works as expected.", {


  res1 <- get_output_twoway(prt, "internship", "enrollment", "count", NULL,
                            FALSE, by = c(by1 = 1), shape = "long")

  res1

  expect_equal(nrow(res1), 2)
  expect_equal(ncol(res1), 8)

  res2 <- get_output_twoway(prt, "internship", "enrollment", "count",  NULL,
                            FALSE, by = c(by1 = "A", by2 = "B"),
                            shape = "long")

  res2


  expect_equal(nrow(res2), 2)
  expect_equal(ncol(res2), 9)

})



test_that("freq43: oneway output statistics work.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat,
                   titles = "My first Frequency Table",
                   view = TRUE,
                   weight = "Count",
                   out1 = out(table ="Eyes", stats = c("cnt", "pct", "n")),
                   out2 = out(table = "Hair",
                              stats = c("cumsum", "cnt", "pct"),
                              shape = "long"))

  res

  expect_equal(length(res), 2)
  expect_equal(names(res[[1]]), c("VAR", "CAT", "CNT", "PCT", "N"))
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(nrow(res[[2]]), 3)
  expect_equal(res[[2]]$STAT, c("CUMSUM", "CNT", "PCT"))

})

test_that("freq45: twoway output statistics work.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat,
                   tables = c("Eyes", "Region * Eyes", "Region"),
                   titles = "My first Frequency Table",
                   view = TRUE,
                   weight = "Count",
                   out1 = out(table ="Region * Eyes", stats = c("cnt", "pct", "n"),
                              shape = "wide"),
                   out2 = out(table = "Region * Hair",
                              stats = c("cumsum", "cnt", "pct"),
                              shape = "long"))

  res

  expect_equal(length(res), 2)
  expect_equal(names(res[[1]]), c("VAR1", "VAR2", "CAT1", "CAT2", "CNT", "PCT", "N"))
  expect_equal(nrow(res[[1]]), 6)
  expect_equal(nrow(res[[2]]), 3)
  expect_equal(res[[2]]$STAT, c("CUMSUM", "CNT", "PCT"))

})

test_that("freq46: output parameter works.", {

  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  myouts <- list(out1 = out(table ="Region * Eyes", stats = c("cnt", "pct", "n")),
                 out2 = out(table = "Region * Hair",
                             stats = c("cumsum", "cnt", "pct"),
                             shape = "long"))

  res <- proc_freq(dat,
                   tables = c("Eyes", "Region * Eyes", "Region"),
                   titles = "My first Frequency Table",
                   view = TRUE,
                   weight = "Count",
                   output = myouts)


  res

  expect_equal(length(res), 2)
  expect_equal(names(res[[1]]), c("VAR1", "VAR2", "CAT1", "CAT2", "CNT", "PCT", "N"))
  expect_equal(nrow(res[[1]]), 6)
  expect_equal(nrow(res[[2]]), 3)
  expect_equal(res[[2]]$STAT, c("CUMSUM", "CNT", "PCT"))

})


test_that("freq47: output report works.", {


  library(common)

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat,
                   tables = c("Eyes", "Hair",  Cross = "Hair * Eyes"),
                   titles = "My first Frequency Table",
                   by = "Region",
                   view = TRUE,
                   weight = "Count",
                   report = out(report = TRUE))


  res
  expect_equal(length(res), 6)
  nms <- names(res)
  expect_equal(nms[1], "Region=1, Eyes")
  expect_equal(nms[4], "Region=2, Eyes")
})





test_that("freq48: oneway output stacked works.", {


  res <- proc_freq(dat,
                   tables = c("Eyes"),
                   titles = "My first Frequency Table",
                   by = "Region",
                   view = TRUE,
                   weight = "Count",
                   report = out(stats = c("n", "cnt", "pct"), shape = "stacked"))


  res

  expect_equal(nrow(res), 18)
  expect_equal(ncol(res), 5)
})

# test_that("freq49: twoway output stacked works.", {
#
#
#   res <- proc_freq(dat,
#                    tables = c("Eyes * Hair"),
#                    titles = "My first Frequency Table",
#                    by = "Region",
#                    view = TRUE,
#                    weight = "Count",
#                    report = out(stats = c("n", "cnt", "pct"), shape = "stacked"))
#
#
#   res
#
#   expect_equal(nrow(res), 90)
#   expect_equal(ncol(res), 7)
# })


test_that("freq50: chisq output statistics works.", {


  res <- proc_freq(prt,
                   tables = c("internship * enrollment"),
                   titles = "My title",
                   by = c("sex"),
                   view = TRUE,
                   weight = "count",
                   options = opts(chisq = TRUE),
                   out5 = out(table = "internship * enrollment",
                              stats = c("n", "cnt", "pct", "chisq")),
                   out6 = out(stats = "chisq"))


  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[["out5"]]), 8)
  expect_equal(ncol(res[["out5"]]), 11)
  expect_equal(nrow(res[["out6"]]), 2)
  expect_equal(ncol(res[["out6"]]), 6)

})


test_that("freq51: fisher output statistics works.", {


  res <- proc_freq(prt,
                   tables = c("internship * enrollment"),
                   titles = "My title",
                   by = c("sex"),
                   view = TRUE,
                   weight = "count",
                   options = opts(fisher = TRUE),
                   out5 = out(table = "internship * enrollment",
                              stats = c("n", "cnt", "pct", "fisher")),
                   out6 = out(stats = "fisher")
  )

  res

  expect_equal(nrow(res[[1]]), 8)
  expect_equal(ncol(res[[1]]), 12)
  expect_equal(nrow(res[[2]]), 2)
  expect_equal(ncol(res[[2]]), 7)

})

test_that("freq52: Logging function works.", {

  res <- log_freq(mtcars, tables = c("mpg", "cyl"),
                  weight = "count",
                  by = "cyl", outcnt = 6)

  res

  expect_equal(length(res), 6)

})


test_that("freq53: error on unknown parameter.", {


  expect_warning(proc_freq(prt2,
                   tables = c("internship"),
                   titles = "My first Frequency Table",
                   by = c("sex", "enrollment"),
                   view = TRUE,
                   fork = TRUE,
                   weight = "count",
                   out = out()))
})



test_that("freq54: where works before and after rename.", {


  res <- proc_freq(prt2,
                   tables = c("internship"),
                   titles = "My first Frequency Table",
                   by = c("sex", "enrollment"),
                   view = TRUE,
                   weight = "count",
                   out = out(rename = list(BY2 = "Enrollment"),
                             where = expression(Enrollment == "no")))

  res

  expect_equal(nrow(res), 4)

  res <- proc_freq(prt2,
                   tables = c("internship"),
                   titles = "My first Frequency Table",
                   by = c("sex", "enrollment"),
                   view = TRUE,
                   weight = "count",
                   out = out(rename = list(BY2 = "Enrollment"),
                             where = expression(BY2 == "no")))

  res

  expect_equal(nrow(res), 4)



})



test_that("freq56: get_table_list() works as expected.", {

  vars <- c("A", "B", "A * B", "A * C")

  res <- get_table_list(vars)

  res
  expect_equal(length(res), 4)
  expect_equal(res[[3]], c("A", "B"))

})


test_that("freq56: get_output_tables() works as expected.", {

  lst <- list(out1 = out(table = "A"),
              out2 = out(table = "B"),
              out3 = out(table = "A * B"),
              out4 = out(table = "A * C"))

  res <- get_output_tables(lst)

  res

  expect_equal(length(res), 4)
  expect_equal(res[[3]], "A * B")


})


test_that("freq55: get_nway_zero_fills() works as expected.", {

  lst <- list(out1 = out(table = "x"),
              out2 = out(table = "y"),
              out3 = out(table = "x * y"))


  dt <- data.frame(x = c("A", "A", "B", "B"),
                   y = c("C", "C", "C", "D"),
                   z = c("E", "F", "F", "F"),
                   w = c(25, 39, 18, 4))

  dt

  res <- get_nway_zero_fills(dt, lst, "z", NULL)

  res

  expect_equal(nrow(res), 20)
  expect_equal(ncol(res), 5)

  res <- get_nway_zero_fills(dt, lst, "z", "w")

  res

  expect_equal(nrow(res), 20)
  expect_equal(ncol(res), 5)


  lst2 <- list(out1 = out(table = "x"))

  res <- get_nway_zero_fills(dt, lst2, c("y", "z"), weight = "w")

  res

  expect_equal(nrow(res), 12)
  expect_equal(ncol(res), 5)


})

test_that("freq52: zero count categories appear on oneway tables.", {

  sp <- prt2

  sp[1, 2] <- "no"

  res <- proc_freq(sp,
                   tables = c("internship"),
                   titles = "My first Frequency Table",
                   by = c("sex", "enrollment"),
                   view = TRUE,
                   weight = "count",
                   out = out())

  res

  expect_equal(ncol(res), 9)
  expect_equal(nrow(res), 8)


  res <- proc_freq(sp,
                   tables = c("internship"),
                   titles = "My first Frequency Table",
                   by = c("sex", "enrollment"),
                   view = TRUE,
                   weight = "count",
                   out = out(report = TRUE))

  res

  expect_equal(length(res), 4)
  expect_equal(ncol(res[[3]]), 6)
  expect_equal(nrow(res[[3]]), 2)

})

test_that("freq52: zero count categories appear on twoway tables.", {

  sp <- prt2

  sp[1, 2] <- "no"

  res <- proc_freq(sp,
                   tables = c("internship * enrollment"),
                   titles = "My first Frequency Table",
                   by = c("sex"),
                   view = TRUE,
                   weight = "count",
                   out = out())

  res

  expect_equal(ncol(res), 10)
  expect_equal(nrow(res), 8)


  res <- proc_freq(sp,
                   tables = c("internship * enrollment"),
                   titles = "My first Frequency Table",
                   by = c("sex"),
                   view = TRUE,
                   weight = "count",
                   out = out(report = TRUE))

  res

  expect_equal(length(res), 2)
  expect_equal(ncol(res[[2]]), 5)
  expect_equal(nrow(res[[2]]), 10)

})



#
# test_that("freq52: oneway across works.", {
#
#
#   res <- proc_freq(prt2,
#                    tables = c("internship"),
#                    titles = "My title",
#                    by = "group",
#                    view = TRUE,
#                    across = "group",
#                    weight = "count",
#                    out5 = out(table = "internship",
#                               stats = c("n", "cnt", "pct"),
#                               shape = "wide"))
#
#   res
#
#   res$CNTPCT <- fapply2(res$CNT, res$PCT, "%d", "(%.1f%%)", " ")
#   res$CNT <- NULL
#   res$PCT <- NULL
#
#   proc_transpose(res, id = "BY", by = "CAT", copy = "VAR", var = c("N", "CNTPCT"))
#
#   # expect_equal(nrow(res[[1]]), 8)
#   # expect_equal(ncol(res[[1]]), 12)
#   # expect_equal(nrow(res[[2]]), 2)
#   # expect_equal(ncol(res[[2]]), 7)
#
# })
#
# test_that("freq53: twoway across works.", {
#
#
#   res <- proc_freq(prt,
#                    tables = c("internship * enrollment"),
#                    titles = "My title",
#                    view = TRUE,
#                    across = "group",
#                    weight = "count",
#                    out5 = out(table = "internship * enrollment",
#                               stats = c("n", "cnt", "pct")),
#   )
#
#   res
#
#   # expect_equal(nrow(res[[1]]), 8)
#   # expect_equal(ncol(res[[1]]), 12)
#   # expect_equal(nrow(res[[2]]), 2)
#   # expect_equal(ncol(res[[2]]), 7)
#
# })
#
#
# test_that("freq54: oneway across with by works.", {
#
#
#   res <- proc_freq(prt,
#                    tables = c("internship"),
#                    titles = "My title",
#                    by = c("sex"),
#                    view = TRUE,
#                    across = "group",
#                    weight = "count",
#                    options = opts(fisher = TRUE),
#                    out5 = out(table = "internship",
#                               stats = c("n", "cnt", "pct"))
#   )
#
#   res
#
#   # expect_equal(nrow(res[[1]]), 8)
#   # expect_equal(ncol(res[[1]]), 12)
#   # expect_equal(nrow(res[[2]]), 2)
#   # expect_equal(ncol(res[[2]]), 7)
#
# })
#
# test_that("freq55: twoway across with by works.", {
#
#
#   res <- proc_freq(prt,
#                    tables = c("internship * enrollement"),
#                    titles = "My title",
#                    by = c("sex"),
#                    view = TRUE,
#                    across = "group",
#                    weight = "count",
#                    options = opts(fisher = TRUE),
#                    out5 = out(table = "internship * enrollment",
#                               stats = c("n", "cnt", "pct", "fisher"))
#   )
#
#   res
#
#   # expect_equal(nrow(res[[1]]), 8)
#   # expect_equal(ncol(res[[1]]), 12)
#   # expect_equal(nrow(res[[2]]), 2)
#   # expect_equal(ncol(res[[2]]), 7)
#
# })
#
#
# test_that("freq56: twoway across with statistics works.", {
#
#
#   res <- proc_freq(prt,
#                    tables = c("internship * enrollement"),
#                    titles = "My title",
#                    view = TRUE,
#                    across = "group",
#                    weight = "count",
#                    options = opts(fisher = TRUE),
#                    out5 = out(table = "internship * enrollment",
#                               stats = c("n", "cnt", "pct", "chisq"))
#   )
#
#   res
#
#   # expect_equal(nrow(res[[1]]), 8)
#   # expect_equal(ncol(res[[1]]), 12)
#   # expect_equal(nrow(res[[2]]), 2)
#   # expect_equal(ncol(res[[2]]), 7)
#
# })
#
# test_that("freq57: twoway across with by and statistics works.", {
#
#
#   res <- proc_freq(prt,
#                    tables = c("internship"),
#                    titles = "My title",
#                    by = c("sex"),
#                    view = TRUE,
#                    across = "group",
#                    weight = "count",
#                    options = opts(fisher = TRUE),
#                    out5 = out(table = "internship * enrollment",
#                               stats = c("n", "cnt", "pct", "chisq"))
#   )
#
#   res
#
#   # expect_equal(nrow(res[[1]]), 8)
#   # expect_equal(ncol(res[[1]]), 12)
#   # expect_equal(nrow(res[[2]]), 2)
#   # expect_equal(ncol(res[[2]]), 7)
#
# })
#
#
# test_that("freq52: combine stats works.", {
#
#
#   res <- proc_freq(prt2,
#                    tables = c("internship"),
#                    titles = "My title",
#                    by = "group",
#                    view = TRUE,
#                    across = "group",
#                    weight = "count",
#                    out5 = out(table = "internship",
#                               stats = c("n", "cnt & pct")))
#
#   res
#
#
#   # expect_equal(nrow(res[[1]]), 8)
#   # expect_equal(ncol(res[[1]]), 12)
#   # expect_equal(nrow(res[[2]]), 2)
#   # expect_equal(ncol(res[[2]]), 7)
#
# })




# test_that("freq35: CMH works with weight.", {
#
#
#   # fp <- file.path(base_path, "/data/treatment.csv")
#   # csv <- read.csv(fp)
#
#   mantelhaen.test()
#
# })

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
options("procs.print" = FALSE)
#options("procs.print" = NULL)

test_that("freq1: Simple proc_freq no output works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   output = "none",
                   titles = "My first Frequency Table")

  res

  expect_equal(is.null(res), TRUE)

})


test_that("freq2: Simple proc_freq with out works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   titles = "My first Frequency Table",
                   options = outcum)

  res

  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 7)

})


# test_that("freq2: proc_freq with label and format options works.", {
#
#
#   labels(dat) <- list(Eyes = "Eye Color",
#                       Hair = "Hair Color",
#                       Region = "Geographic Region")
#
#   res <- proc_freq(dat, tables = c("Eyes"),
#                    titles = "My first Frequency Table",
#
#                    out = out_spec(label = c(VAR = "Variable", CAT = "Category"),
#                              format = list(CUMPCT = "%.3f")))
#
#   res
#
#   # proc_print(res)
#
#   a1 <- attributes(res$VAR)
#   a2 <- attributes(res$CUMPCT)
#
#   expect_equal(nrow(res), 3)
#   expect_equal(ncol(res), 7)
#
#   expect_equal(a1$label, "Variable")
#   expect_equal(a2$format, "%.3f")
#
# })



test_that("freq3: Two table proc_freq test with output works.", {

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", HairCount = "Hair"),
                   titles = "My first Frequency Table",
                   options = outcum)

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


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   options = outcum)

  res


  expect_equal(res$CNT[1], 222)
  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 7)


})

test_that("freq5: Two var proc_freq with weight works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   options = outcum)

  res
  #ex <- file.exists(fl)


  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 7)
  expect_equal(res[[1]]$CNT[1], 222)
  expect_equal(res[[2]]$CNT[5], 113)

})


test_that("freq6: Simple proc_freq with output long works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   output = long)


  res

  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 5)


})

test_that("freq7: Simple proc_freq with 2 way works.", {



  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   weight = "Count",
                   titles = "My first Frequency Table")

  res

  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 7)

})


test_that("freq8: Simple proc_freq in multiple outputs works.", {

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat,
                   tables = v(out1 = Eyes, out2 = Hair, out3 = Eyes * Hair),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   options = outcum
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

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes"),
                   weight = "Count",
                   titles = "My first Frequency Table",
                   by = "Region",
                   options =  outcum)

  res

  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 8)

})


test_that("freq10: Two way proc_freq works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   options = "outcum",
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 9)

})


test_that("freq11: Two way proc_freq no weight works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c(FreqCount = "Eyes * Hair"),
                   titles = "Eye and Hair Color of European Children",
                   options = outcum)

  res

  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 9)

})

test_that("freq12: One way and two way proc_freq works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair", FreqCount = "Eyes * Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   options = outcum)

  res

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 7)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(ncol(res[[2]]), 7)
  expect_equal(nrow(res[[3]]), 15)
  expect_equal(ncol(res[[3]]), 9)


})




test_that("freq13: Nocum option work as expected.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   output = report,
                   options = nocum,
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CUMSUM" %in% d, FALSE)
  expect_equal("CUMPCT" %in% d, FALSE)


})


test_that("freq14: output = out on table.", {


  res <- proc_freq(dat, tables = c("Eyes", "Eyes * Hair"),
                   output = out,
                   options = noprint,
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal(d[1], "Eyes")
  expect_equal(d[2], "Eyes * Hair")


})


test_that("freq15: Outcum option works as expected.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   options = nocum,
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CUMSUM" %in% d, FALSE)
  expect_equal("CUMPCT" %in% d, FALSE)

  res <- proc_freq(dat, tables = c("Eyes"),
                   options = outcum,
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CUMSUM" %in% d, TRUE)
  expect_equal("CUMPCT" %in% d, TRUE)



})


test_that("freq16: Freq and Pct options works as expected.", {


  res <- proc_freq(dat, tables = c("Eyes"),
                   options = v(nofreq,
                               nocum),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CNT" %in% d, FALSE)
  expect_equal("PCT" %in% d, TRUE)

  res <- proc_freq(dat, tables = c("Eyes"),
                   options = v(nopercent, nocum),
                   titles = "Eye and Hair Color of European Children")

  res

  d <- names(res)


  expect_equal("CNT" %in% d, TRUE)
  expect_equal("PCT" %in% d, FALSE)



})


test_that("freq17: Sparse option works as expected.", {


  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   options = v(nosparse),
                   titles = "Eye and Hair Color of European Children")

  res


  expect_equal(nrow(res), 14)
  expect_equal(ncol(res), 7)

})

test_that("freq18: Crosstab works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   output = report)

  res

  expect_equal(nrow(res), 14)
  expect_equal(ncol(res), 8)

})

# Not sure what is wrong with this.  Should be working.
# test_that("freq19: Format options on table.", {
#
#
#   labels(dat) <- list(Eyes = "Eye Color",
#                       Hair = "Hair Color",
#                       Region = "Geographic Region")
#
#   fmt1 <- value(condition(is.na(x), ""),
#                 condition(TRUE, "%.3f%%"))
#
#   res <- proc_freq(dat, tables = c("Eyes * Hair"),
#                    options = v(format = fmt1, out),
#                    weight = "Count",
#                    titles = "Eye and Hair Color of European Children")
#
#   # Interactive test
#   expect_equal(TRUE, TRUE)
#
# })

test_that("freq20: SAS replication of one way tables works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair"),
                   titles = "Eye and Hair Color of European Children",
                   weight = "Count", options = outcum)

  res

  expect_equal(nrow(res[[1]]), 3)
  expect_equal(ncol(res[[1]]), 7)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(ncol(res[[2]]), 7)

})

test_that("freq21: Rowpct and Colpct options on table work.", {



  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes * Hair"),
                   output = report,
                   options = v(norow, nocol),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(nrow(res), 8)
  expect_equal(ncol(res), 8)


})

test_that("freq22: Crosstab option works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat, tables = c("Eyes", "Hair", "Eyes * Hair"),
                   options = v(crosstab),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal(nrow(res[[3]]), 14)
  expect_equal(ncol(res[[3]]), 8)

})

# test_that("freq23: proc_freq with drop, keep and rename options works.", {
#
#
#   labels(dat) <- list(Eyes = "Eye Color",
#                       Hair = "Hair Color",
#                       Region = "Geographic Region")
#
#   res <- proc_freq(dat, tables = c("Eyes"),
#                    titles = "My first Frequency Table",
#                    out = out_spec(drop = "CUMPCT",
#                              keep = c("CAT", "VAR", "N", "CNT", "PCT"),
#                              rename = c(VAR = "BLOCK")))
#
#   res
#
#   # proc_print(res)
#
#   expect_equal(nrow(res), 3)
#   expect_equal(ncol(res), 5)
#   expect_equal(names(res), c("BLOCK", "CAT", "N", "CNT", "PCT"))
#
# })

# test_that("freq24: proc_freq with where output option works.", {
#
#
#   labels(dat) <- list(Eyes = "Eye Color",
#                       Hair = "Hair Color",
#                       Region = "Geographic Region")
#
#   res <- proc_freq(dat, tables = c("Eyes"),
#                    titles = "My first Frequency Table",
#                    out = out_spec(where = expression(CAT == "green")))
#
#   res
#
#   # proc_print(res)
#
#
#   expect_equal(nrow(res), 1)
#   expect_equal(ncol(res), 7)
#
# })


test_that("freq25: Single by group on single table works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")


  res <- proc_freq(dat, tables = c("Eyes"), by = "Region",
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children")

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 6)
  expect_equal(typeof(res$BY), 'integer')

})

test_that("freq26: Single by group on double table works.", {


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
  expect_equal(typeof(res[[1]]$BY), 'integer')
  expect_equal(typeof(res[[2]]$BY), 'integer')

})


test_that("freq27: Double by group on double table works.", {


  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")


  res <- proc_freq(spdat, tables = c("Eyes", "Hair"), by = c("Sex", "Region"),
                   weight = "Count",
                   options = v(nosparse),
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
  expect_equal(ncol(res), 8)


})

test_that("freq30: Crosstab with by and report works.", {


  spdat <- dat

  spdat$Sex <- c(rep("M", 13), rep("F", 14))

  labels(spdat) <- list(Eyes = "Eye Color",
                        Hair = "Hair Color",
                        Region = "Geographic Region")

  res <- proc_freq(spdat, tables = c(FreqTable = "Eyes * Hair"),
                   by = c("Sex"),
                   weight = "Count",
                   titles = "Eye and Hair Color of European Children",
                   output = report)

  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 14)
  expect_equal(names(res)[1], "Sex=F, FreqTable")
  expect_equal(names(res)[2], "Sex=M, FreqTable")


})


test_that("freq31: Parameter checks work.", {


  expect_error(proc_freq(dat, tables = c("Fork", "Eyes", "Bork")))
  expect_error(proc_freq(dat, by = "Fork", tables = "Eye"))


})



test_that("freq32: chi sqr works with weight.", {


  # fp <- file.path(base_path, "/data/treatment.csv")
  # csv <- read.csv(fp)


  res <- proc_freq(prt, tables = "internship * enrollment",
                   output = report,
                   options = chisq,
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
                   output = report,
                   options = fisher,
                   weight = "count")

  res

  expect_equal(length(res), 2)

  res2 <- res[[2]]
  expect_equal(res2[1, 2], 50)
  expect_equal(res2[2, 2], 0.85127668)
  expect_equal(res2[3, 2], 0.22133142)
  expect_equal(res2[4, 2], 0.41215159)

  #expect_equal(nrow(res[[1]]), 14)

})

test_that("freq34: fisher's works with weight and by.", {

  res <- proc_freq(prt, tables = "internship * enrollment",
                   output = report,
                   options = v(fisher),
                   by = "sex",
                   weight = "count")

  res

  expect_equal(length(res), 4)

  res2 <- res[[2]]
  expect_equal(res2[1, 2], 27)
  expect_equal(res2[2, 2], 0.98846024)
  expect_equal(res2[3, 2], 0.03111341)
  expect_equal(res2[4, 2], 0.046665258)

  res4 <- res[[4]]
  expect_equal(res4[1, 2], 23)
  expect_equal(res4[2, 2], 0.83173972)
  expect_equal(res4[3, 2], 0.29935132)
  expect_equal(res4[4, 2], 0.524477809)

  #expect_equal(nrow(res[[1]]), 14)


  res <- proc_freq(prt, tables = "internship * enrollment",
                   options = v(fisher, list),
                   by = "sex",
                   weight = "count")

  res

  expect_equal(length(res), 2)

  res2 <- res[[2]]
  expect_equal(res2[1, 2], 27)
  expect_equal(res2[1, 3], 0.98846024)
  expect_equal(res2[1, 4], 0.03111341)
  expect_equal(res2[1, 5], 0.046665258)
  expect_equal(res2[2, 2], 23)
  expect_equal(res2[2, 3], 0.83173972)
  expect_equal(res2[2, 4], 0.29935132)
  expect_equal(res2[2, 5], 0.524477809)


})


test_that("freq35: chi sqr works with weight and by.", {


  # fp <- file.path(base_path, "/data/treatment.csv")
  # csv <- read.csv(fp)


  res <- proc_freq(prt, tables = "internship * enrollment",
                   output = report,
                   options = ChiSq,
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


  res <- proc_freq(prt, tables = "internship * enrollment",
                   options = ChiSq,
                   by = "sex",
                   weight = "count")

  res

  expect_equal(length(res), 2)

  res2 <- res[[2]]
  expect_equal(res2[1, 2], 4.23661395)
  expect_equal(res2[1, 3], 1)
  expect_equal(res2[1, 4], 0.039560993)
  expect_equal(res2[2, 2], 0.55926894)
  expect_equal(res2[2, 3], 1)
  expect_equal(res2[2, 4], 0.45455495)

})


test_that("freq36: 2 way table is sorted properly.", {



  res <- proc_freq(prt, tables = "internship * enrollment",
                   weight = "count",
                   output = "report")

  res
  expect_equal(res[9, 1], "Total")
  expect_equal(res[10, 1], "Total")
})



test_that("freq37: Crosstab works with factors.", {

  prt2 <- prt

  prt2$internship <- as.factor(prt2$internship)
  prt2$enrollment <- as.factor(prt2$enrollment)


  res <- proc_freq(prt2, tables = c("sex", FreqCounts = "internship * enrollment"),
                   output = "report",
                   weight = "count")

  res
  expect_equal(nrow(res[[1]]), 2)
  expect_equal(ncol(res[[1]]), 6)
  expect_equal(nrow(res[[2]]), 10)
  expect_equal(ncol(res[[2]]), 5)
  #expect_equal(nrow(res[[3]]), 4)
  #expect_equal(ncol(res[[3]]), 4)
})

test_that("freq38: get_output_specs works as expected.", {


  res1 <- get_output_specs(c("A", "B", "A * B"), list(), "", "")

  res1
  expect_equal(length(res1), 3)
  expect_equal(res1[[1]]$table, "A")
  expect_equal(res1[[2]]$table, "B")
  expect_equal(res1[[3]]$table, "A * B")


  res2 <- get_output_specs(c(tab1 = "A", "B", tab3 = "A * B"), list(), "", "")

  res2
  expect_equal(length(res2), 3)
  expect_equal(names(res2), c("tab1", "B", "tab3"))
  expect_equal(res2[[1]]$table, "A")
  expect_equal(res2[[2]]$table, "B")
  expect_equal(res2[[3]]$table, "A * B")

  ot <- list(out = out_spec(stats = c("n", "pct"), shape = "wide"))
  res3 <- get_output_specs(c(tab1 = "A", "B", tab3 = "A * B"), ot, "", "")

  res3
  expect_equal(length(res3), 3)
  expect_equal(names(res3), c("tab1", "B", "tab3"))
  expect_equal(res3[[1]]$table, "A")
  expect_equal(res3[[2]]$table, "B")
  expect_equal(res3[[3]]$table, "A * B")



  ot <- list(out1 = out_spec(table = "A", stats = c("n", "pct"), shape = "wide"),
             out2 = out_spec(table = "B", stats = c("n", "pct"), shape = "wide"),
             out3 = out_spec(table = "A * B", stats = c("n", "pct"), shape = "wide")

             )
  res4 <- get_output_specs(NULL, ot, "", "")

  res4
  expect_equal(length(res4), 3)
  expect_equal(names(res4), c("out1", "out2", "out3"))
  expect_equal(res4[[1]]$table, "A")
  expect_equal(res4[[2]]$table, "B")
  expect_equal(res4[[3]]$table, "A * B")


  ot <- list(out1 = out_spec(stats = c("n", "pct"), shape = "wide"),
             out2 = out_spec(table = "A * B", stats = c("chisq"), shape = "wide")

  )
  res5 <- get_output_specs(c(tab1 = "A", "B", tab3 = "A * B"), ot, "", "")

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
  expect_equal(ncol(res1), 6)

  res2 <- get_output_oneway(prt, "internship", "count", NULL, by = c(am = "A",
                                                                     pm = "B"))

  res2


  expect_equal(nrow(res2), 2)
  expect_equal(ncol(res2), 7)

})


test_that("freq40: get_output_oneway() long works as expected.", {


  res1 <- get_output_oneway(prt, "internship", "count", NULL, by = c(am = 1),
                            shape = "long")

  res1


  expect_equal(nrow(res1), 3)
  expect_equal(ncol(res1), 5)

  res2 <- get_output_oneway(prt, "internship", "count", NULL,
                            by = c(am = "A", pm = "B"), shape = "long")

  res2


  expect_equal(nrow(res2), 3)
  expect_equal(ncol(res2), 6)

})

test_that("freq41: get_output_twoway() works as expected.", {


  res1 <- get_output_twoway(prt, "internship", "enrollment", "count", NULL,
                            FALSE, by = c(by1 = 1), shape = "wide")

  res1

  expect_equal(nrow(res1), 4)
  expect_equal(ncol(res1), 9)

  res2 <- get_output_twoway(prt, "internship", "enrollment", "count",  NULL,
                            FALSE, by = c(by1 = "A", by2 = "B"))

  res2


  expect_equal(nrow(res2), 4)
  expect_equal(ncol(res2), 10)

})

test_that("freq42: get_output_twoway() long works as expected.", {


  res1 <- get_output_twoway(prt, "internship", "enrollment", "count", NULL,
                            FALSE, by = c(by1 = 1), shape = "long")

  res1

  expect_equal(nrow(res1), 4)
  expect_equal(ncol(res1), 8)

  res2 <- get_output_twoway(prt, "internship", "enrollment", "count",  NULL,
                            FALSE, by = c(by1 = "A", by2 = "B"),
                            shape = "long")

  res2


  expect_equal(nrow(res2), 4)
  expect_equal(ncol(res2), 9)

})



test_that("freq43: oneway output statistics work.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat,
                   tables = c("Eyes", "Hair"),
                   output = long,
                   titles = "My first Frequency Table",
                   weight = "Count")

  res

  expect_equal(length(res), 2)
  expect_equal(names(res[[1]]), c("VAR", "STAT", "blue", "brown", "green"))
  expect_equal(nrow(res[[1]]), 3)
  expect_equal(nrow(res[[2]]), 3)
  expect_equal(res[[2]]$STAT, c("N", "CNT", "PCT"))

})

test_that("freq45: twoway output statistics work.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat,
                   tables = c("Eyes", "Region * Eyes", "Region"),
                   titles = "My first Frequency Table",
                   output = long,
                   options = outcum,
                   weight = "Count")

  res

  expect_equal(length(res), 3)
  expect_equal(names(res[[1]]), c("VAR", "STAT", "blue", "brown", "green"))
  expect_equal(nrow(res[[1]]), 5)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(res[[2]]$STAT, c("N", "CNT", "PCT", "CUMSUM", "CUMPCT"))

})

test_that("freq46: output parameter works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")


  res <- proc_freq(dat,
                   tables = c("Eyes", "Region * Eyes", "Region"),
                   titles = "My first Frequency Table",
                   weight = "Count",
                   output = long,
                   options = outcum)


  res

  expect_equal(length(res), 3)
  expect_equal(names(res[[1]]), c("VAR", "STAT", "blue", "brown", "green"))
  expect_equal(nrow(res[[1]]), 5)
  expect_equal(nrow(res[[2]]), 5)
  expect_equal(res[[2]]$STAT, c("N", "CNT", "PCT", "CUMSUM", "CUMPCT"))

})


test_that("freq47: output report works.", {


  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat,
                   tables = c("Eyes", "Hair",  Cross = "Hair * Eyes"),
                   titles = "My first Frequency Table",
                   by = "Region",
                   weight = "Count",
                   output = report)


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
                   weight = "Count",
                   output = v(stacked))


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
#                    report = out_spec(stats = c("n", "cnt", "pct"), shape = "stacked"))
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
                   weight = "count",
                   options = "chisq")


  res

  expect_equal(length(res), 2)
  expect_equal(nrow(res[[1]]), 8)
  expect_equal(ncol(res[[1]]), 8)
  expect_equal(nrow(res[[2]]), 2)
  expect_equal(ncol(res[[2]]), 4)


})


test_that("freq51: fisher output statistics works.", {


  res <- proc_freq(prt,
                   tables = c("internship * enrollment"),
                   titles = "My title",
                   by = c("sex"),
                   weight = "count",
                   options = v(fisher)
  )

  res

  expect_equal(nrow(res[[1]]), 8)
  expect_equal(ncol(res[[1]]), 8)
  expect_equal(nrow(res[[2]]), 2)
  expect_equal(ncol(res[[2]]), 5)

})

test_that("freq52: Logging function works.", {

  res <- log_freq(mtcars, tables = c("mpg", "cyl"),
                  weight = "count",
                  by = "cyl", outcnt = 6)

  res

  expect_equal(length(res), 6)

})


test_that("freq53: error on unknown parameter.", {


  expect_error(proc_freq(prt2,
                   tables = c("internship"),
                   titles = "My first Frequency Table",
                   by = c("sex", "enrollment"),
                   fork = TRUE,
                   weight = "count"))
})



# test_that("freq54: where works before and after rename.", {
#
#
#   res <- proc_freq(prt2,
#                    tables = c("internship"),
#                    titles = "My first Frequency Table",
#                    by = c("sex", "enrollment"),
#                    weight = "count",
#                    out = out_spec(rename = list(BY2 = "Enrollment"),
#                              where = expression(Enrollment == "no")))
#
#   res
#
#   expect_equal(nrow(res), 4)
#
#   res <- proc_freq(prt2,
#                    tables = c("internship"),
#                    titles = "My first Frequency Table",
#                    by = c("sex", "enrollment"),
#                    weight = "count",
#                    out = out_spec(rename = list(BY2 = "Enrollment"),
#                              where = expression(BY2 == "no")))
#
#   res
#
#   expect_equal(nrow(res), 4)
#
#
#
# })



test_that("freq56: get_table_list() works as expected.", {

  vars <- c("A", "B", "A * B", "A * C")

  res <- get_table_list(vars)

  res
  expect_equal(length(res), 4)
  expect_equal(res[[3]], c("A", "B"))

})


test_that("freq56: get_output_tables() works as expected.", {

  lst <- list(out1 = out_spec(table = "A"),
              out2 = out_spec(table = "B"),
              out3 = out_spec(table = "A * B"),
              out4 = out_spec(table = "A * C"))

  res <- get_output_tables(lst)

  res

  expect_equal(length(res), 4)
  expect_equal(res[[3]], "A * B")


})


test_that("freq55: get_nway_zero_fills() works as expected.", {

  lst <- list(out1 = out_spec(table = "x"),
              out2 = out_spec(table = "y"),
              out3 = out_spec(table = "x * y"))


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


  lst2 <- list(out1 = out_spec(table = "x"))

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
                   weight = "count")

  res

  expect_equal(ncol(res), 7)
  expect_equal(nrow(res), 8)


  res <- proc_freq(sp,
                   tables = c("internship"),
                   titles = "My first Frequency Table",
                   by = c("sex", "enrollment"),
                   weight = "count",
                   output = report)

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
                   weight = "count",
                   output = out)

  res

  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), 8)


  res <- proc_freq(sp,
                   tables = c("internship * enrollment"),
                   titles = "My first Frequency Table",
                   by = c("sex"),
                   weight = "count",
                   output = report)

  res

  expect_equal(length(res), 2)
  expect_equal(ncol(res[[2]]), 5)
  expect_equal(nrow(res[[2]]), 10)

})


test_that("freq53: notable option works as expected.", {


  res <- proc_freq(prt,
                   tables = c("internship * enrollment"),
                   titles = "My title",
                   by = c("sex"),
                   weight = "count",
                   options = v(fisher, notable)
  )

  res


  expect_equal(ncol(res), 5)
  expect_equal(nrow(res), 2)


})

test_that("freq54: nopercent works on two-way.", {

  res <- proc_freq(dat, tables = Eyes * Hair,
            options = v(nocol, norow, crosstab, nopercent))


  res
  expect_equal(ncol(res), 8)
  expect_equal(nrow(res), 4)
})

test_that("freq55: get_nlevels works as expected.", {


  res <- get_nlevels(dat, "Eyes")

  expect_equal(nrow(res), 1)
  expect_equal(res$stub[1], "Eyes")
  expect_equal(res$levels[1], 3)

  res <- get_nlevels(dat, "Eyes", "Hair")

  res
  expect_equal(nrow(res), 2)
  expect_equal(res[["stub"]][1], "Eyes")
  expect_equal(res$levels[1], 3)
  expect_equal(res$levels[2], 5)


  bv <- c("Region" = 1)

  res <- get_nlevels(dat, "Eyes", NULL, byvars = bv)

  res
  attributes(res)
  expect_equal(nrow(res), 1)
  expect_equal(res$stub[1], "Eyes")
  expect_equal(res$levels[1], 3)
  expect_equal(is.null(attributes(res)), FALSE)


  res <- get_nlevels(dat, "Eyes", "Hair", byvars = bv, out = TRUE)

  res
  expect_equal(nrow(res), 1)
  expect_equal(res[["VAR1"]][1], 3)
  expect_equal(res$VAR2[1], 5)
  expect_equal(labels(res), list(VAR1 = "Eyes", VAR2 = "Hair" ))


})


test_that("freq56: nlevels works as expected.", {

  res <- proc_freq(dat, tables = Eyes,
                   output = report,
                   options = nlevels)


  res

  expect_equal(length(res), 2)
  expect_equal(ncol(res[[1]]), 2)
  expect_equal(nrow(res[[1]]), 1)


  res <- proc_freq(dat, tables = v(Eyes, Hair),
                   output = report,
                   options = nlevels)


  res

  expect_equal(length(res), 4)
  expect_equal(ncol(res[[1]]), 2)
  expect_equal(nrow(res[[1]]), 1)


})


test_that("freq57: get_nlevels missing option works.", {

  prtm <- read.table(header = TRUE, text = '
  sex internship enrollment count
  1  boys        yes        yes    35
  2  boys         no        yes    14
  3 girls        yes        yes    32
  4 girls         no        yes    53
  5  boys        yes         no    29
  6  boys         no         no    27
  7 girls        yes         no    10
  8 girls         no         no    23
  9 NA            NA        yes   25')


  res <- get_nlevels(prt, "sex", missing = TRUE)

  res
  expect_equal(res$MISS[1], 0)

  res <- get_nlevels(prtm, "sex", missing = TRUE)

  res

  expect_equal(res$MISS[1], 1)



  res <- get_nlevels(prt, "sex", missing = TRUE, out = TRUE)

  res
  expect_equal(res$MISS[1], 0)

  res <- get_nlevels(prtm, "sex", missing = TRUE, out = TRUE)

  res

  expect_equal(res$MISS[1], 1)


  res <- get_nlevels(prtm, "internship", "enrollment", missing = TRUE, out = FALSE)

  res

  expect_equal(res$MISS[1], 1)
  expect_equal(res$MISS[2], 0)



  res <- get_nlevels(prtm, "internship", "enrollment", missing = TRUE, out = TRUE)

  res

  expect_equal(res$VAR1.MISS[1], 1)
  expect_equal(res$VAR2.MISS[1], 0)



  res <- get_nlevels(prtm, "internship", "enrollment", byvars = "sex = 1",
                     missing = TRUE, out = FALSE)

  res

  expect_equal(res$MISS[1], 1)
  expect_equal(res$MISS[2], 0)



  res <- get_nlevels(prtm, "internship", "enrollment", missing = TRUE, out = TRUE)

  res

  expect_equal(res$VAR1.MISS[1], 1)
  expect_equal(res$VAR2.MISS[1], 0)


})

test_that("freq58: proc_freq missing option works.", {

  prtm <- read.table(header = TRUE, text = '
  sex internship enrollment count
  1  boys        yes        yes    35
  2  boys         no        yes    14
  3 girls        yes        yes    32
  4 girls         no        yes    53
  5  boys        yes         NA    29
  6  boys         no         no    27
  7 girls        yes         no    10
  8 girls         no         no    23
  9 girls            NA        yes   25
  10 girls           NA        no   29')


  res <- proc_freq(prtm, tables = v(internship),
                   options = v(nlevels, missing))


  res$tab2

  r1 <- res[[1]]
  expect_equal(length(res), 2)
  expect_equal(r1$VAR[1], 3)
  expect_equal(r1$MISS[1], 1)
  expect_equal(r1$NONMISS[1], 2)


  res <- proc_freq(prtm, tables = internship * enrollment,
                   options = v(nlevels, missing))

  res

  r1 <- res[[1]]
  expect_equal(length(res), 2)
  expect_equal(r1$VAR2[1], 3)
  expect_equal(r1$VAR2.MISS[1], 1)
  expect_equal(r1$VAR2.NONMISS[1], 2)



})

test_that("freq59: chi sqr works without weight.", {

  res <- proc_freq(prt, tables = "internship * enrollment",
                   output = report,
                   options = chisq)

  res

  expect_equal(length(res), 2)

  res2 <- res[[2]]
  expect_equal(res2[1, 2], 0.0)
  expect_equal(res2[2, 2], 1)
  expect_equal(res2[3, 2], 1.0)

})


test_that("freq60: fisher's works without weight.", {

  res <- proc_freq(prt, tables = "internship * enrollment",
                   output = report,
                   options = fisher)

  res

  expect_equal(length(res), 2)

  res2 <- res[[2]]
  expect_equal(res2[1, 2], 2)
  expect_equal(res2[2, 2], 0.75714286)
  expect_equal(res2[3, 2], 0.75714286)
  expect_equal(res2[4, 2], 1)

  #expect_equal(nrow(res[[1]]), 14)

})

test_that("freq61: factors and ordering with crosstab output works.", {


  res1 <- proc_freq(dat, tables = c("Eyes", "Hair", comb = "Eyes * Hair"),
                   output = out,
                   titles = "Eye and Hair Color of European Children")

  res1


  datsp <- dat

  datsp$Eyes <- factor(dat$Eyes, levels = c("green", "brown", "blue"))
  datsp$Hair <- factor(dat$Hair, levels = c("fair", "medium", "red", "dark", "black"))


  res2 <- proc_freq(datsp, tables = c("Eyes", "Hair", comb = "Eyes * Hair"),
                    output = out,
                    titles = "Eye and Hair Color of European Children")

  res2


  expect_equal(as.character(res1$Eyes$CAT), c("blue", "brown", "green"))
  expect_equal(as.character(res2$Eyes$CAT), c("green", "brown", "blue"))
  expect_equal(unique(as.character(res1$comb$CAT1)), c("blue", "brown", "green"))
  expect_equal(unique(as.character(res2$comb$CAT1)), c("green", "brown", "blue"))

})

test_that("freq62: factors and ordering with list output works.", {


  res1 <- proc_freq(dat, tables = c("Eyes", "Hair", comb = "Eyes * Hair"),
                    output = out,
                    options = list,
                    titles = "Eye and Hair Color of European Children")

  res1


  datsp <- dat

  datsp$Eyes <- factor(dat$Eyes, levels = c("green", "brown", "blue"))
  datsp$Hair <- factor(dat$Hair, levels = c("fair", "medium", "red", "dark", "black"))


  res2 <- proc_freq(datsp, tables = c("Eyes", "Hair", comb = "Eyes * Hair"),
                    output = out,
                    options = list,
                    titles = "Eye and Hair Color of European Children")

  res2


  expect_equal(as.character(res1$Eyes$CAT), c("blue", "brown", "green"))
  expect_equal(as.character(res2$Eyes$CAT), c("green", "brown", "blue"))
  expect_equal(as.character(res1$Hair$CAT), c("black", "dark", "fair", "medium", "red"))
  expect_equal(as.character(res2$Hair$CAT), c("fair", "medium", "red", "dark", "black"))
  expect_equal(unique(as.character(res1$comb$CAT1)), c("blue", "brown", "green"))
  expect_equal(unique(as.character(res2$comb$CAT1)), c("green", "brown", "blue"))
  expect_equal(unique(as.character(res1$comb$CAT2)),  c("black", "dark", "fair", "medium", "red"))
  expect_equal(unique(as.character(res2$comb$CAT2)), c("fair", "medium", "red", "dark", "black"))


})


test_that("freq63: totals always end up at bottom.", {



  datsp <- dat

  datsp$Eyes <- sub("blue", "zed", datsp$Eyes, fixed = TRUE)

  res1 <- proc_freq(datsp, tables = c("Eyes", "Hair", comb = "Eyes * Hair"),
                    output = out,
                    titles = "Eye and Hair Color of European Children")

  res1

  expect_equal(as.character(res1$Eyes$CAT), c("brown", "green", "zed"))
  expect_equal(unique(as.character(res1$comb$CAT1)), c("brown", "green","zed"))


  datsp$Eyes <- factor(datsp$Eyes, levels = c("green", "zed", "brown"))
  datsp$Hair <- factor(datsp$Hair, levels = c("fair", "medium", "red", "dark", "black"))


  res2 <- proc_freq(datsp, tables = c("Eyes", "Hair", comb = "Eyes * Hair"),
                    output = out,
                    titles = "Eye and Hair Color of European Children")

  res2


  expect_equal(as.character(res2$Eyes$CAT), c("green", "zed", "brown"))
  expect_equal(unique(as.character(res2$comb$CAT1)), c("green", "zed", "brown"))

})

test_that("freq64: Fisher with sort works as expected.", {

  prtsp <- prt

  prtsp$enrollment <- factor(prtsp$enrollment, c("yes", "no"))
  prtsp$internship <- factor(prtsp$internship, c("yes", "no"))

  res <- proc_freq(prtsp, tables = c(comb = "internship * enrollment"),
            options = Fisher,
            by = "sex",
            weight = "count")

  expect_equal(res$`fisher:comb`$FISHER.1.1[1], 35)
  expect_equal(res$`fisher:comb`$FISHER.1.1[2], 32)
  expect_equal(res$`fisher:comb`$FISHER.LS[1], 0.98846024)
  expect_equal(res$`fisher:comb`$FISHER.LS[2], 0.83173972)
  expect_equal(res$`fisher:comb`$FISHER.RS[1], 0.03111341)
  expect_equal(res$`fisher:comb`$FISHER.RS[2], 0.29935132)

})

test_that("freq65: Fisher without sort works as expected.", {

  res <- proc_freq(prt, tables = c(comb = "internship * enrollment"),
                   options = Fisher,
                   by = "sex",
                   weight = "count")

  expect_equal(res$`fisher:comb`$FISHER.1.1[1], 27)
  expect_equal(res$`fisher:comb`$FISHER.1.1[2], 23)
  expect_equal(res$`fisher:comb`$FISHER.LS[1], 0.98846024)
  expect_equal(res$`fisher:comb`$FISHER.LS[2], 0.83173972)
  expect_equal(res$`fisher:comb`$FISHER.RS[1], 0.03111341)
  expect_equal(res$`fisher:comb`$FISHER.RS[2], 0.29935132)

})

test_that("freq66: nonobs keyword works as expected.", {

  res <- proc_freq(prt, tables = v(internship),
                   options = v(nonobs),
                   weight = "count")


  expect_equal("N" %in% names(res), FALSE)

})


test_that("freq67: factor with sparse show zero counts.", {


  datsp <- dat

  datsp$Eyes <- ifelse(datsp$Eyes == "green", "brown", datsp$Eyes)


  datsp$Eyes <- factor(datsp$Eyes, levels = c("green", "brown", "blue"))


  res1 <- proc_freq(datsp, tables = c("Eyes"),
                    output = out,
                    options = nosparse,
                    titles = "Eye and Hair Color of European Children")

  res1


  res2 <- proc_freq(datsp, tables = c("Eyes"),
                    output = out,
                    options = sparse,
                    titles = "Eye and Hair Color of European Children")

  res2

  expect_equal(as.character(res1$CAT), c("brown", "blue"))
  expect_equal(as.character(res2$CAT), c("green", "brown", "blue"))


})


test_that("freq68: factors with by work.", {


  datsp <- dat

  datsp$Eyes <- ifelse(datsp$Eyes == "green", "brown", datsp$Eyes)


  datsp$Eyes <- factor(datsp$Eyes, levels = c("green", "brown", "blue"))


  res1 <- proc_freq(datsp, tables = c("Hair"),
                    output = out,
                    by = "Eyes",
                    options = nosparse,
                    titles = "Eye and Hair Color of European Children")

  res1


  res2 <- proc_freq(datsp, tables = c("Hair"),
                    output = out,
                    by = "Eyes",
                    options = sparse,
                    titles = "Eye and Hair Color of European Children")

  res2

  expect_equal(unique(as.character(res1$BY)), c("brown", "blue"))
  expect_equal(unique(as.character(res2$BY)), c("green", "brown", "blue"))
  expect_equal(class(res1$BY), 'factor')
  expect_equal(class(res2$BY), 'factor')

})

test_that("freq68: var and by as factors work.", {


  datsp <- dat

  datsp$Eyes <- ifelse(datsp$Eyes == "green", "brown", datsp$Eyes)
  datsp$Eyes <- factor(datsp$Eyes, levels = c("green", "brown", "blue"))
  datsp$Hair <- ifelse(datsp$Hair == "fair", "black", datsp$Hair)
  datsp$Hair <- factor(datsp$Hair, levels = c("red", "medium", "fair", "dark", "black"))


  res1 <- proc_freq(datsp, tables = c("Hair"),
                    output = out,
                    by = "Eyes",
                    options = nosparse,
                    titles = "Eye and Hair Color of European Children")

  res1


  res2 <- proc_freq(datsp, tables = c("Hair"),
                    output = out,
                    by = "Eyes",
                    options = sparse,
                    titles = "Eye and Hair Color of European Children")

  res2

  expect_equal(unique(as.character(res1$BY)), c("brown", "blue"))
  expect_equal(unique(as.character(res1$CAT)), c("red", "medium", "dark", "black"))
  expect_equal(unique(as.character(res2$BY)), c("green", "brown", "blue"))
  expect_equal(unique(as.character(res2$CAT)), c("red", "medium", "fair", "dark", "black"))


})


# test_that("freq67: outexpect keyword works as expected.", {
#
#   res <- proc_freq(dat, tables = v(Eyes * Hair),
#                    options = v(outexpect, list),
#                    weight = "Count")
#
#   sum(res$CNT)
#
#   expect_equal("N" %in% names(res), FALSE)
#
# })

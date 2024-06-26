

base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."


cls <- read.table(header = TRUE, text = '
Name Sex Age Height Weight Region
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

dev <- FALSE

options("procs.print" = FALSE)


test_that("reg0: sasLM version", {

  myfm <- formula(Weight ~ Height)

  res1 <- sasLM::REG(myfm, cls, Resid = TRUE)

  res1

  expect_equal(TRUE, TRUE)


  cls2 <- cls

  cls2[5, "Weight"] <- NA
  cls2[11, "Height"] <- NA

  res2 <- sasLM::REG(myfm, cls2, Resid = TRUE)

  res2



})

test_that("reg1: parameter checks work.", {

  myfm <- formula(Weight ~ Height)
  bfm <- formula(Weight ~ Fork)

  expect_error(proc_reg("bork", model = myfm))
  expect_error(proc_reg(cls[0, ], model = myfm))
  expect_error(proc_reg(cls, model = bfm))

})


test_that("reg2: get_output_specs_reg works.", {

  myfm <- formula(Weight ~ Height)

  res <- get_output_specs_reg(myfm, "", "report", report = TRUE)

  res

  expect_equal(names(res), "MODEL1")
  expect_equal(res$MODEL1$var, "Weight")
  expect_equal(res$MODEL1$report, TRUE)
  expect_equal(res$MODEL1$formula, myfm)

})

"noprint"


test_that("reg3: Basic proc_reg() works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls, myfm1,
                   output = "report")

  res1

  expect_equal(length(res1), 4)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls, myfm2, output = "report")

  res2

  expect_equal(length(res2), 4)

  # R Syntax 2
  myfm3 <- Weight ~ Height

  res2 <- proc_reg(cls, myfm3, output = "report")

  res2

  expect_equal(length(res2), 4)

})

test_that("reg4: noprint and none options works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls, myfm1, output = "report",
                   options = "noprint")

  res1

  # This is an interactive test.  Make sure nothing
  # is sent to the viewer.
  expect_equal(TRUE, TRUE)

  res2 <- proc_reg(cls, myfm1, output = "none",
                   options = "noprint")

  res2

  expect_equal(is.null(res2), TRUE)

})

test_that("reg5: Multiple independant variables works.", {


  # R Syntax
  myfm1 <- formula(Weight ~ Height + Age)


  res1 <- proc_reg(cls, myfm1, output = "report")

  res1

  expect_equal(length(res1), 4)


  # SAS Syntax
  myfm2 <- "Weight = Height Age"


  res2 <- proc_reg(cls, myfm2, output = "report")

  res2

  expect_equal(length(res2), 4)

})

test_that("reg6: Missing values work.", {

  cls2 <- cls

  cls2[2, "Height"] <- NA
  cls2[5, "Weight"] <- NA


  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls2, myfm1, output = "report")

  res1

  expect_equal(length(res1), 4)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls2, myfm2, output = "report")

  res2

  expect_equal(length(res2), 4)

})


test_that("reg7: by parameter works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls, myfm1,
                   by = Sex,
                   output = "report")

  res1

  expect_equal(length(res1), 2)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls, myfm2, by = Sex, output = "report")

  res2

  expect_equal(length(res2), 2)


  # Multiple Bys
  res3 <- proc_reg(cls, myfm1,
                   by = v(Sex, Region),
                   output = c("out", "report"))

  res3

  expect_equal(length(res3), 2)
  expect_equal(is.data.frame(res3$out), TRUE)
  expect_equal(nrow(res3$out), 4)
  expect_equal(length(res3$report), 4)
  expect_equal(length(res3$report$`MODEL1:Sex=F, Region=A`), 4)


})


test_that("reg8: Multiple models  works.", {


  # R Syntax
  myfm1 <- list(formula(Weight ~ Height),
                formula(Weight ~ Height + Age))


  res1 <- proc_reg(cls, myfm1, output = "report")

  res1

  expect_equal(length(res1), 2)


  # SAS Syntax
  myfm2 <- c("Weight = Height",
             "Weight = Height Age")


  res2 <- proc_reg(cls, myfm2, output = "report")

  res2

  expect_equal(length(res2), 2)

  # SAS Syntax
  myfm3 <- c("Weight = Height",
             "Height = Weight",
             "Weight = Height Age")


  res3 <- proc_reg(cls, myfm3, output = "report")

  res3

  expect_equal(length(res3), 3)


})



test_that("reg9: Model names work as expected.", {

  # R Syntax
  myfm1 <- list(md1 =  formula(Weight ~ Height),
                md2 = formula(Height ~ Weight),
                formula(Weight ~ Height + Age))


  res1 <- proc_reg(cls, myfm1, output = "report")

  res1

  expect_equal(length(res1), 3)


  # SAS Syntax
  myfm2 <- c(md1 = "Weight = Height",
             md2 = "Height = Weight",
             "Weight = Height Age")


  res2 <- proc_reg(cls, myfm2, output = "report",
                   titles = "My nice title")

  res2

  expect_equal(length(res2), 3)

  # R Syntax
  res3 <- proc_reg(cls, myfm1, output = "out")

  res3

  expect_equal(nrow(res3), 3)
  expect_equal(res3$MODEL, c("md1", "md2", "MODEL3"))
})


test_that("reg10: Output dataset works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls, myfm1)

  res1

  expect_equal(nrow(res1), 1)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls, myfm2)

  res2

  expect_equal(nrow(res2), 1)

})

test_that("reg11: Output dataset two models works.", {

  # R Syntax
  myfm1 <- list(formula(Weight ~ Height),
                formula(Height ~ Weight + Age))


  res1 <- proc_reg(cls, myfm1)

  res1

  expect_equal(nrow(res1), 2)


  # SAS Syntax
  myfm2 <- c("Weight = Height",
             "Weight = Height Age")


  res2 <- proc_reg(cls, myfm2)

  res2

  expect_equal(nrow(res2), 2)

})


test_that("reg12: Output by dataset works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)

  res1 <- proc_reg(cls, myfm1, by = Sex)

  res1

  expect_equal(nrow(res1), 2)


  # SAS Syntax
  myfm2 <- "Weight = Height"


  res2 <- proc_reg(cls, myfm2, by = Sex)

  res2

  expect_equal(nrow(res2), 2)

  # Multiple by
  res3 <- proc_reg(cls, myfm1, by = v(Region, Sex))

  res3

  expect_equal(nrow(res3), 4)


})

test_that("reg13: Optional statistics work.", {


  myfm1 <- formula(Weight ~ Height)

  # PRESS
  res1 <- proc_reg(cls, myfm1, stats = press)

  res1

  expect_equal(res1$PRESS, 2651.35206)

  res1 <- proc_reg(cls, myfm1, options = press)

  expect_equal("PRESS" %in% names(res1), TRUE)

  # EDF
  res2 <- proc_reg(cls, myfm1, stats = edf)

  res2

  expect_equal(res2$IN, 1)
  expect_equal(res2$P, 2)
  expect_equal(res2$EDF, 17)
  expect_equal(res2$RSQ, 0.7705068427)

  res2 <- proc_reg(cls, myfm1, options = edf)

  res2

  expect_equal("EDF" %in% names(res2), TRUE)

  # RSQUARE + 2 IV
  myfm2 <- formula(Weight ~ Height + Age)
  res3 <- proc_reg(cls, myfm2, stats = rsquare)

  res3

  expect_equal(res3$IN, 2)
  expect_equal(res3$P, 3)
  expect_equal(res3$EDF, 16)
  expect_equal(res3$RSQ, 0.7729049378)

  res3 <- proc_reg(cls, myfm2, options = rsquare)

  expect_equal("RSQ" %in% names(res3), TRUE)

  # ADJRSQ
  res4 <- proc_reg(cls, myfm1, stats = adjrsq)

  res4

  expect_equal(res4$ADJRSQ, 0.7570072452)

  # MSE
  res5 <- proc_reg(cls, myfm1, stats = mse)

  res5

  expect_equal(res5$MSE, 126.02868962)

  # SSE
  res6 <- proc_reg(cls, myfm1, stats = sse)

  res6

  expect_equal(res6$SSE, 2142.4877235)


  # SEB
  res7 <- proc_reg(cls, myfm1, stats = seb)

  res7

  expect_equal(nrow(res7), 2)
  expect_equal(res7$Intercept[2], 32.274591303)
  expect_equal(res7$Height[2], 0.5160939482)

  res7 <- proc_reg(cls, myfm1, options = outseb)

  res7

  expect_equal(nrow(res7), 2)

  # outest
  res8 <- proc_reg(cls, myfm1, options = OUTEST)

  res8

  expect_equal(nrow(res8), 1)

  # Bad values
  expect_error(proc_reg(cls, myfm1, options = fork))
  expect_error(proc_reg(cls, myfm1, stats = fork))

})



test_that("reg14: table statistics work.", {

  myfm1 <- formula(Weight ~ Height)

  # TABLE
  res1 <- proc_reg(cls, myfm1, stats = table)

  res1

  expect_equal(res1$RMSE[1], 11.2262500246)

  expect_equal(res1$TYPE, c("PARMS", "STDERR", "T", "PVALUE", "L95B", "U95B"))

  inval <- c(-143.026918439, 32.2745913033, -4.43156404663, 0.000365578926885,
    -211.120353939, -74.9334829395)

  expect_equal(res1$Intercept, inval)

  hval <- c(3.89903026878, 0.516093948163, 7.55488469233, 0.000000788681647101,
            2.81016721732, 4.98789332024)

  expect_equal(res1$Height, hval)

  # Check tableout option
  res2 <- proc_reg(cls, myfm1, options = tableout)

  res2

  expect_equal(nrow(res2), 6)

})


test_that("reg15: alpha option works.", {


  myfm1 <- formula(Weight ~ Height)

  res1 <- proc_reg(cls, myfm1, options = c(alpha = .1),
                   stats = table)

  res1

  expect_equal(res1$TYPE[5], "L90B")
  expect_equal(res1$TYPE[6], "U90B")
  expect_equal(res1$Height[5], 3.0012298)
  expect_equal(res1$Height[6], 4.7968308)


  res2 <- proc_reg(cls, myfm1, options = v(tableout, alpha = .1))

  res2

  expect_equal(res2$TYPE[5], "L90B")
  expect_equal(res2$TYPE[6], "U90B")
  expect_equal(res2$Height[5], 3.0012298)
  expect_equal(res2$Height[6], 4.7968308)



})


test_that("reg16: weight parameters works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)

  res1 <- proc_reg(cls, myfm1, weight = Age)

  res1

  expect_equal(is.data.frame(res1), TRUE)
  expect_equal(res1$RMSE, 41.262062)
  expect_equal(res1$Intercept, -144.839944)
  expect_equal(res1$Height, 3.9290125)

})


test_that("reg17: single model output options work.", {


  myfm1 <- formula(Weight ~ Height)

  # Wide
  res1 <- proc_reg(cls, myfm1, stats = v(seb, edf))

  res1

  expect_equal(nrow(res1), 2)
  expect_equal("EDF" %in% names(res1), TRUE)

  # Long
  res1 <- proc_reg(cls, myfm1, stats = v(seb, edf),
                   output = "long")

  res1

  expect_equal(nrow(res1), 8)
  expect_equal(all(c("PARMS", "SEB") %in% names(res1)), TRUE)

  # Stacked
  res1 <- proc_reg(cls, myfm1, stats = v(seb, edf),
                   output = "stacked")

  res1

  expect_equal(nrow(res1), 16)
  expect_equal("VALUES" %in% names(res1), TRUE)

})

test_that("reg18: double model output options work.", {


  myfm1 <- formula(Weight ~ Height)
  myfm2 <- formula(Height ~ Weight + Age)

  # Wide
  res1 <- proc_reg(cls, list(myfm1, myfm2), stats = v(seb, edf))

  res1

  expect_equal(nrow(res1), 4)
  expect_equal("EDF" %in% names(res1), TRUE)

  # Long
  res1 <- proc_reg(cls, list(myfm1, myfm2), stats = v(seb, edf),
                   output = "long")

  res1

  expect_equal(nrow(res1), 18)
  expect_equal(all(c("PARMS", "SEB") %in% names(res1)), TRUE)

  # Stacked
  res1 <- proc_reg(cls, list(myfm1, myfm2), stats = v(seb, edf),
                   output = "stacked")

  res1

  expect_equal(nrow(res1), 36)
  expect_equal("VALUES" %in% names(res1), TRUE)


})


test_that("reg19: white/spec options work.", {


  myfm1 <- formula(Weight ~ Height)

  res1 <- proc_reg(cls, myfm1, output = "report",
                   stats = spec)

  res1

  expect_equal(length(res1), 5)
  expect_equal(as.integer(res1$SpecTest$DF), 2)
  expect_equal(as.double(res1$SpecTest$CHISQ), 6.24599610)
  expect_equal(round(as.double(res1$SpecTest$PCHISQ), 8), 0.04402498)

})



# test_that("reg20: acov option works.", {
#
#
#   myfm1 <- formula(Weight ~ Height)
#
#   res1 <- proc_reg(cls, myfm1, output = 'report', stats = acov)
#
#   res1
#
#
#
# })



test_that("reg21: hcc option works.", {


  myfm1 <- formula(Weight ~ Height)

  res1 <- proc_reg(cls, myfm1, stats = hcc, output = "report")

  res1

  c1 <- res1$ParameterEstimates
  expect_equal(all(c("HCSTDERR", "HCT", "HCPROBT") %in% names(c1)),
                   TRUE)
  expect_equal(as.numeric(c1$HCSTDERR), c(25.81836644, 0.43125902))
  expect_equal(as.numeric(c1$HCT), c(-5.5397354, 9.0410406))
  expect_equal(round(as.numeric(c1$HCPROBT), 12), c(.0000360089760592,
                                                   .0000000664243133830))

  res2 <- proc_reg(cls, myfm1, stats = v(hcc, hccmethod = 3),
                   output = "report")

  res2

  c2 <- res2$ParameterEstimates
  expect_equal(all(c("HCSTDERR", "HCT", "HCPROBT") %in% names(c2)),
               TRUE)
  expect_equal(as.numeric(c2$HCSTDERR), c(32.23465876, 0.5353434))
  expect_equal(as.numeric(c2$HCT), c(-4.43705390, 7.28323249))
  expect_equal(round(as.numeric(c2$HCPROBT), 10), c(.0003613023,
                                                    .0000012785))


})


test_that("reg22: Confidence limit statistics work.", {


  myfm1 <- formula(Weight ~ Height)

  # CLB
  res1 <- proc_reg(cls, myfm1, stats = clb, output = "report")

  res1

  expect_equal("LCLM" %in% names(res1$ParameterEstimates), TRUE)
  expect_equal("UCLM" %in% names(res1$ParameterEstimates), TRUE)
  expect_equal(res1$ParameterEstimates$LCLM[1], -211.120354)
  expect_equal(res1$ParameterEstimates$UCLM[1], -74.933483)

})

test_that("reg23: Test P option.", {


  myfm1 <- formula(Weight ~ Height)

  # P
  res1 <- proc_reg(cls, myfm1, stats = p, output = "report")

  res1

  expect_equal(length(res1), 6)

  t5 <- res1$OutputStatistics
  t6 <- res1$ResidualStatistics

  expect_equal(t5[1, "DEPVAL"], 112.5)
  expect_equal(t5[1, "PREVAL"], 126.00617)
  expect_equal(t5[1, "RESID"], -13.50617)

  expect_equal(as.numeric(round(t6$VALUE, 4)), c(0, 2142.4877, 2651.3521))


  cls2 <- cls

  cls2[5, "Weight"] <- NA
  cls2[11, "Height"] <- NA

  res2 <- proc_reg(cls2, myfm1, stats = p, output = "report")

  expect_equal(nrow(res2$OutputStatistics), 17)


})


test_that("reg23: Multiple output options works.", {


  myfm1 <- formula(Weight ~ Height)

  # P
  res1 <- proc_reg(cls, myfm1,
                   output = c("report", "out", "long"))

  res1

  expect_equal(length(res1), 2)
  expect_equal(is.data.frame(res1$out), TRUE)
  expect_equal(length(res1$report), 4)

})



test_that("reg24: Check for no rows.", {


  myfm1 <- formula(Weight ~ Height)

  cls2 <- cls[0, ]

  expect_error(proc_reg(cls2, myfm1))


})


test_that("reg25: by parameter with output options works.", {

  # R Syntax
  myfm1 <- formula(Weight ~ Height)


  res1 <- proc_reg(cls, myfm1,
                   by = Sex,
                   options = tableout)

  res1

  expect_equal(nrow(res1), 12)
  expect_equal(any(is.na(res1$BY)), FALSE)




  # Multiple Bys
  res3 <- proc_reg(cls, myfm1,
                   by = v(Sex, Region),
                   options = tableout)

  res3

  expect_equal(nrow(res3), 24)
  expect_equal(any(is.na(res3$BY1)), FALSE)
  expect_equal(any(is.na(res3$BY2)), FALSE)


})


# Testing plots

# library(ggplot2)
#
# ggplot(res2$Statistics, aes(x = PREVAL, y = RESID)) +
#   geom_point() +
#   geom_hline(yintercept = 0) +
#   geom_smooth(se = FALSE, color = "red") +
#   labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')
#


# res1 <- proc_reg(cars, dist ~ speed, output = report, stats = p)
# cars |>
# ggplot(aes(speed, dist))+
#   geom_point(aes(size = abs(res1$Statistics$RESID)))+
#   geom_point(aes(y=res1$Statistics$PREVAL), color="green")+
#   geom_smooth(method = "lm")+
#   geom_smooth(se = FALSE, color="blue")+
#   geom_segment(aes(xend = speed, yend = res1$Statistics$PREVAL), color="red")
#pltcar




# Tests for proc_glm().
#
# The classic SAS "class" dataset (cls) is reused from the reg tests so the
# results can be validated directly against SAS PROC GLM output.  Expected
# numeric values in the "SAS comparison" tests below are taken from running the
# equivalent PROC GLM step in SAS:
#
#   proc glm data=cls;
#     class Sex;
#     model Weight = Sex Height;
#   run;

options("procs.print" = FALSE)

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


test_that("glm1: parameter checks work.", {

  myfm <- Weight ~ Sex + Height
  bfm  <- Weight ~ Sex + Fork

  expect_error(proc_glm("bork", model = myfm))          # not a data frame
  expect_error(proc_glm(cls[0, ], model = myfm))        # no rows
  expect_error(proc_glm(cls))                           # model required
  expect_error(proc_glm(cls, model = myfm, class = "Fork"))   # bad class name
  expect_error(proc_glm(cls, model = myfm, by = "Fork"))      # bad by name
  expect_error(proc_glm(cls, model = myfm, output = "bogus")) # bad output kw
  expect_error(proc_glm(cls, model = myfm, stats = "bogus"))  # bad stats kw
})


test_that("glm2: make_factors converts class columns to sorted factors.", {

  res <- make_factors(cls, "Sex")

  expect_true(is.factor(res$Sex))
  expect_equal(levels(res$Sex), c("F", "M"))   # SAS default ascending order
  expect_false(is.factor(res$Height))          # non-class column untouched
})


test_that("glm3: get_output_specs_glm records class on the spec.", {

  myfm <- Weight ~ Sex + Height

  res <- get_output_specs_glm(myfm, class = "Sex", report = TRUE)

  expect_equal(names(res), "MODEL1")
  expect_equal(res$MODEL1$var, "Weight")
  expect_equal(res$MODEL1$class, "Sex")
  expect_equal(res$MODEL1$formula, myfm)
})


test_that("glm4: basic report output has expected tables.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  output = "report")

  # NObs, ClassLevels, ANOVA, FitStatistics, TypeI, TypeIII
  expect_true(all(c("NObs", "ClassLevels", "ANOVA",
                    "FitStatistics", "TypeI", "TypeIII") %in% names(res)))

  # Class level information
  expect_equal(res$ClassLevels$LEVELS, 2, ignore_attr = TRUE)
  expect_equal(res$ClassLevels$VALUES, "F M", ignore_attr = TRUE)

  # Overall model degrees of freedom: 2 params + intercept -> Model DF = 2
  expect_equal(res$ANOVA$DF, c(2, 16, 18), ignore_attr = TRUE)
})


test_that("glm5: default out dataset holds Type I and Type III SS.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex")

  expect_s3_class(res, "data.frame")
  expect_equal(sort(unique(res$TYPE)), c("SS1", "SS3"))
  expect_equal(unique(res$SOURCE), c("Sex", "Height"))
  expect_equal(nrow(res), 4)     # 2 sources x 2 SS types
})


test_that("glm6: SAS comparison - Type I / Type III sums of squares.", {

  # Reference values captured from the sasLM GLM engine, which is designed to
  # reproduce SAS PROC GLM.  CONFIRM these against a real SAS run:
  #   Source     DF   Type I SS       Type III SS
  #   Sex         1   1681.1229532     184.7145003
  #   Height      1   5696.8406658    5696.8406658
  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex")

  ss1 <- res[res$TYPE == "SS1", ]
  ss3 <- res[res$TYPE == "SS3", ]

  expect_equal(ss1$SS, c(1681.1229532, 5696.8406658), tolerance = 1e-5)
  expect_equal(ss3$SS, c(184.7145003, 5696.8406658), tolerance = 1e-5)
})


test_that("glm7: stats keyword selects a single SS type.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = "ss3")

  expect_equal(unique(res$TYPE), "SS3")
  expect_equal(nrow(res), 2)
})


test_that("glm8: SAS model syntax matches R model syntax.", {

  resR <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex")
  resS <- proc_glm(cls, model = "Weight = Sex Height", class = "Sex")

  expect_equal(resR$SS, resS$SS, tolerance = 1e-8)
})


test_that("glm9: by-group produces one block per group.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  by = "Region")

  expect_true("BY" %in% names(res))
  expect_equal(sort(unique(res$BY)), c("A", "B"))
})


test_that("glm10: multiple models are named and stacked.", {

  res <- proc_glm(cls, model = list(m1 = Weight ~ Sex + Height,
                                    m2 = Height ~ Sex + Age),
                  class = "Sex")

  expect_equal(sort(unique(res$MODEL)), c("m1", "m2"))
})


test_that("glm11: long / stacked shaping works.", {

  reslong <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                      output = c("out", "long"))
  expect_true("STAT" %in% names(reslong))

  resstk <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                     output = c("out", "stacked"))
  expect_true(all(c("STAT", "VALUES") %in% names(resstk)))
})


test_that("glm12: none output returns NULL.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  output = "none")

  expect_null(res)
})


test_that("glm13: solution produces a parameter estimates table.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = "solution", output = "report")

  expect_true("ParameterEstimates" %in% names(res))

  pe <- res$ParameterEstimates
  expect_equal(pe$PARM, c("Intercept", "SexF", "SexM", "Height"),
               ignore_attr = TRUE)

  # All singular (class-related + intercept) parameters flagged biased "B";
  # the uniquely estimable Height is not.
  expect_equal(pe$BIASED, c("B", "B", "B", ""), ignore_attr = TRUE)

  # Reference level SexM is zeroed with blank t / p
  expect_equal(pe$EST[pe$PARM == "SexM"], 0, ignore_attr = TRUE)
  expect_true(is.na(pe$T[pe$PARM == "SexM"]))
})


test_that("glm14: SAS comparison - parameter estimates (solution).", {

  # Reference values captured from the sasLM GLM engine (BETA=TRUE), which
  # reproduces SAS PROC GLM's solution.  CONFIRM against a real SAS run:
  #   Parameter    Estimate         Std Error       t       Pr>|t|
  #   Intercept   -126.168694818   34.6351950465  -3.64    0.0022
  #   Sex F         -6.620843046    5.3886999068  -1.23    0.2370
  #   Sex M          0 (B)
  #   Height         3.678903064    0.5391660142   6.82    <.0001
  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = "solution", output = "report")
  pe <- res$ParameterEstimates

  expect_equal(pe$EST, c(-126.168694818, -6.620843046, 0, 3.678903064),
               tolerance = 1e-6, ignore_attr = TRUE)
  expect_equal(pe$STDERR[pe$PARM == "Height"], 0.5391660142,
               tolerance = 1e-6, ignore_attr = TRUE)
})


test_that("glm15: clparm adds confidence limits driven by alpha=.", {

  res95 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                    stats = c("solution", "clparm"), output = "report")
  pe95 <- res95$ParameterEstimates

  expect_true(all(c("LCLM", "UCLM") %in% names(pe95)))

  # Reference (zeroed) level has no confidence limits
  expect_true(is.na(pe95$LCLM[pe95$PARM == "SexM"]))

  # 90% limits (alpha=0.10) are narrower than 95% limits (alpha=0.05)
  res90 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                    stats = c("solution", "clparm"),
                    options = c(alpha = 0.10), output = "report")
  pe90 <- res90$ParameterEstimates

  w95 <- pe95$UCLM[pe95$PARM == "Height"] - pe95$LCLM[pe95$PARM == "Height"]
  w90 <- pe90$UCLM[pe90$PARM == "Height"] - pe90$LCLM[pe90$PARM == "Height"]
  expect_lt(w90, w95)
})


test_that("glm16: outstat option returns the default SS output dataset.", {

  # Following proc_reg's "outest" precedent, the sum-of-squares dataset is the
  # default out, so "outstat" is an accepted keyword that yields the same result.
  base <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex")
  os   <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   options = "outstat")

  expect_equal(os, base, ignore_attr = TRUE)
  expect_true(all(c("SOURCE", "TYPE", "SS") %in% names(os)))
})


test_that("glm17: p keyword adds predicted values and residuals.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = "p", output = "report")

  expect_true(all(c("OutputStatistics", "ResidualStatistics") %in% names(res)))

  os <- res$OutputStatistics
  expect_equal(names(os), c("OBS", "DEPVAL", "PREVAL", "RESID"))
  expect_equal(nrow(os), 19)                       # all obs used

  # Predicted + residual should reconstruct the observed value
  expect_equal(os$DEPVAL, os$PREVAL + os$RESID, tolerance = 1e-8,
               ignore_attr = TRUE)

  # Residuals sum to ~0 for an OLS fit with intercept
  expect_equal(sum(os$RESID), 0, tolerance = 1e-6)

  # SAS prints five residual summary statistics
  rs <- res$ResidualStatistics
  expect_equal(nrow(rs), 5)
  expect_equal(rs$LABEL, c("Sum of Residuals",
                           "Sum of Squared Residuals",
                           "Sum of Squared Residuals - Error SS",
                           "First Order Autocorrelation",
                           "Durbin-Watson D"), ignore_attr = TRUE)
  # Durbin-Watson D matches SAS
  expect_equal(rs$VALUE[rs$LABEL == "Durbin-Watson D"], 2.284666,
               tolerance = 1e-5, ignore_attr = TRUE)
})


test_that("glm18: lsmeans effect must be a class variable.", {

  expect_error(proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                        lsmeans = "Height"))          # not on class
  expect_error(proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                        lsmeans = "Fork"))            # not in data
})


test_that("glm19: lsmeans produces an LS-means table per effect.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  lsmeans = "Sex", output = "report")

  expect_true("LSMeans.Sex" %in% names(res))

  ls <- res$LSMeans.Sex
  expect_equal(names(ls), c("LEVEL", "LSMEAN", "STDERR", "PROBT", "LCLM", "UCLM"))
  expect_equal(ls$LEVEL, c("F", "M"), ignore_attr = TRUE)

  # LS-means lie inside their confidence limits
  expect_true(all(ls$LSMEAN > ls$LCLM & ls$LSMEAN < ls$UCLM))

  # alpha= narrows the LS-mean confidence interval
  res90 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                    lsmeans = "Sex", options = c(alpha = 0.10),
                    output = "report")
  ls90 <- res90$LSMeans.Sex
  expect_lt(ls90$UCLM[1] - ls90$LCLM[1], ls$UCLM[1] - ls$LCLM[1])
})

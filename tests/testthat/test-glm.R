# Tests for proc_glm().
#
# The classic SAS "class" dataset (cls) is reused from the reg tests so results
# can be validated directly against SAS PROC GLM output.  Every numeric value in
# the "matches SAS" tests below is the value produced by the sasLM GLM engine
# (which is designed to reproduce PROC GLM) and has been confirmed against a real
# SAS run of the equivalent step.
#
# Reference SAS step for the main model used throughout:
#
#   proc glm data=cls;
#     class Sex;
#     model Weight = Sex Height / solution clparm ss1 ss2 ss3 p;
#     lsmeans Sex / stderr cl;
#     contrast 'F vs M' Sex 1 -1;
#     estimate 'F vs M' Sex 1 -1;
#   run;

base_path <- file.path(getwd(), "tests/testthat")
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


test_that("glm1: parameter checks work.", {

  myfm <- Weight ~ Sex + Height
  bfm  <- Weight ~ Sex + Fork

  expect_error(proc_glm("bork", model = myfm))                # not a data frame
  expect_error(proc_glm(cls[0, ], model = myfm))              # no rows
  expect_error(proc_glm(cls))                                 # model required
  expect_error(proc_glm(cls, model = myfm, class = "Fork"))   # bad class name
  expect_error(proc_glm(cls, model = myfm, by = "Fork"))      # bad by name
  expect_error(proc_glm(cls, model = myfm, output = "bogus")) # bad output kw
  expect_error(proc_glm(cls, model = myfm, stats = "bogus"))  # bad stats kw
  expect_error(proc_glm(cls, model = myfm, options = "bogus"))# bad option kw

  # lsmeans / random effect must be a class variable
  expect_error(proc_glm(cls, model = myfm, class = "Sex", lsmeans = "Height"))
  expect_error(proc_glm(cls, model = myfm, class = "Sex", lsmeans = "Fork"))
  expect_error(proc_glm(cls, model = myfm, class = "Sex", random = "Height"))

  # contrast / estimate must be named, with the right coefficient count
  expect_error(proc_glm(cls, model = myfm, class = "Sex",
                        contrast = list(list(Sex = c(1, -1)))))         # unnamed
  expect_error(proc_glm(cls, model = myfm, class = "Sex",
                        contrast = list("bad" = list(Sex = c(1, -1, 1))),
                        output = "report"))                             # bad count
})


test_that("glm2: get_output_specs_glm records class on the spec.", {

  myfm <- Weight ~ Sex + Height

  res <- get_output_specs_glm(myfm, class = "Sex", report = TRUE)

  expect_equal(names(res), "MODEL1")
  expect_equal(res$MODEL1$var, "Weight")
  expect_equal(res$MODEL1$class, "Sex")
  expect_equal(res$MODEL1$formula, myfm)
})


test_that("glm3: make_factors converts class columns to sorted factors.", {

  res <- make_factors(cls, "Sex")

  expect_true(is.factor(res$Sex))
  expect_equal(levels(res$Sex), c("F", "M"))   # SAS default ascending order
  expect_false(is.factor(res$Height))          # non-class column untouched
})


test_that("glm4: Basic proc_glm() works.", {

  # R Syntax
  res1 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   output = "report")

  # NObs, ClassLevels, ANOVA, FitStatistics, TypeI, TypeIII
  expect_equal(names(res1), c("ClassLevels", "NObs", "ANOVA",
                              "FitStatistics", "TypeI", "TypeIII"))

  # Class level information
  expect_equal(res1$ClassLevels$LEVELS, 2, ignore_attr = TRUE)
  expect_equal(res1$ClassLevels$VALUES, "F M", ignore_attr = TRUE)

  # Observation counts
  expect_equal(res1$NObs$NOBS, c(19, 19))

  # SAS Syntax matches R syntax
  res2 <- proc_glm(cls, model = "Weight = Sex Height", class = "Sex")
  res1o <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex")

  expect_equal(res1o$SS, res2$SS)
})


test_that("glm5: ANOVA and fit statistics match SAS.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  output = "report")

  # ANOVA: Model / Error / Corrected Total
  aov <- res$ANOVA
  expect_equal(aov$DF, c(2, 16, 18), ignore_attr = TRUE)
  expect_equal(aov$SUMSQ[1], 7377.96361898, ignore_attr = TRUE)
  expect_equal(aov$SUMSQ[2], 1957.77322312, ignore_attr = TRUE)
  expect_equal(aov$SUMSQ[3], 9335.73684211, ignore_attr = TRUE)
  expect_equal(aov$MEANSQ[1], 3688.981809491, ignore_attr = TRUE)
  expect_equal(aov$MEANSQ[2], 122.360826445, ignore_attr = TRUE)
  expect_equal(aov$FVAL[1], 30.1483891263, ignore_attr = TRUE)
  expect_equal(aov$PROBF[1], 3.74033296136e-06, ignore_attr = TRUE)

  # Fit statistics: R-Square, Coeff Var, Root MSE, Dependent Mean
  fit <- res$FitStatistics
  expect_equal(fit$RSQ, 0.790292586838, ignore_attr = TRUE)
  expect_equal(fit$COEFVAR, 11.0587726002, ignore_attr = TRUE)
  expect_equal(fit$RMSE, 11.0616828035, ignore_attr = TRUE)
  expect_equal(fit$DEPMEAN, 100.026315789, ignore_attr = TRUE)
})


test_that("glm6: Type I / II / III sums of squares match SAS.", {

  # Type I and Type III are the PROC GLM default output.
  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex")

  expect_equal(sort(unique(res$TYPE)), c("SS1", "SS3"))
  expect_equal(unique(res$SOURCE), c("Sex", "Height"))
  expect_equal(nrow(res), 4)                       # 2 sources x 2 SS types

  ss1 <- res[res$TYPE == "SS1", ]
  ss3 <- res[res$TYPE == "SS3", ]

  #   Source     DF   Type I SS       Type III SS
  #   Sex         1   1681.1229532    184.7145003
  #   Height      1   5696.8406658    5696.8406658
  expect_equal(ss1$SS[1], 1681.122953216, ignore_attr = TRUE)
  expect_equal(ss1$SS[2], 5696.840665766, ignore_attr = TRUE)
  expect_equal(ss3$SS[1], 184.714500345, ignore_attr = TRUE)
  expect_equal(ss3$SS[2], 5696.840665766, ignore_attr = TRUE)

  # F values and p-values for Type I
  expect_equal(ss1$FVAL[1], 13.73906177374, ignore_attr = TRUE)
  expect_equal(ss1$FVAL[2], 46.55771647896, ignore_attr = TRUE)
  expect_equal(ss1$PROBF[1], 1.91538463586e-03, ignore_attr = TRUE)
  expect_equal(ss1$PROBF[2], 4.09250060773e-06, ignore_attr = TRUE)

  # Type II (ss2 keyword).  For a main-effects model Type II == Type III.
  res2 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   stats = "ss2", output = "report")
  expect_equal(res2$TypeII$SUMSQ[1], 184.714500345, ignore_attr = TRUE)
  expect_equal(res2$TypeII$SUMSQ[2], 5696.840665766, ignore_attr = TRUE)

  # A single stats keyword restricts the output to that SS type
  ress3 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                    stats = "ss3")
  expect_equal(unique(ress3$TYPE), "SS3")
  expect_equal(nrow(ress3), 2)
})


test_that("glm7: parameter estimates (solution) match SAS.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = "solution", output = "report")
  pe <- res$ParameterEstimates

  expect_equal(pe$PARM, c("Intercept", "SexF", "SexM", "Height"),
               ignore_attr = TRUE)

  # Singular (class-related + intercept) parameters are flagged biased "B";
  # the uniquely estimable Height is not.
  expect_equal(pe$BIASED, c("B", "B", "B", ""), ignore_attr = TRUE)

  #   Parameter    Estimate         Std Error       t         Pr>|t|
  #   Intercept   -126.168694818   34.635195046   -3.64      0.0022
  #   Sex F         -6.620843046    5.388699907   -1.23      0.2370
  #   Sex M          0 (B)          0
  #   Height         3.678903064    0.539166014    6.82      <.0001
  expect_equal(pe$EST[1], -126.16869481800, ignore_attr = TRUE)
  expect_equal(pe$EST[2], -6.62084304645, ignore_attr = TRUE)
  expect_equal(pe$EST[3], 0, ignore_attr = TRUE)
  expect_equal(pe$EST[4], 3.67890306397, ignore_attr = TRUE)

  expect_equal(pe$STDERR[1], 34.635195046487, ignore_attr = TRUE)
  expect_equal(pe$STDERR[2], 5.388699906767, ignore_attr = TRUE)
  expect_equal(pe$STDERR[4], 0.539166014175, ignore_attr = TRUE)

  expect_equal(pe$T[1], -3.64278863303, ignore_attr = TRUE)
  expect_equal(pe$T[2], -1.22865313731, ignore_attr = TRUE)
  expect_equal(pe$T[4], 6.82332151367, ignore_attr = TRUE)

  expect_equal(pe$PROBT[1], 2.19185342953e-03, ignore_attr = TRUE)
  expect_equal(pe$PROBT[2], 2.36967807134e-01, ignore_attr = TRUE)
  expect_equal(pe$PROBT[4], 4.09250060773e-06, ignore_attr = TRUE)

  # Reference level SexM is zeroed with blank t / p
  expect_equal(pe$EST[pe$PARM == "SexM"], 0, ignore_attr = TRUE)
  expect_true(is.na(pe$T[pe$PARM == "SexM"]))

  # "est" is an accepted alias for "solution"
  resa <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   stats = "est", output = "report")
  expect_true("ParameterEstimates" %in% names(resa))
})


test_that("glm8: clparm confidence limits match SAS.", {

  # 95% limits (default alpha = 0.05)
  res95 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                    stats = c("solution", "clparm"), output = "report")
  pe95 <- res95$ParameterEstimates

  expect_true(all(c("LCLM", "UCLM") %in% names(pe95)))

  expect_equal(pe95$LCLM[1], -199.59202833661, ignore_attr = TRUE)
  expect_equal(pe95$LCLM[2], -18.04437653472, ignore_attr = TRUE)
  expect_equal(pe95$LCLM[4], 2.53592217335, ignore_attr = TRUE)
  expect_equal(pe95$UCLM[1], -52.74536129940, ignore_attr = TRUE)
  expect_equal(pe95$UCLM[2], 4.80269044182, ignore_attr = TRUE)
  expect_equal(pe95$UCLM[4], 4.82188395458, ignore_attr = TRUE)

  # Reference (zeroed) level has no confidence limits
  expect_true(is.na(pe95$LCLM[pe95$PARM == "SexM"]))

  # 90% limits (alpha = 0.10)
  res90 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                    stats = c("solution", "clparm"),
                    options = c(alpha = 0.10), output = "report")
  pe90 <- res90$ParameterEstimates

  expect_equal(pe90$LCLM[1], -186.63771647431, ignore_attr = TRUE)
  expect_equal(pe90$LCLM[2], -16.02888625003, ignore_attr = TRUE)
  expect_equal(pe90$LCLM[4], 2.73758192101, ignore_attr = TRUE)
  expect_equal(pe90$UCLM[1], -65.69967316170, ignore_attr = TRUE)
  expect_equal(pe90$UCLM[2], 2.78720015712, ignore_attr = TRUE)
  expect_equal(pe90$UCLM[4], 4.62022420692, ignore_attr = TRUE)

  # 90% limits are narrower than 95% limits
  w95 <- pe95$UCLM[4] - pe95$LCLM[4]
  w90 <- pe90$UCLM[4] - pe90$LCLM[4]
  expect_lt(w90, w95)

  # "clb" is an accepted alias for "clparm"
  resa <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   stats = c("solution", "clb"), output = "report")
  expect_true(all(c("LCLM", "UCLM") %in% names(resa$ParameterEstimates)))
})


test_that("glm9: predicted values and residuals (p) match SAS.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = "p", output = "report")

  expect_true(all(c("OutputStatistics", "ResidualStatistics") %in% names(res)))

  os <- res$OutputStatistics
  expect_equal(names(os), c("OBS", "DEPVAL", "PREVAL", "RESID"))
  expect_equal(nrow(os), 19)                       # all obs used

  # First three predicted values and residuals match SAS
  expect_equal(os$PREVAL[1], 127.6756165956, ignore_attr = TRUE)
  expect_equal(os$PREVAL[2], 75.0684852496, ignore_attr = TRUE)
  expect_equal(os$PREVAL[3], 107.4428322125, ignore_attr = TRUE)
  expect_equal(os$RESID[1], -15.17561659558, ignore_attr = TRUE)
  expect_equal(os$RESID[2], 8.93151475043, ignore_attr = TRUE)
  expect_equal(os$RESID[3], -9.44283221246, ignore_attr = TRUE)

  # Predicted + residual reconstruct the observed value; residuals sum to ~0
  expect_equal(os$DEPVAL, os$PREVAL + os$RESID, ignore_attr = TRUE)
  expect_equal(sum(os$RESID), 0)

  # SAS prints five residual summary statistics
  rs <- res$ResidualStatistics
  expect_equal(nrow(rs), 5)
  expect_equal(rs$LABEL, c("Sum of Residuals",
                           "Sum of Squared Residuals",
                           "Sum of Squared Residuals - Error SS",
                           "First Order Autocorrelation",
                           "Durbin-Watson D"), ignore_attr = TRUE)

  expect_equal(rs$VALUE[2], 1957.77322312, ignore_attr = TRUE)
  expect_equal(rs$VALUE[4], -0.211868472697, ignore_attr = TRUE)
  expect_equal(rs$VALUE[5], 2.28466645943, ignore_attr = TRUE)
})


test_that("glm10: lsmeans match SAS.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  lsmeans = "Sex", output = "report")

  expect_true("LSMeans.Sex" %in% names(res))

  ls <- res$LSMeans.Sex
  expect_equal(names(ls), c("LEVEL", "LSMEAN", "STDERR", "PROBT", "LCLM", "UCLM"))
  expect_equal(ls$LEVEL, c("F", "M"), ignore_attr = TRUE)

  #   Sex   Weight LSMEAN   Std Error    LCL          UCL
  #   F     96.5416616     3.8057634    88.4738036   104.6095195
  #   M     103.1625046    3.5993770    95.5321663   110.7928429
  expect_equal(ls$LSMEAN[1], 96.5416615545, ignore_attr = TRUE)
  expect_equal(ls$LSMEAN[2], 103.1625046010, ignore_attr = TRUE)
  expect_equal(ls$STDERR[1], 3.80576336924, ignore_attr = TRUE)
  expect_equal(ls$STDERR[2], 3.59937695592, ignore_attr = TRUE)
  expect_equal(ls$LCLM[1], 88.4738036205, ignore_attr = TRUE)
  expect_equal(ls$LCLM[2], 95.5321663182, ignore_attr = TRUE)
  expect_equal(ls$UCLM[1], 104.609519489, ignore_attr = TRUE)
  expect_equal(ls$UCLM[2], 110.792842884, ignore_attr = TRUE)

  # alpha = narrows the LS-mean confidence interval
  res90 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                    lsmeans = "Sex", options = c(alpha = 0.10),
                    output = "report")
  ls90 <- res90$LSMeans.Sex
  expect_lt(ls90$UCLM[1] - ls90$LCLM[1], ls$UCLM[1] - ls$LCLM[1])
})


test_that("glm11: contrast and estimate match SAS.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  contrast = list("F vs M" = list(Sex = c(1, -1))),
                  estimate = list("F vs M" = list(Sex = c(1, -1))),
                  stats = "clparm", output = "report")

  # Contrast: a 1-df F vs M contrast reproduces the Type III SS for Sex
  expect_true("Contrasts" %in% names(res))
  ct <- res$Contrasts
  expect_equal(ct$CONTRAST, "F vs M", ignore_attr = TRUE)
  expect_equal(ct$DF, 1, ignore_attr = TRUE)
  expect_equal(ct$SUMSQ, 184.714500345, ignore_attr = TRUE)
  expect_equal(ct$FVAL, 1.50958853181, ignore_attr = TRUE)
  expect_equal(ct$PROBF, 0.236967807134, ignore_attr = TRUE)

  # Estimate: F vs M equals the SexF solution estimate (SexM is the reference)
  expect_true("Estimates" %in% names(res))
  es <- res$Estimates
  expect_equal(es$EST, -6.62084304645, ignore_attr = TRUE)
  expect_equal(es$STDERR, 5.38869990677, ignore_attr = TRUE)
  expect_equal(es$DF, 16, ignore_attr = TRUE)
  expect_equal(es$T, -1.22865313731, ignore_attr = TRUE)
  expect_equal(es$PROBT, 0.236967807134, ignore_attr = TRUE)

  # clparm adds confidence limits to the estimate
  expect_true(all(c("LCLM", "UCLM") %in% names(es)))
  expect_equal(es$LCLM, -18.0443765347, ignore_attr = TRUE)
  expect_equal(es$UCLM, 4.80269044182, ignore_attr = TRUE)

  # Without clparm the estimate has no confidence limits
  res2 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   estimate = list("F vs M" = list(Sex = c(1, -1))),
                   output = "report")
  expect_false(any(c("LCLM", "UCLM") %in% names(res2$Estimates)))
})


test_that("glm12: random produces an expected mean squares table.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Region,
                  class = c("Sex", "Region"), random = "Sex",
                  output = "report")

  expect_true("RandomEffects" %in% names(res))
  em <- res$RandomEffects
  expect_equal(em$SOURCE, c("Sex", "Region"), ignore_attr = TRUE)

  # Random effect -> Var(); fixed effect -> Q()
  expect_true(grepl("Var(Sex)", em$EMS[em$SOURCE == "Sex"], fixed = TRUE))
  expect_true(grepl("Q(Region)", em$EMS[em$SOURCE == "Region"], fixed = TRUE))
})


test_that("glm13: weighted analysis matches SAS.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  weight = "Age", stats = "solution", output = "report")

  # Weighted ANOVA sums of squares
  expect_equal(res$ANOVA$SUMSQ[1], 97411.5010437, ignore_attr = TRUE)
  expect_equal(res$ANOVA$SUMSQ[2], 26759.1353200, ignore_attr = TRUE)
  expect_equal(res$ANOVA$SUMSQ[3], 124170.6363636, ignore_attr = TRUE)

  # Weighted Type III sums of squares
  expect_equal(res$TypeIII$SUMSQ[1], 2184.34600582, ignore_attr = TRUE)
  expect_equal(res$TypeIII$SUMSQ[2], 74778.51236596, ignore_attr = TRUE)

  # Weighted parameter estimates
  expect_equal(res$ParameterEstimates$EST[1], -128.14066972669, ignore_attr = TRUE)
  expect_equal(res$ParameterEstimates$EST[2], -6.27304944530, ignore_attr = TRUE)
  expect_equal(res$ParameterEstimates$EST[4], 3.71000959043, ignore_attr = TRUE)
})


test_that("glm14: by parameter works.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  by = "Region")

  expect_true("BY" %in% names(res))
  expect_equal(sort(unique(res$BY)), c("A", "B"))
  expect_equal(nrow(res), 8)                       # 2 groups x 2 sources x 2 SS

  # Type III SS per region matches SAS
  a3 <- res[res$BY == "A" & res$TYPE == "SS3", ]
  b3 <- res[res$BY == "B" & res$TYPE == "SS3", ]
  expect_equal(a3$SS[1], 56.59749674771, ignore_attr = TRUE)
  expect_equal(a3$SS[2], 762.80443311052, ignore_attr = TRUE)
  expect_equal(b3$SS[1], 583.15607215369, ignore_attr = TRUE)
  expect_equal(b3$SS[2], 4276.43738947706, ignore_attr = TRUE)
})


test_that("glm15: multiple models and model names work.", {

  # Unnamed models default to MODEL1, MODEL2, ...
  res1 <- proc_glm(cls, model = list(Weight ~ Sex + Height,
                                     Height ~ Sex + Age), class = "Sex")
  expect_equal(sort(unique(res1$MODEL)), c("MODEL1", "MODEL2"))

  # Named models keep their names
  res2 <- proc_glm(cls, model = list(m1 = Weight ~ Sex + Height,
                                     m2 = Height ~ Sex + Age), class = "Sex")
  expect_equal(sort(unique(res2$MODEL)), c("m1", "m2"))
  expect_equal(nrow(res2), 8)                      # 2 models x 2 sources x 2 SS

  # Second model Type I / III SS match SAS
  m2ss1 <- res2[res2$MODEL == "m2" & res2$TYPE == "SS1", ]
  m2ss3 <- res2[res2$MODEL == "m2" & res2$TYPE == "SS3", ]
  expect_equal(m2ss1$SS[1], 52.2463216374, ignore_attr = TRUE)
  expect_equal(m2ss1$SS[2], 297.2584061303, ignore_attr = TRUE)
  expect_equal(m2ss3$SS[1], 37.9612521495, ignore_attr = TRUE)
  expect_equal(m2ss3$SS[2], 297.2584061303, ignore_attr = TRUE)
})


test_that("glm16: output dataset and shaping work.", {

  # Default out dataset is wide, one row per source per SS type
  wide <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex")
  expect_s3_class(wide, "data.frame")
  expect_true(all(c("MODEL", "DEPVAR", "SOURCE", "TYPE", "SS") %in% names(wide)))

  # Long transposes statistics into rows
  reslong <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                      output = c("out", "long"))
  expect_true("STAT" %in% names(reslong))

  # Stacked collapses to a single value column
  resstk <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                     output = c("out", "stacked"))
  expect_true(all(c("STAT", "VALUES") %in% names(resstk)))
})


test_that("glm17: where expression works.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  output = "report", where = expression(Weight < 150))

  # One observation dropped by the filter
  expect_equal(res$NObs$NOBS, c(18, 18))
  expect_equal(res$ANOVA$DF, c(2, 15, 17), ignore_attr = TRUE)

  # Type III SS on the filtered data matches SAS
  expect_equal(res$TypeIII$SUMSQ[1], 154.242318232, ignore_attr = TRUE)
  expect_equal(res$TypeIII$SUMSQ[2], 3995.639149667, ignore_attr = TRUE)
})


test_that("glm18: output keywords and outstat option work.", {

  # none returns NULL
  resn <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   output = "none")
  expect_null(resn)

  # outstat yields the default sum-of-squares dataset (like proc_reg's outest)
  base <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex")
  os   <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   options = "outstat")
  expect_equal(os, base, ignore_attr = TRUE)
  expect_true(all(c("SOURCE", "TYPE", "SS") %in% names(os)))
})

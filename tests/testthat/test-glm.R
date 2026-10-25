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

fitness <- read.table(header = TRUE, text = '
Age Weight Oxygen RunTime
44 89.47 44.609 11.37
40 75.07 45.313 10.07
44 85.84 54.297  8.65
42 68.15 59.571  8.17
38 89.02 49.874  9.22
47 77.45 44.811 11.63
40 75.98 45.681 11.95
43 81.19 49.091 10.85
44 81.42 39.442 13.08
38 81.87 60.055  8.63
44 73.03 50.541 10.13
45 87.66 37.388 14.03
45 66.45 44.754 11.12
47 79.15 47.273 10.60
54 83.12 51.855 10.33
49 81.42 49.156  8.95
51 69.63 40.836 10.95
51 77.91 46.672 10.00
48 91.63 46.774 10.25
49 73.37 50.388 10.08
57 73.37 39.407 12.63
54 79.38 46.080 11.17
52 76.32 45.441  9.63
50 70.87 54.625  8.92
51 67.25 45.118 11.08
54 91.63 39.203 12.88
51 73.71 45.790 10.47
57 59.08 50.545  9.93
49 76.32 48.673  9.40
48 61.24 47.920 11.50
52 82.78 47.467 10.50')


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

  # The output dataset carries an ERROR row alongside the sum-of-squares rows,
  # the way the SAS OUTSTAT= dataset does.  Without it the dataset is not
  # self-contained: there is no error term to recompute an F value from.
  expect_equal(sort(unique(res$TYPE)), c("ERROR", "SS1", "SS3"))
  expect_equal(unique(res$SOURCE), c("ERROR", "Sex", "Height"))
  expect_equal(nrow(res), 5)             # 2 sources x 2 SS types + 1 error row

  err <- res[res$TYPE == "ERROR", ]
  expect_equal(nrow(err), 1)
  expect_equal(err$DF, 16, ignore_attr = TRUE)
  expect_equal(err$SS, 1957.773223, ignore_attr = TRUE, tolerance = 1e-7)
  expect_equal(err$MEANSQ, 122.360826, ignore_attr = TRUE, tolerance = 1e-7)
  expect_true(is.na(err$FVAL))           # an error term has no F test

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
  expect_equal(sort(unique(ress3$TYPE)), c("ERROR", "SS3"))
  expect_equal(nrow(ress3), 3)                     # 2 sources + 1 error row
})


test_that("glm7: parameter estimates (solution) match SAS.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = "solution", output = "report")
  pe <- res$ParameterEstimates

  expect_equal(pe$PARM, c("Intercept", "Sex F", "Sex M", "Height"),
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
  expect_equal(pe$EST[pe$PARM == "Sex M"], 0, ignore_attr = TRUE)
  expect_true(is.na(pe$T[pe$PARM == "Sex M"]))

  # "est" is an accepted alias for "solution"
  resa <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   stats = "est", output = "report")
  expect_true("ParameterEstimates" %in% names(resa))
})

test_that("glm7a: biased column is removed from parameter estimates table appropriately.", {

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = "solution", output = "report")
  pe <- res$ParameterEstimates

  nms1 <- names(pe)


  expect_equal("BIASED" %in% nms1, TRUE)



  pe2 <- proc_glm(fitness, output = "report",
                  model = Oxygen ~ RunTime)

  nms2 <- names(pe2)

  expect_equal("BIASED" %in% nms2, FALSE)



  # pe2 <- proc_glm(fitness, output = "report",
  #                 model = Oxygen ~ RunTime,
  # stats = v(e1, e2, e3))
  #
  # fitness3 <- fitness
  # fitness3$AgeGroup <- cut(fitness3$Age,
  #                          breaks = c(-Inf, 44, 50, Inf),
  #                          labels = c("Young", "Middle", "Old"))
  # fitness3$AgeCat <- ifelse(fitness3$Age < 50, "<50", ">=50")
  # fitness3$AgeGroup <- factor(fitness3$AgeGroup)
  #
  # proc_glm(fitness3,
  #          model = Oxygen ~ RunTime + AgeGroup,
  #          class = "AgeGroup",
  #          lsmeans = "AgeGroup",
  #          output = "report")




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
  expect_true(is.na(pe95$LCLM[pe95$PARM == "Sex M"]))

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
  # expect_equal(names(ls), c("LEVEL", "LSMEAN", "STDERR", "PROBT", "LCLM", "UCLM"))
  expect_equal(names(ls), c("LEVEL", "LSMEAN"))
  expect_equal(ls$LEVEL, c("F", "M"), ignore_attr = TRUE)

  #   Sex   Weight LSMEAN   Std Error    LCL          UCL
  #   F     96.5416616     3.8057634    88.4738036   104.6095195
  #   M     103.1625046    3.5993770    95.5321663   110.7928429
  expect_equal(ls$LSMEAN[1], 96.5416615545, ignore_attr = TRUE)
  expect_equal(ls$LSMEAN[2], 103.1625046010, ignore_attr = TRUE)
  # expect_equal(ls$STDERR[1], 3.80576336924, ignore_attr = TRUE)
  # expect_equal(ls$STDERR[2], 3.59937695592, ignore_attr = TRUE)
  # expect_equal(ls$LCLM[1], 88.4738036205, ignore_attr = TRUE)
  # expect_equal(ls$LCLM[2], 95.5321663182, ignore_attr = TRUE)
  # expect_equal(ls$UCLM[1], 104.609519489, ignore_attr = TRUE)
  # expect_equal(ls$UCLM[2], 110.792842884, ignore_attr = TRUE)

  # alpha = narrows the LS-mean confidence interval
  # res90 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
  #                   lsmeans = "Sex", options = c(alpha = 0.10),
  #                   output = "report")
  # ls90 <- res90$LSMeans.Sex
  # expect_lt(ls90$UCLM[1] - ls90$LCLM[1], ls$UCLM[1] - ls$LCLM[1])

  # fitness3 <- fitness
  # fitness3$AgeGroup <- cut(fitness3$Age,
  #                          breaks = c(-Inf, 44, 50, Inf),
  #                          labels = c("Young", "Middle", "Old"))
  # fitness3$AgeCat <- ifelse(fitness3$Age < 50, "<50", ">=50")
  # fitness3$AgeGroup <- factor(fitness3$AgeGroup)
  #
  # proc_glm(fitness3,
  #          model = Oxygen ~ RunTime + AgeGroup,
  #          order = "internal",
  #          class = "AgeGroup",
  #          lsmeans = "AgeGroup")



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
  expect_equal(nrow(res), 10)                # 2 groups x (2 sources x 2 SS + 1 error)

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
  expect_equal(nrow(res2), 10)               # 2 models x (2 sources x 2 SS + 1 error)

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



# Order parameter ------------------------------------------------------------

test_that("glm19: order controls the class level order and reference level.", {

  # Default (internal): F sorts first, so M is the last level and therefore the
  # reference -- SexM carries the zero estimate.
  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = "solution", output = "report")
  pe <- res$ParameterEstimates
  expect_equal(pe$PARM, c("Intercept", "Sex F", "Sex M", "Height"),
               ignore_attr = TRUE)
  expect_equal(pe$EST[pe$PARM == "Sex M"], 0, ignore_attr = TRUE)

  # order = "data" puts M first (Alfred is row 1), so F becomes the reference.
  resd <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   order = "data", stats = "solution", output = "report")
  ped <- resd$ParameterEstimates
  expect_equal(ped$PARM, c("Intercept", "Sex M", "Sex F", "Height"),
               ignore_attr = TRUE)
  expect_equal(ped$EST[ped$PARM == "Sex F"], 0, ignore_attr = TRUE)

  # order = "freq" puts the most frequent level first.  There are 10 M and 9 F.
  resf <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   order = "freq", stats = "solution", output = "report")
  expect_equal(resf$ParameterEstimates$PARM,
               c("Intercept", "Sex M", "Sex F", "Height"), ignore_attr = TRUE)

  # Invalid keywords are rejected
  expect_error(proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                        order = "bork"))
})


test_that("glm20: order remaps the contrast coefficients silently.", {

  # The coefficients are read in level order, so "1 -1" means F - M by default
  # and M - F under order = "data".  Same magnitude, opposite sign.
  ce  <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  estimate = list("1 -1" = list(Sex = c(1, -1))),
                  output = "report")$Estimates
  ced <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  order = "data",
                  estimate = list("1 -1" = list(Sex = c(1, -1))),
                  output = "report")$Estimates
  expect_equal(ce$EST, -ced$EST, ignore_attr = TRUE)
})


test_that("glm21: order is vectorized across class variables.", {

  # One keyword per class variable, like proc_ttest()
  res <- proc_glm(cls, model = Weight ~ Sex + Region,
                  class = c("Sex", "Region"), order = c("data", "internal"),
                  stats = "solution", output = "report")
  expect_equal(res$ParameterEstimates$PARM,
               c("Intercept", "Sex M", "Sex F", "Region A", "Region B"),
               ignore_attr = TRUE)

  # A single keyword is recycled across all class variables
  res2 <- proc_glm(cls, model = Weight ~ Sex + Region,
                   class = c("Sex", "Region"), order = "data",
                   stats = "solution", output = "report")
  expect_equal(res2$ParameterEstimates$PARM,
               c("Intercept", "Sex M", "Sex F", "Region A", "Region B"),
               ignore_attr = TRUE)

  # Misaligned keywords are an error
  expect_error(proc_glm(cls, model = Weight ~ Sex + Region,
                        class = c("Sex", "Region"),
                        order = c("data", "freq", "internal")),
               "misaligned")
})


test_that("glm22: a numeric class variable is factored, not treated as continuous.", {

  # This is the one place proc_glm() cannot follow proc_ttest() exactly.
  # proc_ttest() leaves "internal" alone because it does its own grouping, but
  # sasLM::GLM() would read an unfactored numeric class variable as a continuous
  # predictor and fit 1 df instead of one per level.
  dat <- data.frame(g = c(1, 1, 2, 2, 3, 3), y = c(5, 6, 9, 10, 14, 15))

  res <- proc_glm(dat, model = y ~ g, class = "g", output = "report")
  expect_equal(res$TypeIII$DF, 2, ignore_attr = TRUE)

  # Numeric levels sort by value, so 2 comes before 10
  dat2 <- data.frame(g = c(10, 10, 2, 2), y = c(1, 2, 3, 4))
  res2 <- proc_glm(dat2, model = y ~ g, class = "g", stats = "solution",
                   output = "report")
  expect_equal(res2$ParameterEstimates$PARM, c("Intercept", "g 2", "g 10"),
               ignore_attr = TRUE)
})


test_that("glm23: order = formatted applies a user-defined format.", {

  dat <- data.frame(g = c(1, 2, 3, 1, 2, 3), y = c(5, 9, 14, 6, 10, 15))
  formats(dat) <- list(g = value(condition(x == 1, "Low"),
                                 condition(x == 2, "Mid"),
                                 condition(x == 3, "High")))

  res <- proc_glm(dat, model = y ~ g, class = "g", order = "formatted",
                  stats = "solution", output = "report")

  # The format supplies both the labels and their order
  expect_equal(res$ParameterEstimates$PARM,
               c("Intercept", "g Low", "g Mid", "g High"), ignore_attr = TRUE)

  # A format that collapses values collapses levels, and the model loses the
  # degrees of freedom that separated them.  This mirrors the SAS practice of
  # grouping a class variable with a format.
  dat2 <- dat
  formats(dat2) <- list(g = value(condition(x < 3, "LowMid"),
                                  condition(TRUE, "High")))
  res2 <- proc_glm(dat2, model = y ~ g, class = "g", order = "formatted",
                   output = "report")
  expect_equal(res$TypeIII$DF, 2, ignore_attr = TRUE)
  expect_equal(res2$TypeIII$DF, 1, ignore_attr = TRUE)
})


# contrast and estimate ------------------------------------------------------

# A three-level class variable, so multi-row contrasts have something to test.
# Levels under the default "internal" order are Middle, Old, Young -- the same
# order SAS reports under its Class Level Information.
clsa <- cls
clsa$AgeGroup <- ifelse(clsa$Age <= 12, "Young",
                        ifelse(clsa$Age <= 14, "Middle", "Old"))


test_that("glm24: single-row contrasts match SAS.", {

  # Verified against a real SAS run:
  #   proc glm data=clsa;
  #     class AgeGroup;
  #     model Weight = Height AgeGroup / clparm;
  #     contrast "Young vs Middle" AgeGroup -1 0 1;
  #     contrast "Young vs Old"    AgeGroup  0 -1 1;
  #   run;
  res <- proc_glm(clsa, model = Weight ~ Height + AgeGroup, class = "AgeGroup",
                  contrast = list("Young vs Middle" = list(AgeGroup = c(-1, 0, 1)),
                                  "Young vs Old"    = list(AgeGroup = c(0, -1, 1))),
                  output = "report")

  ct <- res$Contrasts
  expect_equal(ct$CONTRAST, c("Young vs Middle", "Young vs Old"),
               ignore_attr = TRUE)
  expect_equal(ct$DF, c(1, 1), ignore_attr = TRUE)

  # SAS: 375.5376368 and 1.8544003
  expect_equal(ct$SUMSQ, c(375.5376368, 1.8544003), ignore_attr = TRUE,
               tolerance = 1e-7)
  # SAS: F 4.00 / 0.02, Pr > F 0.0640 / 0.8901
  expect_equal(round(ct$FVAL, 2), c(4.00, 0.02), ignore_attr = TRUE)
  expect_equal(round(ct$PROBF, 4), c(0.0640, 0.8901), ignore_attr = TRUE)
})


test_that("glm25: a multi-row contrast is one multi-df F test.", {

  # A matrix value is the R equivalent of the comma in a SAS CONTRAST:
  #   contrast "Any diff" AgeGroup -1 0 1,
  #                       AgeGroup  0 -1 1;
  # SAS reports one row: DF 2, SS 733.3898693, MS 366.6949346, F 3.90, p 0.0432.
  res <- proc_glm(clsa, model = Weight ~ Height + AgeGroup, class = "AgeGroup",
                  contrast = list("Any diff" = list(AgeGroup = rbind(c(-1, 0, 1),
                                                                    c(0, -1, 1)))),
                  output = "report")

  ct <- res$Contrasts
  expect_equal(nrow(ct), 1)                        # one row, not two
  expect_equal(ct$DF, 2, ignore_attr = TRUE)
  expect_equal(ct$SUMSQ, 733.3898693, ignore_attr = TRUE, tolerance = 1e-7)
  expect_equal(ct$MEANSQ, 366.6949346, ignore_attr = TRUE, tolerance = 1e-7)
  expect_equal(round(ct$FVAL, 2), 3.90, ignore_attr = TRUE)
  expect_equal(round(ct$PROBF, 4), 0.0432, ignore_attr = TRUE)

  # Identical to the Type III test for the effect, in SAS and here
  t3 <- res$TypeIII
  expect_equal(ct$DF, t3$DF[t3$SOURCE == "AgeGroup"], ignore_attr = TRUE)
  expect_equal(ct$SUMSQ, t3$SUMSQ[t3$SOURCE == "AgeGroup"], ignore_attr = TRUE)
})


test_that("glm26: estimate matches SAS.", {

  # SAS: Estimate 12.4624658, Std Error 6.23307271, t 2.00, Pr > |t| 0.0640,
  #      95% Confidence Limits -0.8230142 to 25.7479458
  res <- proc_glm(clsa, model = Weight ~ Height + AgeGroup, class = "AgeGroup",
                  estimate = list("Young vs Middle" = list(AgeGroup = c(-1, 0, 1))),
                  stats = "clparm", output = "report")

  es <- res$Estimates
  expect_equal(es$PARM, "Young vs Middle", ignore_attr = TRUE)
  expect_equal(es$EST, 12.4624658, ignore_attr = TRUE, tolerance = 1e-7)
  expect_equal(es$STDERR, 6.23307271, ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(es$DF, 15, ignore_attr = TRUE)
  expect_equal(round(es$T, 2), 2.00, ignore_attr = TRUE)
  expect_equal(round(es$PROBT, 4), 0.0640, ignore_attr = TRUE)
  expect_equal(es$LCLM, -0.8230142, ignore_attr = TRUE, tolerance = 1e-6)
  expect_equal(es$UCLM, 25.7479458, ignore_attr = TRUE, tolerance = 1e-7)
})


test_that("glm27: the SAS interaction operator works.", {

  # SAS: contrast "inter" Sex*Region 1 -1 -1 1;
  #      DF 1, Contrast SS 1523.903926, F 3.80, Pr > F 0.0703
  res <- proc_glm(cls, model = Weight ~ Sex + Region + Sex:Region,
                  class = c("Sex", "Region"),
                  contrast = list("inter" = list("Sex*Region" = c(1, -1, -1, 1))),
                  output = "report")

  ct <- res$Contrasts
  expect_equal(ct$DF, 1, ignore_attr = TRUE)
  expect_equal(ct$SUMSQ, 1523.903926, ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(round(ct$FVAL, 2), 3.80, ignore_attr = TRUE)
  expect_equal(round(ct$PROBF, 4), 0.0703, ignore_attr = TRUE)

  # The R spelling gives the same answer
  res2 <- proc_glm(cls, model = Weight ~ Sex + Region + Sex:Region,
                   class = c("Sex", "Region"),
                   contrast = list("inter" = list("Sex:Region" = c(1, -1, -1, 1))),
                   output = "report")
  expect_equal(res$Contrasts$SUMSQ, res2$Contrasts$SUMSQ, ignore_attr = TRUE)
})


test_that("glm28: contrast and estimate are validated.", {

  myfm <- Weight ~ Sex + Height

  # The label list must be named
  expect_error(proc_glm(cls, model = myfm, class = "Sex",
                        contrast = list(list(Sex = c(1, -1)))), "named list")

  # The inner coefficient list must be named for a model effect
  expect_error(proc_glm(cls, model = myfm, class = "Sex",
                        contrast = list("F vs M" = list(c(1, -1)))), "named list")

  # An effect that is not in the model
  expect_error(proc_glm(cls, model = myfm, class = "Sex",
                        contrast = list("x" = list(Fork = c(1, -1)))),
               "not found in the model")

  # The wrong number of coefficients for the effect
  expect_error(proc_glm(cls, model = myfm, class = "Sex",
                        contrast = list("x" = list(Sex = c(1, -1, 0)))),
               "expects 2 coefficients")

  # Multiple rows are rejected on estimate, where they have no meaning
  expect_error(proc_glm(cls, model = myfm, class = "Sex",
                        estimate = list("m" = list(Sex = rbind(c(1, -1),
                                                               c(1, 0))))),
               "single-row")

  # Ragged row counts across effects
  expect_error(proc_glm(cls, model = Weight ~ Sex + Region,
                        class = c("Sex", "Region"),
                        contrast = list("x" = list(Sex = c(1, -1),
                                                   Region = rbind(c(1, -1),
                                                                  c(0, 1))))),
               "same number of rows")
})


test_that("glm29: a non-estimable spec is dropped with a warning.", {

  # A single level effect on its own is not estimable in an over-parameterized
  # model: the level effects are confounded with the intercept, so adding a
  # constant to the intercept and subtracting it from every level leaves the
  # fit unchanged.  Only differences between levels are estimable.
  #
  # sasLM CONTR() does no estimability check and would return an ordinary
  # looking F value for it.  SAS prints a row for such a contrast too, but the
  # two do not agree, because the value depends on which generalized inverse
  # happens to be used.  Dropping it is the safe reading, and matching SAS is
  # not available here in any case.
  expect_warning(
    res <- proc_glm(clsa, model = Weight ~ Height + AgeGroup, class = "AgeGroup",
                    contrast = list("Young only" = list(AgeGroup = c(0, 0, 1)),
                                    "good" = list(AgeGroup = c(-1, 0, 1))),
                    output = "report"),
    "not estimable")

  expect_equal(nrow(res$Contrasts), 1)
  expect_equal(res$Contrasts$CONTRAST, "good", ignore_attr = TRUE)

  # When nothing is estimable there is no contrast table at all
  suppressWarnings(
    res2 <- proc_glm(clsa, model = Weight ~ Height + AgeGroup, class = "AgeGroup",
                     contrast = list("Young only" = list(AgeGroup = c(0, 0, 1))),
                     output = "report"))
  expect_false("Contrasts" %in% names(res2))

  # An effect name that is no longer special: "intercept" is just a name, and
  # there is no such effect in the model.
  expect_error(proc_glm(clsa, model = Weight ~ Height + AgeGroup,
                        class = "AgeGroup",
                        contrast = list("x" = list(intercept = 1))),
               "not found in the model")
})


# MODEL options --------------------------------------------------------------

test_that("glm30: noint drops the intercept and uncorrects the total.", {

  # SAS: model Weight = Sex Height / noint solution;
  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = c("noint", "solution"), output = "report")

  pe <- res$ParameterEstimates
  expect_false("Intercept" %in% pe$PARM)

  # With no intercept there is no reference level, so both Sex levels are
  # estimated rather than one of them being aliased to zero.
  expect_equal(pe$PARM, c("Sex F", "Sex M", "Height"), ignore_attr = TRUE)

  # SAS: -132.7895379 / -126.1686948 / 3.6789031
  expect_equal(pe$EST, c(-132.7895379, -126.1686948, 3.6789031),
               ignore_attr = TRUE, tolerance = 1e-8)
  # SAS: 32.87490267 / 34.63519505 / 0.53916601
  expect_equal(pe$STDERR, c(32.87490267, 34.63519505, 0.53916601),
               ignore_attr = TRUE, tolerance = 1e-8)

  # SAS labels the total row "Uncorrected Total" and reports DF 19, SS 199435.75
  av <- res$ANOVA
  expect_equal(av$SOURCE[3], "Uncorrected Total", ignore_attr = TRUE)
  expect_equal(av$DF, c(3, 16, 19), ignore_attr = TRUE)
  expect_equal(av$SUMSQ, c(197477.9768, 1957.7732, 199435.7500),
               ignore_attr = TRUE, tolerance = 1e-6)

  # SAS Type III under noint: Sex DF 2 SS 2659.761676, Height DF 1 SS 5696.840666
  t3 <- res$TypeIII
  expect_equal(t3$DF, c(2, 1), ignore_attr = TRUE)
  expect_equal(t3$SUMSQ, c(2659.761676, 5696.840666), ignore_attr = TRUE,
               tolerance = 1e-8)

  # The default model keeps the intercept and the corrected total
  base <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   stats = "solution", output = "report")
  expect_true("Intercept" %in% base$ParameterEstimates$PARM)
  expect_equal(base$ANOVA$SOURCE[3], "Corrected Total", ignore_attr = TRUE)
})


test_that("glm31: clm and cli add confidence limits to the predicted values.", {

  # SAS: model Weight = Sex Height / clm cli;
  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = c("clm", "cli"), output = "report")

  st <- res$OutputStatistics
  expect_true(all(c("LCLM", "UCLM", "LCLI", "UCLI") %in% names(st)))

  # SAS observation 1: Predicted 127.67561660, CL Mean 118.25036242 137.10087078
  expect_equal(st$PREVAL[1], 127.67561660, ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(st$LCLM[1], 118.25036242, ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(st$UCLM[1], 137.10087078, ignore_attr = TRUE, tolerance = 1e-8)

  # The individual limits are wider than the mean limits everywhere, and match
  # predict.lm(interval = "prediction").
  lmf <- stats::lm(Weight ~ Sex + Height, cls)
  pr <- suppressWarnings(stats::predict(lmf, interval = "prediction"))
  expect_equal(st$LCLI, unname(pr[, 2]), ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(st$UCLI, unname(pr[, 3]), ignore_attr = TRUE, tolerance = 1e-8)
  expect_true(all(st$UCLI - st$LCLI > st$UCLM - st$LCLM))

  # clm alone does not add the individual limits, and vice versa
  resm <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   stats = "clm", output = "report")
  expect_false(any(c("LCLI", "UCLI") %in% names(resm$OutputStatistics)))

  resi <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   stats = "cli", output = "report")
  expect_false(any(c("LCLM", "UCLM") %in% names(resi$OutputStatistics)))

  # The alpha option flows through to the limits
  res90 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                    stats = "clm", options = c("alpha" = 0.1),
                    output = "report")
  expect_true(res90$OutputStatistics$UCLM[1] - res90$OutputStatistics$LCLM[1] <
              st$UCLM[1] - st$LCLM[1])
})


test_that("glm32: xpx and inverse match the SAS matrices.", {

  # SAS: model Weight = Sex Height / xpx inverse;
  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = c("xpx", "inverse"), output = "report")

  expect_true(all(c("XPX", "Inverse") %in% names(res)))

  # The matrix is augmented with the dependent variable, so it is one row and
  # one column bigger than the number of model parameters.
  xp <- res$XPX
  expect_equal(names(xp), c("stub", "(Intercept)", "SexF", "SexM", "Height",
                            "Weight"))
  expect_equal(nrow(xp), 5)

  # SAS X'X, first row:   19  9  10  1184.4  1900.5
  expect_equal(c(xp[["(Intercept)"]][1], xp$SexF[1], xp$SexM[1],
                 xp$Height[1], xp$Weight[1]),
               c(19, 9, 10, 1184.4, 1900.5), ignore_attr = TRUE,
               tolerance = 1e-10)
  # SAS X'X, last row:  1900.5  811  1089.5  120316.05  199435.75
  expect_equal(c(xp[["(Intercept)"]][5], xp$SexF[5], xp$SexM[5],
                 xp$Height[5], xp$Weight[5]),
               c(1900.5, 811, 1089.5, 120316.05, 199435.75), ignore_attr = TRUE,
               tolerance = 1e-10)

  # SAS X'X Generalized Inverse (g2), first row:
  #   9.8037645769  -0.604260372  0  -0.151834839  -126.1686948
  inv <- res$Inverse
  expect_equal(c(inv[["(Intercept)"]][1], inv$SexF[1], inv$SexM[1],
                 inv$Height[1], inv$Weight[1]),
               c(9.8037645769, -0.604260372, 0, -0.151834839, -126.1686948),
               ignore_attr = TRUE, tolerance = 1e-8)

  # SAS prints the matrix symmetric, so the last row equals the last column,
  # and the bottom right cell is the error sum of squares.
  expect_equal(c(inv[["(Intercept)"]][5], inv$SexF[5], inv$SexM[5],
                 inv$Height[5], inv$Weight[5]),
               c(-126.1686948, -6.620843046, 0, 3.678903064, 1957.7732231),
               ignore_attr = TRUE, tolerance = 1e-7)
  expect_equal(inv$Weight[5], res$ANOVA$SUMSQ[2], ignore_attr = TRUE,
               tolerance = 1e-6)
})


test_that("glm33: e1, e2 and e3 produce the estimable function tables.", {

  # SAS: model Weight = Sex Height / e1 e2 e3;
  #
  # SAS presents these symbolically: rows are model parameters, columns are the
  # effects being tested, and cells are expressions in the free symbols L1, L2,
  # ... where Lk belongs to the k-th parameter.
  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  stats = c("e1", "e2", "e3"), output = "report")

  expect_true(all(c("TypeIEstimableFunctions", "TypeIIEstimableFunctions",
                    "TypeIIIEstimableFunctions") %in% names(res)))

  e3t <- res$TypeIIIEstimableFunctions
  expect_equal(names(e3t), c("stub", "Sex", "Height"))
  expect_equal(e3t$stub, c("Intercept", "Sex F", "Sex M", "Height"),
               ignore_attr = TRUE)

  # SAS Type III: Sex F = L2, Sex M = -L2, Intercept = 0, Height = 0
  expect_equal(e3t$Sex, c("0", "L2", "-L2", "0"), ignore_attr = TRUE)
  expect_equal(e3t$Height, c("0", "0", "0", "L4"), ignore_attr = TRUE)

  # SAS Type II is identical to Type III for this model
  e2t <- res$TypeIIEstimableFunctions
  expect_equal(e2t$Sex, e3t$Sex, ignore_attr = TRUE)
  expect_equal(e2t$Height, e3t$Height, ignore_attr = TRUE)

  # Keywords are independent
  res1 <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   stats = "e1", output = "report")
  expect_true("TypeIEstimableFunctions" %in% names(res1))
  expect_false("TypeIIIEstimableFunctions" %in% names(res1))
})


test_that("glm34: singular and zeta options are accepted and used.", {

  # zeta is the tolerance of the estimability check.  Loosening it far enough
  # lets a non-estimable contrast through, which proves the option is wired to
  # the check rather than being ignored.
  expect_warning(
    proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
             contrast = list("F only" = list(Sex = c(1, 0))),
             output = "report"),
    "not estimable")

  res <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                  contrast = list("F only" = list(Sex = c(1, 0))),
                  options = c("zeta" = 1e6), output = "report")
  expect_true("Contrasts" %in% names(res))

  # singular is passed through to the generalized inverse
  ressg <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                    stats = "inverse", options = c("singular" = 1e-8),
                    output = "report")
  expect_true("Inverse" %in% names(ressg))

  # Unknown keywords are still rejected
  expect_error(proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                        stats = "bork"), "Invalid stats keyword")
  expect_error(proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                        options = "bork"), "Invalid options keyword")
})


test_that("glm35: contrast, estimate, clm and cli respect the weight.", {

  # sasLM CONTR() and ESTM() call REG() without passing the weights, so going
  # through them would silently return the unweighted statistics -- which would
  # not even agree with the weighted "solution" table printed next to them.
  #
  # SAS, weighted:
  #   Contrast F vs M  DF 1  SS 928.2062631  F 4.56  Pr > F 0.0486
  #   Estimate F vs M  -11.0283318  Std Error 5.16636855  t -2.13  Pr 0.0486
  #   Observation 1    Predicted 131.71552139  CL Mean 123.76809374 139.66294905
  clsw <- cls
  clsw$w <- c(rep(1, 10), rep(3, 9))

  res <- proc_glm(clsw, model = Weight ~ Sex + Height, class = "Sex",
                  weight = "w",
                  contrast = list("F vs M" = list(Sex = c(1, -1))),
                  estimate = list("F vs M" = list(Sex = c(1, -1))),
                  stats = c("clm", "cli"), output = "report")

  ct <- res$Contrasts
  expect_equal(ct$SUMSQ, 928.2062631, ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(round(ct$FVAL, 2), 4.56, ignore_attr = TRUE)
  expect_equal(round(ct$PROBF, 4), 0.0486, ignore_attr = TRUE)

  es <- res$Estimates
  expect_equal(es$EST, -11.0283318, ignore_attr = TRUE, tolerance = 1e-7)
  expect_equal(es$STDERR, 5.16636855, ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(round(es$T, 2), -2.13, ignore_attr = TRUE)

  # The unweighted answer, which is what the sasLM wrappers would have returned
  expect_false(isTRUE(all.equal(es$EST[1], -6.620843046)))

  st <- res$OutputStatistics
  expect_equal(st$PREVAL[1], 131.71552139, ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(st$LCLM[1], 123.76809374, ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(st$UCLM[1], 139.66294905, ignore_attr = TRUE, tolerance = 1e-8)

  # cli divides the error variance by the observation weight, matching a
  # weighted predict.lm(interval = "prediction")
  lw <- stats::lm(Weight ~ Sex + Height, clsw, weights = clsw$w)
  pr <- suppressWarnings(stats::predict(lw, interval = "prediction"))
  expect_equal(st$LCLI, unname(pr[, 2]), ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(st$UCLI, unname(pr[, 3]), ignore_attr = TRUE, tolerance = 1e-8)

  # A weighted fit must not make an estimable contrast look non-estimable:
  # estimability depends on the design matrix, not the weights.
  expect_no_warning(
    proc_glm(clsw, model = Weight ~ Sex + Height, class = "Sex", weight = "w",
             contrast = list("F vs M" = list(Sex = c(1, -1))),
             output = "report"))
})


test_that("glm36: order = formatted requires a user-defined format.", {

  # Only a value() format carries a level order.  Any other kind of format is
  # rejected by fapply() once it is marked as.factor, which is the same
  # behavior proc_ttest() has.  proc_freq() guards on the class instead and
  # silently ignores the request.
  dat <- data.frame(g = c(1.2, 1.4, 2.6, 2.8), y = 1:4)
  formats(dat) <- list(g = "%.0f")

  expect_error(proc_glm(dat, model = y ~ g, class = "g", order = "formatted",
                        output = "report"))

  # With no format at all, formatted falls back to the internal order
  dat2 <- data.frame(g = c(10, 10, 2, 2), y = c(1, 2, 3, 4))
  res <- proc_glm(dat2, model = y ~ g, class = "g", order = "formatted",
                  stats = "solution", output = "report")
  expect_equal(res$ParameterEstimates$PARM, c("Intercept", "g 2", "g 10"),
               ignore_attr = TRUE)
})


# The SAS fitness data, used by the estimable function tests below.
fitness <- read.table(header = TRUE, text = '
Age Weight Oxygen RunTime
44 89.47 44.609 11.37
40 75.07 45.313 10.07
44 85.84 54.297  8.65
42 68.15 59.571  8.17
38 89.02 49.874  9.22
47 77.45 44.811 11.63
40 75.98 45.681 11.95
43 81.19 49.091 10.85
44 81.42 39.442 13.08
38 81.87 60.055  8.63
44 73.03 50.541 10.13
45 87.66 37.388 14.03
45 66.45 44.754 11.12
47 79.15 47.273 10.60
54 83.12 51.855 10.33
49 81.42 49.156  8.95
51 69.63 40.836 10.95
51 77.91 46.672 10.00
48 91.63 46.774 10.25
49 73.37 50.388 10.08
57 73.37 39.407 12.63
54 79.38 46.080 11.17
52 76.32 45.441  9.63
50 70.87 54.625  8.92
51 67.25 45.118 11.08
54 91.63 39.203 12.88
51 73.71 45.790 10.47
57 59.08 50.545  9.93
49 76.32 48.673  9.40
48 61.24 47.920 11.50
52 82.78 47.467 10.50')


test_that("glm37: estimable functions for a continuous effect match SAS.", {

  # SAS (proc_glm-011): model Oxygen = RunTime / e1 e2 e3;
  # All three tables are the same: Intercept = 0, RunTime = L2.
  res <- proc_glm(fitness, model = Oxygen ~ RunTime,
                  stats = c("e1", "e2", "e3"), output = "report")

  for (tnm in c("TypeIEstimableFunctions", "TypeIIEstimableFunctions",
                "TypeIIIEstimableFunctions")) {
    ef <- res[[tnm]]
    expect_equal(names(ef), c("stub", "RunTime"))
    expect_equal(ef$stub, c("Intercept", "RunTime"), ignore_attr = TRUE)
    expect_equal(ef$RunTime, c("0", "L2"), ignore_attr = TRUE)
  }
})


test_that("glm38: estimable functions for a class effect match SAS.", {

  # SAS:
  #   proc glm data=fitness3;
  #     class AgeGroup;
  #     model Oxygen = RunTime AgeGroup / e1 e2 e3;
  #   run;
  #
  # AgeGroup is character, so the levels sort as SAS sorts them: Middle, Old,
  # Young.  The first levels of the effect carry the free symbols and the last
  # level is their negative sum.
  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))

  res <- proc_glm(fitness3, model = Oxygen ~ RunTime + AgeGroup,
                  class = "AgeGroup", stats = c("e1", "e2", "e3"),
                  output = "report")

  stubs <- c("Intercept", "RunTime", "AgeGroup Middle", "AgeGroup Old",
             "AgeGroup Young")
  agcol <- c("0", "0", "L3", "L4", "-L3-L4")

  e1t <- res$TypeIEstimableFunctions
  expect_equal(names(e1t), c("stub", "RunTime", "AgeGroup"))
  expect_equal(e1t$stub, stubs, ignore_attr = TRUE)
  expect_equal(e1t$RunTime,
               c("0", "L2", "0.0107*L2", "0.0541*L2", "-0.0648*L2"),
               ignore_attr = TRUE)
  expect_equal(e1t$AgeGroup, agcol, ignore_attr = TRUE)

  for (tnm in c("TypeIIEstimableFunctions", "TypeIIIEstimableFunctions")) {
    ef <- res[[tnm]]
    expect_equal(ef$stub, stubs, ignore_attr = TRUE)
    expect_equal(ef$RunTime, c("0", "L2", "0", "0", "0"), ignore_attr = TRUE)
    expect_equal(ef$AgeGroup, agcol, ignore_attr = TRUE)
  }
})


test_that("glm39: estimable function coefficients use the SAS 6-character width.", {

  # SAS:
  #   proc glm data=fitness3;
  #     class AgeGroup;
  #     model Oxygen = RunTime AgeGroup RunTime*AgeGroup / e1 e2 e3;
  #   run;
  #
  # SAS writes each coefficient in 6 characters with trailing zeros dropped.
  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))

  res <- proc_glm(fitness3, model = Oxygen ~ RunTime + AgeGroup + RunTime:AgeGroup,
                  class = "AgeGroup", stats = c("e1", "e2", "e3"),
                  output = "report")

  cell <- function(tbl, stub, col) tbl[[col]][tbl$stub == stub]
  ix <- paste("RunTime*AgeGroup", c("Middle", "Old", "Young"))

  e1t <- res$TypeIEstimableFunctions
  expect_equal(vapply(ix, function(s) cell(e1t, s, "RunTime"), ""),
               c("0.4823*L2", "0.779*L2", "-0.2613*L2"), ignore_attr = TRUE)
  expect_equal(vapply(ix, function(s) cell(e1t, s, "AgeGroup"), ""),
               c("10.481*L3-0.2524*L4", "-0.0869*L3+10.739*L4",
                 "-10.394*L3-10.486*L4"), ignore_attr = TRUE)

  e2t <- res$TypeIIEstimableFunctions
  expect_equal(vapply(ix, function(s) cell(e2t, s, "RunTime"), ""),
               c("0.3836*L2", "0.1993*L2", "0.4171*L2"), ignore_attr = TRUE)

  e3t <- res$TypeIIIEstimableFunctions
  expect_equal(vapply(ix, function(s) cell(e3t, s, "RunTime"), ""),
               rep("0.3333*L2", 3), ignore_attr = TRUE)
})



test_that("glm40: estimable functions for a class*class interaction match SAS.", {

  # SAS:
  #   data fitness4;
  #     set fitness3;
  #     if RunTime <= 10.5 then RunCat = 'Fast'; else RunCat = 'Slow';
  #   run;
  #   proc glm data=fitness4;
  #     class AgeGroup RunCat;
  #     model Oxygen = AgeGroup RunCat AgeGroup*RunCat / e1 e2 e3;
  #   run;
  #
  # SAS orders the interaction parameters by the levels of each factor, the
  # first factor varying slowest.
  fitness4 <- fitness
  fitness4$AgeGroup <- ifelse(fitness4$Age <= 44, "Young",
                              ifelse(fitness4$Age <= 50, "Middle", "Old"))
  fitness4$RunCat <- ifelse(fitness4$RunTime <= 10.5, "Fast", "Slow")

  res <- proc_glm(fitness4, model = Oxygen ~ AgeGroup + RunCat + AgeGroup:RunCat,
                  class = c("AgeGroup", "RunCat"), stats = c("e1", "e2", "e3"),
                  output = "report")

  stubs <- c("Intercept", "AgeGroup Middle", "AgeGroup Old", "AgeGroup Young",
             "RunCat Fast", "RunCat Slow",
             paste("AgeGroup*RunCat", c("Middle Fast", "Middle Slow",
                                        "Old Fast", "Old Slow",
                                        "Young Fast", "Young Slow")))
  rccol <- c("0", "0", "0", "0", "L5", "-L5", "0.3278*L5", "-0.3278*L5",
             "0.3576*L5", "-0.3576*L5", "0.3147*L5", "-0.3147*L5")
  ixcol <- c(rep("0", 6), "L7", "-L7", "L9", "-L9", "-L7-L9", "L7+L9")

  e1t <- res$TypeIEstimableFunctions
  expect_equal(names(e1t), c("stub", "AgeGroup", "RunCat", "AgeGroup*RunCat"))
  expect_equal(e1t$stub, stubs, ignore_attr = TRUE)
  expect_equal(e1t$AgeGroup,
               c("0", "L2", "L3", "-L2-L3", "-0.1*L2-0.0545*L3",
                 "0.1*L2+0.0545*L3", "0.5*L2", "0.5*L2", "0.5455*L3",
                 "0.4545*L3", "-0.6*L2-0.6*L3", "-0.4*L2-0.4*L3"),
               ignore_attr = TRUE)
  expect_equal(e1t$RunCat, rccol, ignore_attr = TRUE)
  expect_equal(e1t[["AgeGroup*RunCat"]], ixcol, ignore_attr = TRUE)

  e2t <- res$TypeIIEstimableFunctions
  expect_equal(e2t$stub, stubs, ignore_attr = TRUE)
  expect_equal(e2t$AgeGroup,
               c("0", "L2", "L3", "-L2-L3", "0", "0",
                 "0.5328*L2+0.0179*L3", "0.4672*L2-0.0179*L3",
                 "0.0358*L2+0.565*L3", "-0.0358*L2+0.435*L3",
                 "-0.5685*L2-0.5828*L3", "-0.4315*L2-0.4172*L3"),
               ignore_attr = TRUE)
  expect_equal(e2t$RunCat, rccol, ignore_attr = TRUE)
  expect_equal(e2t[["AgeGroup*RunCat"]], ixcol, ignore_attr = TRUE)

  e3t <- res$TypeIIIEstimableFunctions
  expect_equal(e3t$stub, stubs, ignore_attr = TRUE)
  expect_equal(e3t$AgeGroup,
               c("0", "L2", "L3", "-L2-L3", "0", "0", "0.5*L2", "0.5*L2",
                 "0.5*L3", "0.5*L3", "-0.5*L2-0.5*L3", "-0.5*L2-0.5*L3"),
               ignore_attr = TRUE)
  expect_equal(e3t$RunCat,
               c("0", "0", "0", "0", "L5", "-L5",
                 rep(c("0.3333*L5", "-0.3333*L5"), 3)),
               ignore_attr = TRUE)
  expect_equal(e3t[["AgeGroup*RunCat"]], ixcol, ignore_attr = TRUE)
})


test_that("glm41: estimable functions for a continuous*class interaction match SAS.", {

  # SAS:
  #   proc glm data=fitness3;
  #     class AgeGroup;
  #     model Oxygen = RunTime AgeGroup RunTime*AgeGroup / e1 e2 e3;
  #   run;
  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))

  res <- proc_glm(fitness3, model = Oxygen ~ RunTime + AgeGroup + RunTime:AgeGroup,
                  class = "AgeGroup", stats = c("e1", "e2", "e3"),
                  output = "report")

  stubs <- c("Intercept", "RunTime", "AgeGroup Middle", "AgeGroup Old",
             "AgeGroup Young", "RunTime*AgeGroup Middle",
             "RunTime*AgeGroup Old", "RunTime*AgeGroup Young")
  agcol <- c("0", "0", "L3", "L4", "-L3-L4", "0", "0", "0")
  ixcol <- c(rep("0", 5), "L6", "L7", "-L6-L7")

  e1t <- res$TypeIEstimableFunctions
  expect_equal(e1t$stub, stubs, ignore_attr = TRUE)
  expect_equal(e1t$RunTime,
               c("0", "L2", "0.0107*L2", "0.0541*L2", "-0.0648*L2",
                 "0.4823*L2", "0.779*L2", "-0.2613*L2"), ignore_attr = TRUE)
  expect_equal(e1t$AgeGroup,
               c("0", "0", "L3", "L4", "-L3-L4", "10.481*L3-0.2524*L4",
                 "-0.0869*L3+10.739*L4", "-10.394*L3-10.486*L4"),
               ignore_attr = TRUE)
  expect_equal(e1t[["RunTime*AgeGroup"]], ixcol, ignore_attr = TRUE)

  e2t <- res$TypeIIEstimableFunctions
  expect_equal(e2t$stub, stubs, ignore_attr = TRUE)
  expect_equal(e2t$RunTime,
               c("0", "L2", "0", "0", "0", "0.3836*L2", "0.1993*L2",
                 "0.4171*L2"), ignore_attr = TRUE)
  # The Type II AgeGroup column is checked in glm42
  expect_equal(e2t[["RunTime*AgeGroup"]], ixcol, ignore_attr = TRUE)

  e3t <- res$TypeIIIEstimableFunctions
  expect_equal(e3t$stub, stubs, ignore_attr = TRUE)
  expect_equal(e3t$RunTime,
               c("0", "L2", "0", "0", "0", rep("0.3333*L2", 3)),
               ignore_attr = TRUE)
  expect_equal(e3t$AgeGroup, agcol, ignore_attr = TRUE)
  expect_equal(e3t[["RunTime*AgeGroup"]], ixcol, ignore_attr = TRUE)
})


test_that("glm42: Type II follows the SAS containment rule for continuous*class effects.", {

  # SAS only treats E1 as contained in E2 when both involve the same continuous
  # variables, so AgeGroup is not contained in RunTime*AgeGroup.

  # SAS:
  #   proc glm data=fitness3;
  #     class AgeGroup;
  #     model Oxygen = RunTime AgeGroup RunTime*AgeGroup / ss1 ss2 ss3 e2;
  #   run;
  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))

  res <- proc_glm(fitness3, model = Oxygen ~ RunTime + AgeGroup + RunTime:AgeGroup,
                  class = "AgeGroup", stats = c("ss1", "ss2", "ss3", "e2"),
                  output = "report")

  # SAS Type II SS: RunTime 554.8205507, AgeGroup 17.2123192,
  # RunTime*AgeGroup 13.6269196.
  expect_equal(res$TypeII$SUMSQ, c(554.8205507, 17.2123192, 13.6269196),
               ignore_attr = TRUE, tolerance = 1e-7)

  expect_equal(res$TypeIIEstimableFunctions$AgeGroup,
               c("0", "0", "L3", "L4", "-L3-L4", "0", "0", "0"),
               ignore_attr = TRUE)
})


test_that("glm43: interaction contrast and estimate coefficients follow SAS level order.", {

  # SAS:
  #   proc glm data=fitness4;
  #     class AgeGroup RunCat;
  #     model Oxygen = AgeGroup RunCat AgeGroup*RunCat;
  #     contrast 'ix' AgeGroup*RunCat 1 -1 -1 1 0 0;
  #     estimate 'ix' AgeGroup*RunCat 1 -1 -1 1 0 0;
  #   run;
  #
  # The coefficients are read in SAS order, Middle Fast, Middle Slow, Old Fast,
  # Old Slow, Young Fast, Young Slow, so this is (Middle Fast - Middle Slow) -
  # (Old Fast - Old Slow).  The model is saturated, so the expected values
  # are the cell mean contrast, and they match a real SAS run:
  #   Contrast ix  DF 1  SS 0.14977907  F 0.01  Pr > F 0.9235
  #   Estimate ix  -0.33886667  Std Error 3.49570498  t -0.10  Pr > |t| 0.9235
  fitness4 <- fitness
  fitness4$AgeGroup <- ifelse(fitness4$Age <= 44, "Young",
                              ifelse(fitness4$Age <= 50, "Middle", "Old"))
  fitness4$RunCat <- ifelse(fitness4$RunTime <= 10.5, "Fast", "Slow")

  ix <- list("AgeGroup*RunCat" = c(1, -1, -1, 1, 0, 0))
  res <- proc_glm(fitness4, model = Oxygen ~ AgeGroup + RunCat + AgeGroup:RunCat,
                  class = c("AgeGroup", "RunCat"),
                  contrast = list("ix" = ix), estimate = list("ix" = ix),
                  output = "report")

  expect_equal(res$Estimates$EST, -0.3388666667, ignore_attr = TRUE,
               tolerance = 1e-8)
  expect_equal(res$Estimates$STDERR, 3.4957049846, ignore_attr = TRUE,
               tolerance = 1e-8)
  expect_equal(res$Contrasts$SUMSQ, 0.1497790667, ignore_attr = TRUE,
               tolerance = 1e-8)
  expect_equal(round(res$Contrasts$FVAL, 2), 0.01, ignore_attr = TRUE)
  expect_equal(round(res$Contrasts$PROBF, 4), 0.9235, ignore_attr = TRUE)
  expect_equal(round(res$Estimates$PROBT, 4), 0.9235, ignore_attr = TRUE)

  # A factor keeps its own level order, which need not be alphabetical.  With
  # Young, Middle, Old the same vector is (Young diff - Middle diff).
  fitness5 <- fitness4
  fitness5$AgeGroup <- factor(fitness5$AgeGroup,
                              levels = c("Young", "Middle", "Old"))
  res2 <- proc_glm(fitness5, model = Oxygen ~ AgeGroup + RunCat + AgeGroup:RunCat,
                   class = c("AgeGroup", "RunCat"), estimate = list("ix" = ix),
                   output = "report")

  m <- tapply(fitness5$Oxygen, list(fitness5$AgeGroup, fitness5$RunCat), mean)
  expect_equal(res2$Estimates$EST,
               (m["Young", "Fast"] - m["Young", "Slow"]) -
                 (m["Middle", "Fast"] - m["Middle", "Slow"]),
               ignore_attr = TRUE, tolerance = 1e-8)
})


test_that("glm44: class*class contrasts and estimates match SAS.", {

  # SAS (fitness4 as in glm40):
  #   proc glm data=fitness4;
  #     class AgeGroup RunCat;
  #     model Oxygen = AgeGroup RunCat AgeGroup*RunCat;
  #     contrast 'Middle: Fast vs Slow' RunCat 1 -1 AgeGroup*RunCat 1 -1 0 0 0 0;
  #     contrast 'Young: Fast vs Slow'  RunCat 1 -1 AgeGroup*RunCat 0 0 0 0 1 -1;
  #     contrast 'Fast: Middle vs Old'  AgeGroup 1 -1 0 AgeGroup*RunCat 1 0 -1 0 0 0;
  #     contrast 'ix Middle vs Young'   AgeGroup*RunCat 1 -1 0 0 -1 1;
  #     contrast 'ix 2df'               AgeGroup*RunCat 1 -1 -1 1 0 0,
  #                                     AgeGroup*RunCat 1 -1 0 0 -1 1;
  #     estimate ... (the first four, same coefficients)
  #   run;
  #
  # The first three mix a main effect with the interaction, so both parts
  # have to land on the right parameters.  "ix 2df" spans the whole
  # interaction, so its SS equals the Type III interaction SS.
  fitness4 <- fitness
  fitness4$AgeGroup <- ifelse(fitness4$Age <= 44, "Young",
                              ifelse(fitness4$Age <= 50, "Middle", "Old"))
  fitness4$RunCat <- ifelse(fitness4$RunTime <= 10.5, "Fast", "Slow")

  ix <- "AgeGroup*RunCat"
  specs <- list(
    "Middle: Fast vs Slow" = setNames(list(c(1, -1), c(1, -1, 0, 0, 0, 0)),
                                      c("RunCat", ix)),
    "Young: Fast vs Slow"  = setNames(list(c(1, -1), c(0, 0, 0, 0, 1, -1)),
                                      c("RunCat", ix)),
    "Fast: Middle vs Old"  = setNames(list(c(1, -1, 0), c(1, 0, -1, 0, 0, 0)),
                                      c("AgeGroup", ix)),
    "ix Middle vs Young"   = setNames(list(c(1, -1, 0, 0, -1, 1)), ix))

  res <- proc_glm(fitness4, model = Oxygen ~ AgeGroup + RunCat + AgeGroup:RunCat,
                  class = c("AgeGroup", "RunCat"),
                  contrast = c(specs, list("ix 2df" = setNames(
                    list(rbind(c(1, -1, -1, 1, 0, 0), c(1, -1, 0, 0, -1, 1))), ix))),
                  estimate = specs, output = "report")

  ct <- res$Contrasts
  expect_equal(ct$DF, c(1, 1, 1, 1, 2), ignore_attr = TRUE)
  expect_equal(ct$SUMSQ, c(75.4600900, 176.2437648, 10.4934900, 11.5814543,
                           13.9694816), ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(round(ct$FVAL, 2), c(4.73, 11.06, 0.66, 0.73, 0.44),
               ignore_attr = TRUE)
  expect_equal(round(ct$PROBF, 4), c(0.0392, 0.0027, 0.4248, 0.4021, 0.6500),
               ignore_attr = TRUE)

  es <- res$Estimates
  expect_equal(es$EST, c(5.49400000, 8.56941667, 1.96153333, -3.07541667),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(es$STDERR, c(2.52500056, 2.57706791, 2.41750398, 3.60789507),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(round(es$PROBT, 4), c(0.0392, 0.0027, 0.4248, 0.4021),
               ignore_attr = TRUE)
})


test_that("glm45: continuous*class contrasts and estimates match SAS.", {

  # SAS:
  #   proc glm data=fitness4;
  #     class AgeGroup;
  #     model Oxygen = RunTime AgeGroup RunTime*AgeGroup;
  #     contrast 'slope Middle vs Old' RunTime*AgeGroup 1 -1 0;
  #     contrast 'slopes equal'        RunTime*AgeGroup 1 -1 0,
  #                                    RunTime*AgeGroup 1 0 -1;
  #     estimate 'slope Middle'        RunTime 1 RunTime*AgeGroup 1 0 0;
  #     estimate 'slope Young'         RunTime 1 RunTime*AgeGroup 0 0 1;
  #     estimate 'slope Middle vs Old' RunTime*AgeGroup 1 -1 0;
  #   run;
  #
  # "slopes equal" spans the whole interaction, so its SS equals the Type III
  # RunTime*AgeGroup SS.
  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))

  jx <- "RunTime*AgeGroup"
  res <- proc_glm(fitness3, model = Oxygen ~ RunTime + AgeGroup + RunTime:AgeGroup,
                  class = "AgeGroup",
                  contrast = list(
                    "slope Middle vs Old" = setNames(list(c(1, -1, 0)), jx),
                    "slopes equal" = setNames(list(rbind(c(1, -1, 0),
                                                         c(1, 0, -1))), jx)),
                  estimate = list(
                    "slope Middle" = setNames(list(1, c(1, 0, 0)), c("RunTime", jx)),
                    "slope Young"  = setNames(list(1, c(0, 0, 1)), c("RunTime", jx)),
                    "slope Middle vs Old" = setNames(list(c(1, -1, 0)), jx)),
                  output = "report")

  ct <- res$Contrasts
  expect_equal(ct$DF, c(1, 2), ignore_attr = TRUE)
  expect_equal(ct$SUMSQ, c(1.30826227, 13.62691955), ignore_attr = TRUE,
               tolerance = 1e-8)
  expect_equal(round(ct$PROBF, 4), c(0.6688, 0.3907), ignore_attr = TRUE)

  es <- res$Estimates
  expect_equal(es$EST, c(-2.62010681, -3.72128167, 0.42424529),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(es$STDERR, c(0.57301077, 0.54952203, 0.97999496),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(round(es$T, 2), c(-4.57, -6.77, 0.43), ignore_attr = TRUE)
})


test_that("glm46: an interaction model keeps its observations for class levels, p and lsmeans.", {

  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))

  res <- proc_glm(fitness3, model = Oxygen ~ RunTime + AgeGroup + RunTime:AgeGroup,
                  class = "AgeGroup", stats = "p", lsmeans = "AgeGroup",
                  output = "report")

  # SAS: AgeGroup  3  Middle Old Young
  expect_equal(res$ClassLevels$LEVELS, 3, ignore_attr = TRUE)
  expect_equal(res$ClassLevels$VALUES, "Middle Old Young", ignore_attr = TRUE)

  expect_equal(nrow(res$OutputStatistics), 31)
  expect_equal(res$LSMeans.AgeGroup$LEVEL, c("Middle", "Old", "Young"),
               ignore_attr = TRUE)
})


test_that("glm47: sums of squares tables spell interactions the SAS way.", {

  # SAS writes RunTime*AgeGroup, not RunTime:AgeGroup
  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))
  mdl <- Oxygen ~ RunTime + AgeGroup + RunTime:AgeGroup
  src <- c("RunTime", "AgeGroup", "RunTime*AgeGroup")

  res <- proc_glm(fitness3, model = mdl, class = "AgeGroup",
                  stats = c("ss1", "ss2", "ss3"), output = "report")
  expect_equal(res$TypeI$SOURCE, src, ignore_attr = TRUE)
  expect_equal(res$TypeII$SOURCE, src, ignore_attr = TRUE)
  expect_equal(res$TypeIII$SOURCE, src, ignore_attr = TRUE)

  dat <- proc_glm(fitness3, model = mdl, class = "AgeGroup",
                  stats = c("ss1", "ss2", "ss3"))
  expect_equal(unique(dat$SOURCE[dat$SOURCE != "ERROR"]), src,
               ignore_attr = TRUE)
})


test_that("glm48: Type II matches SAS for class*class and weighted models.", {

  # SAS (fitness4 as in glm40):
  #   model Oxygen = AgeGroup RunCat AgeGroup*RunCat / ss2;
  #   Type II SS: AgeGroup 91.4818672, RunCat 330.5225556,
  #               AgeGroup*RunCat 13.9694816
  fitness4 <- fitness
  fitness4$AgeGroup <- ifelse(fitness4$Age <= 44, "Young",
                              ifelse(fitness4$Age <= 50, "Middle", "Old"))
  fitness4$RunCat <- ifelse(fitness4$RunTime <= 10.5, "Fast", "Slow")

  res <- proc_glm(fitness4, model = Oxygen ~ AgeGroup + RunCat + AgeGroup:RunCat,
                  class = c("AgeGroup", "RunCat"), stats = "ss2",
                  output = "report")
  expect_equal(res$TypeII$SUMSQ, c(91.4818672, 330.5225556, 13.9694816),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(round(res$TypeII$PROBF, 4), c(0.0755, 0.0001, 0.6500),
               ignore_attr = TRUE)

  # In an additive model Type II equals Type III, and the weighted Type III
  # for this model is checked against SAS in glm13.
  resw <- proc_glm(cls, model = Weight ~ Sex + Height, class = "Sex",
                   weight = "Age", stats = "ss2", output = "report")
  expect_equal(resw$TypeII$SUMSQ, c(2184.34600582, 74778.51236596),
               ignore_attr = TRUE, tolerance = 1e-9)
})


test_that("glm49: solution for a class*class interaction matches SAS.", {

  # SAS (fitness4 as in glm40):
  #   model Oxygen = AgeGroup RunCat AgeGroup*RunCat / solution;
  #
  # SAS zeroes every parameter that involves the last level of any factor.
  fitness4 <- fitness
  fitness4$AgeGroup <- ifelse(fitness4$Age <= 44, "Young",
                              ifelse(fitness4$Age <= 50, "Middle", "Old"))
  fitness4$RunCat <- ifelse(fitness4$RunTime <= 10.5, "Fast", "Slow")

  res <- proc_glm(fitness4, model = Oxygen ~ AgeGroup + RunCat + AgeGroup:RunCat,
                  class = c("AgeGroup", "RunCat"), stats = "solution",
                  output = "report")
  pe <- res$ParameterEstimates

  expect_equal(pe$PARM,
               c("Intercept", "AgeGroup Middle", "AgeGroup Old",
                 "AgeGroup Young", "RunCat Fast", "RunCat Slow",
                 paste("AgeGroup*RunCat", c("Middle Fast", "Middle Slow",
                                            "Old Fast", "Old Slow",
                                            "Young Fast", "Young Slow"))),
               ignore_attr = TRUE)
  expect_equal(pe$EST,
               c(44.70575000, -0.27655000, -2.57695000, 0, 8.56941667, 0,
                 -3.07541667, 0, -2.73655000, 0, 0, 0),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(pe$STDERR[c(1, 2, 3, 5, 7, 9)],
               c(1.99618822, 2.67816753, 2.67816753, 2.57706791, 3.60789507,
                 3.53349749), ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(round(pe$PROBT[c(2, 3, 5, 7, 9)], 4),
               c(0.9186, 0.3452, 0.0027, 0.4021, 0.4459), ignore_attr = TRUE)
  expect_true(all(pe$BIASED == "B"))
})


test_that("glm50: solution for a continuous*class interaction matches SAS.", {

  # SAS:
  #   model Oxygen = RunTime AgeGroup RunTime*AgeGroup / solution;
  #
  # RunTime is the slope of the reference group, Young.
  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))

  res <- proc_glm(fitness3, model = Oxygen ~ RunTime + AgeGroup + RunTime:AgeGroup,
                  class = "AgeGroup", stats = "solution", output = "report")
  pe <- res$ParameterEstimates

  expect_equal(pe$PARM,
               c("Intercept", "RunTime", "AgeGroup Middle", "AgeGroup Old",
                 "AgeGroup Young", "RunTime*AgeGroup Middle",
                 "RunTime*AgeGroup Old", "RunTime*AgeGroup Young"),
               ignore_attr = TRUE)
  expect_equal(pe$EST,
               c(87.84912840, -3.72128167, -12.77403113, -9.44665751, 0,
                 1.10117486, 0.67692958, 0),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(pe$STDERR[c(1, 2, 3, 4, 6, 7)],
               c(5.67357688, 0.54952203, 8.37346227, 10.36846280, 0.79392431,
                 0.96644877), ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(round(pe$PROBT[c(3, 4, 6, 7)], 4),
               c(0.1397, 0.3709, 0.1777, 0.4901), ignore_attr = TRUE)
})


test_that("glm51: estimable functions use the weights.", {

  # SAS:
  #   proc glm data=fitness3;
  #     class AgeGroup;
  #     weight Weight;
  #     model Oxygen = RunTime AgeGroup RunTime*AgeGroup / e1 e2 e3;
  #   run;
  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))

  res <- proc_glm(fitness3, model = Oxygen ~ RunTime + AgeGroup + RunTime:AgeGroup,
                  class = "AgeGroup", weight = "Weight",
                  stats = c("e1", "e2", "e3"), output = "report")

  agcol <- c("0", "0", "L3", "L4", "-L3-L4", "0", "0", "0")
  ixcol <- c(rep("0", 5), "L6", "L7", "-L6-L7")

  e1t <- res$TypeIEstimableFunctions
  expect_equal(e1t$RunTime,
               c("0", "L2", "0.0107*L2", "0.0544*L2", "-0.0651*L2",
                 "0.4874*L2", "0.7831*L2", "-0.2704*L2"), ignore_attr = TRUE)
  expect_equal(e1t$AgeGroup,
               c("0", "0", "L3", "L4", "-L3-L4", "10.499*L3-0.2659*L4",
                 "-0.0876*L3+10.776*L4", "-10.411*L3-10.51*L4"),
               ignore_attr = TRUE)
  expect_equal(e1t[["RunTime*AgeGroup"]], ixcol, ignore_attr = TRUE)

  e2t <- res$TypeIIEstimableFunctions
  expect_equal(e2t$RunTime,
               c("0", "L2", "0", "0", "0", "0.3899*L2", "0.1978*L2",
                 "0.4124*L2"), ignore_attr = TRUE)
  expect_equal(e2t$AgeGroup, agcol, ignore_attr = TRUE)

  e3t <- res$TypeIIIEstimableFunctions
  expect_equal(e3t$RunTime, c("0", "L2", "0", "0", "0", rep("0.3333*L2", 3)),
               ignore_attr = TRUE)
  expect_equal(e3t$AgeGroup, agcol, ignore_attr = TRUE)
})


test_that("glm52: a 3x3 class*class model matches SAS.", {

  # SAS:
  #   data fitness5;
  #     set fitness3;
  #     if Weight < 76 then WtCat = 'Lo';
  #     else if Weight < 82 then WtCat = 'Mid';
  #     else WtCat = 'High';
  #   run;
  #   proc glm data=fitness5;
  #     class AgeGroup WtCat;
  #     model Oxygen = AgeGroup WtCat AgeGroup*WtCat / ss2 solution e1;
  #     lsmeans AgeGroup WtCat;
  #     contrast 'ix MO HL' AgeGroup*WtCat 1 -1 0 -1 1 0 0 0 0;
  #     contrast 'ix 4df'   AgeGroup*WtCat 1 -1 0 -1 1 0 0 0 0,
  #                         AgeGroup*WtCat 1 0 -1 -1 0 1 0 0 0,
  #                         AgeGroup*WtCat 1 -1 0 0 0 0 -1 1 0,
  #                         AgeGroup*WtCat 1 0 -1 0 0 0 -1 0 1;
  #     estimate 'Young: High vs Mid' WtCat 1 0 -1
  #                                   AgeGroup*WtCat 0 0 0 0 0 0 1 0 -1;
  #   run;
  fitness5 <- fitness
  fitness5$AgeGroup <- ifelse(fitness5$Age <= 44, "Young",
                              ifelse(fitness5$Age <= 50, "Middle", "Old"))
  fitness5$WtCat <- ifelse(fitness5$Weight < 76, "Lo",
                           ifelse(fitness5$Weight < 82, "Mid", "High"))

  ix <- "AgeGroup*WtCat"
  res <- proc_glm(fitness5, model = Oxygen ~ AgeGroup + WtCat + AgeGroup:WtCat,
                  class = c("AgeGroup", "WtCat"),
                  stats = c("ss2", "solution", "e1"),
                  lsmeans = c("AgeGroup", "WtCat"),
                  contrast = list(
                    "ix MO HL" = setNames(list(c(1, -1, 0, -1, 1, 0, 0, 0, 0)), ix),
                    "ix 4df" = setNames(list(rbind(c(1, -1, 0, -1, 1, 0, 0, 0, 0),
                                                   c(1, 0, -1, -1, 0, 1, 0, 0, 0),
                                                   c(1, -1, 0, 0, 0, 0, -1, 1, 0),
                                                   c(1, 0, -1, 0, 0, 0, -1, 0, 1))), ix)),
                  estimate = list(
                    "Young: High vs Mid" = setNames(list(c(1, 0, -1),
                                                         c(0, 0, 0, 0, 0, 0, 1, 0, -1)),
                                                    c("WtCat", ix))),
                  output = "report")

  expect_equal(res$ClassLevels$VALUES, c("Middle Old Young", "High Lo Mid"),
               ignore_attr = TRUE)
  expect_equal(res$TypeII$SUMSQ, c(110.8953211, 12.0801224, 70.2745688),
               ignore_attr = TRUE, tolerance = 1e-8)

  pe <- res$ParameterEstimates
  expect_equal(pe$PARM[8:16],
               paste("AgeGroup*WtCat", c("Middle High", "Middle Lo", "Middle Mid",
                                         "Old High", "Old Lo", "Old Mid",
                                         "Young High", "Young Lo", "Young Mid")),
               ignore_attr = TRUE)
  expect_equal(pe$EST,
               c(49.52933333, -2.05108333, -3.46500000, 0, 0.06400000,
                 0.74716667, 0, -5.46125000, 1.19633333, 0, 0.04666667,
                 -2.47230000, 0, 0, 0, 0),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(pe$STDERR[c(8, 9, 11, 12)],
               c(6.52223454, 5.70352865, 6.32749692, 5.79061046),
               ignore_attr = TRUE, tolerance = 1e-8)

  expect_equal(res$LSMeans.AgeGroup$LSMEAN,
               c(46.3270000, 45.5261778, 49.7997222), ignore_attr = TRUE,
               tolerance = 1e-8)
  expect_equal(res$LSMeans.WtCat$LSMEAN,
               c(45.9497778, 48.0124833, 47.6906389), ignore_attr = TRUE,
               tolerance = 1e-8)

  expect_equal(res$Contrasts$SUMSQ, c(65.61745707, 70.27456882),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(res$Estimates$EST, 0.06400000, ignore_attr = TRUE,
               tolerance = 1e-8)
  expect_equal(res$Estimates$STDERR, 4.47421598, ignore_attr = TRUE,
               tolerance = 1e-8)

  e1t <- res$TypeIEstimableFunctions
  expect_equal(e1t[["AgeGroup*WtCat"]],
               c(rep("0", 7), "L8", "L9", "-L8-L9", "L11", "L12", "-L11-L12",
                 "-L8-L11", "-L9-L12", "L8+L9+L11+L12"), ignore_attr = TRUE)
  expect_equal(e1t$WtCat[c(8, 14)],
               c("0.2838*L5+0.0205*L6", "0.3571*L5+0.0002*L6"),
               ignore_attr = TRUE)
})


test_that("glm53: a weighted continuous*class model matches SAS.", {

  # SAS:
  #   proc glm data=fitness3;
  #     class AgeGroup;
  #     weight Weight;
  #     model Oxygen = RunTime AgeGroup RunTime*AgeGroup / ss2 solution;
  #     lsmeans AgeGroup;
  #     contrast 'slopes equal'          RunTime*AgeGroup 1 -1 0,
  #                                      RunTime*AgeGroup 1 0 -1;
  #     estimate 'slope Old'             RunTime 1 RunTime*AgeGroup 0 1 0;
  #     estimate 'slope Middle vs Young' RunTime*AgeGroup 1 0 -1;
  #   run;
  fitness3 <- fitness
  fitness3$AgeGroup <- ifelse(fitness3$Age <= 44, "Young",
                              ifelse(fitness3$Age <= 50, "Middle", "Old"))

  jx <- "RunTime*AgeGroup"
  res <- proc_glm(fitness3, model = Oxygen ~ RunTime + AgeGroup + RunTime:AgeGroup,
                  class = "AgeGroup", weight = "Weight",
                  stats = c("ss2", "solution"), lsmeans = "AgeGroup",
                  contrast = list(
                    "slopes equal" = setNames(list(rbind(c(1, -1, 0),
                                                         c(1, 0, -1))), jx)),
                  estimate = list(
                    "slope Old" = setNames(list(1, c(0, 1, 0)), c("RunTime", jx)),
                    "slope Middle vs Young" = setNames(list(c(1, 0, -1)), jx)),
                  output = "report")

  expect_equal(res$TypeII$SUMSQ, c(44025.78541, 1322.80710, 1035.03376),
               ignore_attr = TRUE, tolerance = 1e-9)

  pe <- res$ParameterEstimates
  expect_equal(pe$EST,
               c(87.46541534, -3.69006658, -12.48874132, -9.32272000, 0,
                 1.06703309, 0.67283551, 0),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(pe$STDERR[c(1, 2, 3, 4, 6, 7)],
               c(5.57558277, 0.53921280, 8.17365186, 10.19289633, 0.77348227,
                 0.94709214), ignore_attr = TRUE, tolerance = 1e-8)

  expect_equal(res$LSMeans.AgeGroup$LSMEAN,
               c(47.2089030, 46.2018979, 48.4018944), ignore_attr = TRUE,
               tolerance = 1e-8)

  expect_equal(res$Contrasts$SUMSQ, 1035.033758, ignore_attr = TRUE,
               tolerance = 1e-9)
  expect_equal(res$Estimates$EST, c(-3.01723107, 1.06703309),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(res$Estimates$STDERR, c(0.77860971, 0.77348227),
               ignore_attr = TRUE, tolerance = 1e-8)
})


test_that("glm54: order = data carries through to an interaction.", {

  # SAS:
  #   proc glm data=fitness4 order=data;
  #     class AgeGroup RunCat;
  #     model Oxygen = AgeGroup RunCat AgeGroup*RunCat / solution e3;
  #     contrast 'ix' AgeGroup*RunCat 1 -1 -1 1 0 0;
  #     estimate 'Young: Slow vs Fast' RunCat 1 -1 AgeGroup*RunCat 1 -1 0 0 0 0;
  #   run;
  #
  # Levels in order of appearance: AgeGroup Young Middle Old, RunCat Slow Fast.
  fitness4 <- fitness
  fitness4$AgeGroup <- ifelse(fitness4$Age <= 44, "Young",
                              ifelse(fitness4$Age <= 50, "Middle", "Old"))
  fitness4$RunCat <- ifelse(fitness4$RunTime <= 10.5, "Fast", "Slow")

  kx <- "AgeGroup*RunCat"
  res <- proc_glm(fitness4, model = Oxygen ~ AgeGroup + RunCat + AgeGroup:RunCat,
                  class = c("AgeGroup", "RunCat"), order = "data",
                  stats = c("solution", "e3"),
                  contrast = list("ix" = setNames(list(c(1, -1, -1, 1, 0, 0)), kx)),
                  estimate = list(
                    "Young: Slow vs Fast" = setNames(list(c(1, -1),
                                                          c(1, -1, 0, 0, 0, 0)),
                                                     c("RunCat", kx))),
                  output = "report")

  expect_equal(res$ClassLevels$VALUES, c("Young Middle Old", "Slow Fast"),
               ignore_attr = TRUE)

  pe <- res$ParameterEstimates
  expect_equal(pe$PARM[7:12],
               paste("AgeGroup*RunCat", c("Young Slow", "Young Fast",
                                          "Middle Slow", "Middle Fast",
                                          "Old Slow", "Old Fast")),
               ignore_attr = TRUE)
  expect_equal(pe$EST,
               c(47.96166667, 5.31350000, 1.96153333, 0, -5.83286667, 0,
                 -2.73655000, 0, 0.33886667, 0, 0, 0),
               ignore_attr = TRUE, tolerance = 1e-8)

  expect_equal(res$Contrasts$SUMSQ, 11.58145429, ignore_attr = TRUE,
               tolerance = 1e-8)
  expect_equal(res$Estimates$EST, -8.56941667, ignore_attr = TRUE,
               tolerance = 1e-8)

  expect_equal(res$TypeIIIEstimableFunctions[[kx]],
               c(rep("0", 6), "L7", "-L7", "L9", "-L9", "-L7-L9", "L7+L9"),
               ignore_attr = TRUE)
})


test_that("glm55: noint with an interaction matches SAS.", {

  # SAS:
  #   proc glm data=fitness4;
  #     class AgeGroup RunCat;
  #     model Oxygen = AgeGroup RunCat AgeGroup*RunCat / noint solution;
  #   run;
  fitness4 <- fitness
  fitness4$AgeGroup <- ifelse(fitness4$Age <= 44, "Young",
                              ifelse(fitness4$Age <= 50, "Middle", "Old"))
  fitness4$RunCat <- ifelse(fitness4$RunTime <= 10.5, "Fast", "Slow")

  res <- proc_glm(fitness4, model = Oxygen ~ AgeGroup + RunCat + AgeGroup:RunCat,
                  class = c("AgeGroup", "RunCat"),
                  stats = c("noint", "solution", "ss1"), output = "report")

  expect_equal(res$TypeI$SUMSQ[1], 69686.89091, ignore_attr = TRUE,
               tolerance = 1e-9)

  pe <- res$ParameterEstimates
  expect_equal(pe$PARM[1:4],
               c("AgeGroup Middle", "AgeGroup Old", "AgeGroup Young",
                 "RunCat Fast"), ignore_attr = TRUE)
  expect_equal(pe$EST,
               c(44.42920000, 42.12880000, 44.70575000, 8.56941667, 0,
                 -3.07541667, 0, -2.73655000, 0, 0, 0),
               ignore_attr = TRUE, tolerance = 1e-8)
  expect_equal(pe$STDERR[1:4],
               c(1.78544502, 1.78544502, 1.99618822, 2.57706791),
               ignore_attr = TRUE, tolerance = 1e-8)
})

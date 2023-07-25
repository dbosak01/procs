

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


adsl <- read.table(header = TRUE, text = '
  SUBJID ARM    SEX RACE   AGE AGEGR1
  "001"   "ARM A" "F"  "WHITE" 19   "18-29 years"
  "002"   "ARM B" "F"  "WHITE" 21   "18-29 years"
  "003"   "ARM C" "F"  "WHITE" 23   "18-29 years"
  "004"   "ARM D" "F"  "BLACK OR AFRICAN AMERICAN" 28   "18-29 years"
  "005"   "ARM A" "M"  "WHITE" 37   "30-39 years"
  "006"   "ARM B" "M"  "WHITE" 34   "30-39 years"
  "007"   "ARM C" "M"  "WHITE" 36   "30-39 years"
  "008"   "ARM D" "M"  "WHITE" 30   "30-39 years"
  "009"   "ARM A" "F"  "WHITE" 39   "30-39 years"
  "010"   "ARM B" "F"  "WHITE" 31   "30-39 years"
  "011"   "ARM C" "F"  "BLACK OR AFRICAN AMERICAN" 33   "30-39 years"
  "012"   "ARM D" "F"  "WHITE" 38   "30-39 years"
  "013"   "ARM A" "M"  "BLACK OR AFRICAN AMERICAN" 37   "30-39 years"
  "014"   "ARM B" "M"  "WHITE" 34   "30-39 years"
  "015"   "ARM C" "M"  "WHITE" 36   "30-39 years"
  "016"   "ARM A" "M"  "WHITE" 40   "40-49 years"')

# data Migraine;
# input Gender $ Treatment $ Response $ Count @@;
# datalines;
# female Active  Better 16   female Active  Same 11
# female Placebo Better  5   female Placebo Same 20
# male   Active  Better 12   male   Active  Same 16
# male   Placebo Better  7   male   Placebo Same 19
# ;

migraine <- read.table(header = TRUE, text = '
Gender Treatment Response Count
female Active  Better 16
female Active  Same 11
female Placebo Better  5
female Placebo Same 20
male   Active  Better 12
male   Active  Same 16
male   Placebo Better  7
male   Placebo Same 19')

test_that("stats1: Standard error works.", {


  dt <- c(4, -1, 7, -4, 6, 8, 10)

  res <- get_stderr(dt)

  res

  expect_equal(res, 1.9112983)


})


test_that("stats2: CLM works.", {


  dt <- c(4, -1, 7, -4, 6, 8, 10)

  # 95% limit
  res <- get_clm(dt, alpha = .05)

  res

  expect_equal(res[["ucl"]], 8.9624928)
  expect_equal(res[["lcl"]], -0.3910642)


  # 90% limit
  res <- get_clm(dt, alpha = 0.1)

  res

  expect_equal(res[["ucl"]], 7.9997115)
  expect_equal(res[["lcl"]], 0.5717171)




})


test_that("stats3: getmode works.", {

  dt <- c(4, 3, 7, 4, 3, 3, 10)

  res <- get_mode(dt)


  expect_equal(res, 3)

  dt <- c("a", "c", "b", "a", "c", "c", "d")

  res <- get_mode(dt)

  expect_equal(res, "c")

})

test_that("stats4: CLM works with NA.", {


  dt <- c(4, -1, 7, -4, NA, 8, 10)

  res <- get_clm(dt, TRUE)

  res

  expect_equal(res[["ucl"]], 9.7479957)
  expect_equal(res[["lcl"]], -1.74799573)

})

# Matches SAS!
test_that("stat5: chisq works no weight uncorrected", {


  res <- get_chisq(prt$enrollment, prt$internship)

  res

  expect_equal(res[1, 2], 0)
  expect_equal(res[2, 2], 1)
  expect_equal(res[3, 2], 1)

})


# Matches SAS!
test_that("stat6: chisq works with weight uncorrected", {

  res <- get_chisq(prt$internship, prt$enrollment, prt$count)

  res

  expect_equal(res[1, 2], 0.8189423)
  expect_equal(res[2, 2], 1)
  expect_equal(res[3, 2], 0.365489592)



})


test_that("stat7: chisq works with weight corrected", {

  res <- get_chisq(prt$internship, prt$enrollment, prt$count, TRUE)

  res

  expect_equal(res[1, 2], 0.58989261)
  expect_equal(res[2, 2], 1)
  expect_equal(res[3, 2], 0.44246065)



})

# Matches SAS!
test_that("stat8: fisher works no weight", {


  res <- get_fisher(prt$enrollment, prt$internship)

  res

  expect_equal(res[1, 2], 2)
  expect_equal(res[2, 2], 0.75714286)
  expect_equal(res[3, 2], 0.75714286)
  expect_equal(res[4, 2], 1)

})



# Matches SAS!
test_that("stat9: fisher works with weight", {


  res <- get_fisher(prt$internship, prt$enrollment, prt$count)

  res

  expect_equal(res[1, 2], 67)
  expect_equal(res[2, 2], 0.85127668)
  expect_equal(res[3, 2], 0.22133142)
  expect_equal(res[4, 2], 0.41215159)


})


# test_that("stat10: aov works with one class without weight", {
#
#
#
#   res <- get_aov(adsl, "AGE", "ARM", output = TRUE)
#
#
#   res
#
#   expect_equal(res$AOV.F[1], 0.29836512)
#   expect_equal(res$AOV.P[1], 0.82594862)
#
#   res <- get_aov(adsl, "AGE", "ARM", output = FALSE)
#
#
#   res
#
#   # proc_print(res)
#
#   expect_equal(res$AOV.F[1], 0.29836512)
#   expect_equal(res$AOV.P[1], 0.82594862)
#
#
# })
#
# test_that("stat11: aov works with 2 classes without weight", {
#
#
#
#   res <- get_aov(adsl, "AGE", c("SEX", "ARM"), output = TRUE)
#
#   res
#   expect_equal(res$AOV.F[1], 4.3420586)
#   expect_equal(res$AOV.P[1], 0.061298422)
#
#
#   res <- get_aov(adsl, "AGE", c("SEX", "ARM"), output = FALSE)
#
#
#   res
#
#   # proc_print(res)
#
#   expect_equal(res$AOV.F[1], 4.3420586)
#   expect_equal(res$AOV.P[1], 0.061298422)
#
#
# })


test_that("stat12: get_t() works as expected.", {

  dt <- c(-10, -21, -12,  -5,   1, -70, -41, -24)

  res <- get_t(dt)

  res

  expect_equal(res[["T"]], -2.78847393)
  expect_equal(res[["PRT"]], 0.026967454)
  expect_equal(res[["DF"]],  7.00000000)


  dt <- c(-10, -21, -12,  -5,   1, -70, -41, -24)


  # Change in alpha does not change t test results
  res <- get_t(dt, alpha = 0.1)

  res

  expect_equal(res[["T"]], -2.78847393)
  expect_equal(res[["PRT"]], 0.026967454)
  expect_equal(res[["DF"]],  7.00000000)


})


test_that("stat13: get_skewness() works as expected.", {

  d <- c(27, 32, 46, 38, 23, 51, 19, 57, 33, 62,
         26, 43, 28, 69, 55, 28, 42, 36, 27, 62)


  res <- get_skewness(d)


  expect_equal(res, 0.49070401)

})


test_that("stat14: get_kurtosis() works as expected.", {


  d <- c(27, 32, 46, 38, 23, 51, 19, 57, 33, 62,
         26, 43, 28, 69, 55, 28, 42, 36, 27, 62)


  res <- get_kurtosis(d)


  expect_equal(res, -0.96131045)

})


# Matches SAS?
# test_that("stat10: cmh works no weight uncorrected", {
#
#
#   res <- get_cmh(prt$enrollment, prt$internship)
#
#   res
#
#   prt
#
#   mantelhaen.test(migraine$Gender, migraine$Treatment, migraine$Response)
#
#   sasLM::ORcmh(
#
#   res <- mantelhaen.test(prt$sex, prt$enrollment, prt$internship)
#
#
#   # expect_equal(res[1, 2], 0)
#   # expect_equal(res[2, 2], 1)
#   # expect_equal(res[3, 2], 1)
#
# })


# Matches SAS?
# test_that("stat11: cmh works with weight uncorrected", {
#
#   res <- get_chisq(prt$internship, prt$enrollment, prt$count)
#
#   res
#
#   # expect_equal(res[1, 2], 0.8189423)
#   # expect_equal(res[2, 2], 1)
#   # expect_equal(res[3, 2], 0.365489592)
#
#
#
# })






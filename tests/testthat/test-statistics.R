

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


test_that("stats1: Standard error works.", {


  dt <- c(4, -1, 7, -4, 6, 8, 10)

  res <- get_stderr(dt)

  res

  expect_equal(res, 1.9112983)


})


test_that("stats2: CLM works.", {


  dt <- c(4, -1, 7, -4, 6, 8, 10)

  res <- get_clm(dt)

  res

  expect_equal(res[["ucl"]], 8.9624928)
  expect_equal(res[["lcl"]], -0.3910642)

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




# test_that("stat10: Chisq works as output.", {
#
#
#
# })





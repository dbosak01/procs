

test_that("stats1: Standard error works.", {


  dt <- c(4, -1, 7, -4, 6, 8, 10)

  res <- stderror(dt)

  res

  expect_equal(res, 1.9112983)


})


test_that("stats2: CLM works.", {


  dt <- c(4, -1, 7, -4, 6, 8, 10)

  res <- clm(dt)

  res

  expect_equal(res[["ucl"]], 8.9624928)
  expect_equal(res[["lcl"]], -0.3910642)

})


test_that("stats3: getmode works.", {

  dt <- c(4, 3, 7, 4, 3, 3, 10)

  res <- getmode(dt)


  expect_equal(res, 3)

  dt <- c("a", "c", "b", "a", "c", "c", "d")

  res <- getmode(dt)

  expect_equal(res, "c")

})

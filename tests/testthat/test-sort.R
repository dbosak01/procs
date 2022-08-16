
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


test_that("sort1: Simple proc_sort test works.", {



  res <- proc_sort(prt, by = c("sex", "count"))

  res

  expect_equal(res[4, 1], "boys")
  expect_equal(res[8, 1], "girls")


})


test_that("sort2: Simple proc_sort with keep works.", {



  res <- proc_sort(prt, by = c("sex", "count"),
                   keep = c("sex", "count", "internship"))

  res

  expect_equal(ncol(res), 3)
  expect_equal(res[4, 1], "boys")
  expect_equal(res[8, 1], "girls")


})

test_that("sort3: proc_sort defaults work.", {

  res <- proc_sort(prt)

  res


  expect_equal(ncol(res), 4)
  expect_equal(res[4, 1], "boys")
  expect_equal(res[8, 1], "girls")
  expect_equal(res[4, 2], "yes")
  expect_equal(res[8, 2], "yes")
})

test_that("sort4: nodupkey work.", {

  res <- proc_sort(prt, by = c("sex", "internship"),
                   keep = c("sex", "internship"),
                   nodupkey = TRUE)

  res

  expect_equal(nrow(res), 4)
  expect_equal(ncol(res), 2)

})


test_that("sort5: proc_sort works with single sort variable.", {



  res <- proc_sort(prt, by = c( "count"))

  res

  expect_equal(res[1, "count"], 10)
  expect_equal(res[8, "count"], 53)


})


test_that("sort6: proc_sort works with unquoted variables.", {

  library(common)

  res <- proc_sort(prt, by = sex, keep = v(sex, count))

  res

  expect_equal(ncol(res), 2)
  expect_equal(res[4, 1], "boys")
  expect_equal(res[8, 1], "girls")


})


test_that("sort7: single keep returns data frame.", {



  res <- proc_sort(prt, by = count, keep = count)

  res

  expect_equal("data.frame" %in% class(res), TRUE)
  expect_equal(ncol(res), 1)


})


test_that("sort8: proc_sort returns tibble.", {

  prt2 <- as_tibble(prt)

  res <- proc_sort(prt2, by = sex, keep = c("sex", "count"))

  res

  expect_equal(ncol(res), 2)
  expect_equal("tbl_df" %in% class(res), TRUE)


})

test_that("sort9: log_sort() works as expected.", {


  # data,  by = NULL, keep = NULL, order = "ascending",
  # nodupkey = FALSE

  res <- log_sort(mtcars, by = c("mpg", "cyl"),
                       keep = c("n", "mean", "median"),
                       order = "ascending",
                       nodupkey = FALSE, outdata = mtcars)

  res

  expect_equal(length(res), 6)

})





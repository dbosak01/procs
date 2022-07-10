
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


test_that("sort2: Simple proc_sort with var works.", {



  res <- proc_sort(prt, by = c("sex", "count"),
                   var = c("sex", "count", "internship"))

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
                   var = c("sex", "internship"),
                   nodupkey = TRUE)

  res

  expect_equal(nrow(res), 4)
  expect_equal(ncol(res), 2)

})



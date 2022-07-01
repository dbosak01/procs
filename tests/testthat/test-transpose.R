base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE

score <- read.table(header = TRUE,
colClasses = c(Student = "character", StudentID = "character",
Section = "character"), text = '
Student StudentID Section Test1 Test2 Final
Capalleti "0545" "1"  94 91 87
Dubose    "1252" "2"  51 65 91
Engles    "1167" "1"  95 97 97
Grant     "1230" "2"  63 75 80
Krupski   "2527" "2"  80 76 71
Lundsford "4860" "1"  92 40 86
McBane    "0674" "1"  75 78 72
')


test_that("transpose1: basic var works without error.", {

  res <- proc_transpose(score, var = c("Test1", "Test2", "Final"))


  res

  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 8)


})

test_that("transpose2: no var works without error.", {

  res <- proc_transpose(score)

  res

  expect_equal(nrow(res), 3)
  expect_equal(ncol(res), 8)


})

test_that("transpose3: name parameter works as expected.", {

  res <- proc_transpose(score, name = "Test")

  res

  expect_equal(names(res[1]), "Test")

})


test_that("transpose4: name_label works.", {

  res <- proc_transpose(score, namelabel = "Test")

  res


  expect_equal(names(res[1]), "NAME")
  expect_equal(attr(res[[1]], "label"), "Test")


})



test_that("transpose5: id, prefix, and suffix works.", {

  res <- proc_transpose(score, id = "StudentID", prefix = "sn", suffix = "x")

  res


  expect_equal(names(res[2]), "sn0545x")


})

test_that("transpose6: id and idlabel.", {

  res <- proc_transpose(score, id = "StudentID", idlabel = "Student")

  res


  expect_equal(names(res[1]), "NAME")

  expect_equal(attr(res[[2]], "label"), "Capalleti")


})


test_that("transpose7: proc_means and proc_transpose.", {


  mres <- proc_means(score, c("Test1", "Test2", "Final"),
                    stats = c("n", "mean", "std", "median", "min", "max"),
                    piped = TRUE)

  res <- proc_transpose(mres, id = "Variable", name = "Statistic")

  res


  expect_equal(ncol(res), 4)

  expect_equal(nrow(res), 6)


})

test_that("transpose8: transpose by.", {

  dat2 <- read.table(header = TRUE, text ='
  ID Location Date  Length1 Weight1 Length2 Weight2 Length3 Weight3 Length4 Weight4
  1 "Cole Pond"   2JUN95 31 .25 32 .3  32 .25 33 .3
  2 "Cole Pond"   3JUL95 33 .32 34 .41 37 .48 32 .28
  3 "Cole Pond"   4AUG95 29 .23 30 .25 34 .47 32 .3
  4 "Eagle Lake"  2JUN95 32 .35 32 .25 33 .30 NA NA
  5 "Eagle Lake"  3JUL95 30 .20 36 .45 NA NA  NA NA
  6 "Eagle Lake"  4AUG95 33 .30 33 .28 34 .42 NA NA
  ')
  dat2

  res <- proc_transpose(dat2, var =c("Length1", "Length2", "Length3", "Length4")
                        #, by = c("Location", "Date")
                        )

  res

  # Not working yet
  expect_equal(1, 1)

})



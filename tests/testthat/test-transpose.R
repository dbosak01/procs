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




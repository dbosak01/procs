base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE

scores <- read.table(header = TRUE, text = '
Student StudentID Section Test1 Test2 Final
Capalleti 0545 1  94 91 87
Dubose    1252 2  51 65 91
Engles    1167 1  95 97 97
Grant     1230 2  63 75 80
Krupski   2527 2  80 76 71
Lundsford 4860 1  92 40 86
McBane    0674 1  75 78 72
')

# test_that("transpose0: show_viewer works as expected with local path.", {
#
#   score_transposed <- proc_transpose(scores)
#
#
#   expect_equal(1, 1)
#
# })

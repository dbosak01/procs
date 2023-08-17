base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE

datm <- read.table(header = TRUE, text = '
LastName  Age PresentScore TasteScore Flavor Layers
Orlando     27 93 80  Vanilla    1
Ramey       32 84 72  Rum        2
Goldston    46 68 75  Vanilla    1
Roe         38 79 73  Vanilla    2
Larsen      23 77 84  Chocolate  3
Davis       51 86 91  Spice      3
Strickland  19 82 79  Chocolate  1
Nguyen      57 77 84  Vanilla    3
Hildenbrand 33 81 83  Chocolate  1
Byron       62 72 87  Vanilla    2
Sanders     26 56 79  Chocolate  1
Jaeger      43 66 74  NA         1
Davis       28 69 75  Chocolate  2
Conrad      69 85 94  Vanilla    1
Walters     55 67 72  Chocolate  2
Rossburger  28 78 81  Spice      2
Matthew     42 81 92  Chocolate  2
Becker      36 62 83  Spice      2
Anderson    27 87 85  Chocolate  1
Merritt     62 73 84  Chocolate  1
')

options("logr.output" = FALSE)
options("procs.print" = FALSE)


test_that("user1: Demog Table using procs.", {



  expect_equal(TRUE, TRUE)

})

test_that("user2: AE Table using procs.", {



  expect_equal(TRUE, TRUE)

})


test_that("user3: VS Table using procs.", {



  expect_equal(TRUE, TRUE)

})


test_that("user4: Patient Profile Table using procs.", {



  expect_equal(TRUE, TRUE)

})


test_that("user5: Listing Table using procs.", {



  expect_equal(TRUE, TRUE)

})



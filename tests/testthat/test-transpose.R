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

dat2 <- read.table(header = TRUE, text ='
  ID Location Date  Length1 Weight1 Length2 Weight2 Length3 Weight3 Length4 Weight4
  1 "Cole Pond"   2JUN95 31 .25 32 .3  32 .25 33 .3
  2 "Cole Pond"   3JUL95 33 .32 34 .41 37 .48 32 .28
  3 "Cole Pond"   4AUG95 29 .23 30 .25 34 .47 32 .3
  4 "Eagle Lake"  2JUN95 32 .35 32 .25 33 .30 NA NA
  5 "Eagle Lake"  3JUL95 30 .20 36 .45 NA NA  NA NA
  6 "Eagle Lake"  4AUG95 33 .30 33 .28 34 .42 NA NA
  ')

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
Jaeger      43 66 74  Rum        1
Davis       28 69 75  Chocolate  2
Conrad      69 85 94  Vanilla    1
Walters     55 67 72  Chocolate  2
Rossburger  28 78 81  Spice      2
Matthew     42 81 92  Chocolate  2
Becker      36 62 83  Spice      2
Anderson    27 87 85  Chocolate  1
Merritt     62 73 84  Chocolate  1
')

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


  mres <- proc_means(score, var = c("Test1", "Test2", "Final"),
                    stats = c("n", "mean", "std", "median", "min", "max"),
                    out = out(direction = "wide", type = FALSE,
                              freq = FALSE))

  res <- proc_transpose(mres, id = "Variable", name = "Statistic")

  res


  expect_equal(ncol(res), 4)

  expect_equal(nrow(res), 6)


})

test_that("transpose8: transpose by with one variable.", {



  res <- proc_transpose(dat2, var =c("Length1", "Length2", "Length3", "Length4")
                        , by = "Location",
                        name = "Measure", id = "Date" )

  res


  expect_equal(ncol(res), 5)
  expect_equal(nrow(res), 8)
  expect_equal(names(res)[3], "2JUN95")

})

test_that("transpose8: transpose by with one variable and id labels.", {



  res <- proc_transpose(dat2, var =c("Length1", "Length2", "Length3", "Length4")
                        , by = "Location",
                        name = "Measure", idlabel = "Date" )

  res


  expect_equal(ncol(res), 5)
  expect_equal(nrow(res), 8)
  expect_equal(names(res)[3], "COL1")
  expect_equal(attr(res[[3]], "label"), "2JUN95")

})


test_that("transpose9: transpose by with two variables.", {


  res <- proc_transpose(dat2, var =c("Length1", "Length2", "Length3", "Length4")
                        , by = c("Location", "Date"),
                        name = "Measure")


  res

  expect_equal(nrow(res), 24)
  expect_equal(ncol(res), 4)

})

test_that("transpose10: transpose by with two variables and v() function.", {


  res <- proc_transpose(dat2, var =v(Length1, Length2, Length3, Length4)
                        , by = v(Location, Date),
                        name = "Measure")


  res

  expect_equal(nrow(res), 24)
  expect_equal(ncol(res), 4)

})

test_that("transpose11: copy parameter works recycle bigger.", {

  stats <- proc_means(datm, out = out(direction = "wide",
                                      type = FALSE, freq = FALSE))


  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = "Group",
                         name = "Statistic", id = "Variable")


  res2
  expect_equal(nrow(res2), 5)
  expect_equal(ncol(res2), 6)
  expect_equal("Group" %in% names(res2), TRUE)



})


test_that("transpose12: copy parameter works recycle smaller", {

  stats <- proc_means(datm, stats = c("n", "mean", "median"),
                      out = out(direction = "wide", type = FALSE, freq = FALSE))


  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = "Group",
                         name = "Statistic", id = "Variable")


  res2
  expect_equal(nrow(res2), 3)
  expect_equal(ncol(res2), 6)
  expect_equal("Group" %in% names(res2), TRUE)



})


test_that("transpose13: copy parameter works recycle smaller", {


  mns <- proc_means(datm, stats = c("n", "mean", "median", "min", "max"),
                    var = "Age", class = "Flavor",
                    out = out(direction = "wide", type = FALSE, freq = FALSE))

  mns[1, 1] <- "Total"
  mns

  res <- proc_transpose(mns, var = c("N", "Mean", "Median", "Minimum", "Maximum"),
                        copy = "Variable", name = "Stat", id = "Flavor")


  res <- res[, c("Variable", "Stat", "Chocolate", "Rum", "Spice", "Vanilla", "Total")]

  res

  expect_equal(nrow(res), 5)
  expect_equal(ncol(res), 7)

})

# Is working
test_that("transpose14: copy parameter works with by groups", {

  stats <- proc_means(datm, stats = c("n", "mean", "median"),
                      var = c("Age", "PresentScore", "TasteScore"),
                      by = "Layers", out = out(type = FALSE, freq = FALSE,
                                               direction ="wide"))

  stats

  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = "Group", by = "Layers",
                         name = "Statistic", id = "Variable")

  res2

  expect_equal(nrow(res2), 9)
  expect_equal(ncol(res2), 6)
  expect_equal("Group" %in% names(res2), TRUE)



})


test_that("transpose15: copy parameter works with by groups", {

  stats <- proc_means(datm, stats = c("n", "mean", "median"),
                      by = "Layers")

  stats

  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = "Group", by = "Layers",
                         name = "Statistic", id = "Variable")

  res2

  expect_equal(nrow(res2), 9)
  expect_equal(ncol(res2), 6)
  expect_equal("Group" %in% names(res2), TRUE)



})

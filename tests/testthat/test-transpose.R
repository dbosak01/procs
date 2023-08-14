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
', stringsAsFactors = FALSE)

dat2 <- read.table(header = TRUE, text ='
  ID Location Date  Length1 Weight1 Length2 Weight2 Length3 Weight3 Length4 Weight4
  1 "Cole Pond"   2JUN95 31 .25 32 .3  32 .25 33 .3
  2 "Cole Pond"   3JUL95 33 .32 34 .41 37 .48 32 .28
  3 "Cole Pond"   4AUG95 29 .23 30 .25 34 .47 32 .3
  4 "Eagle Lake"  2JUN95 32 .35 32 .25 33 .30 NA NA
  5 "Eagle Lake"  3JUL95 30 .20 36 .45 NA NA  NA NA
  6 "Eagle Lake"  4AUG95 33 .30 33 .28 34 .42 NA NA
  ', stringsAsFactors = FALSE)

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
', stringsAsFactors = FALSE)

prt <- read.table(header = TRUE, text = '
  sex internship enrollment count
  1  boys        yes        yes    35
  2  boys         no        yes    14
  3 girls        yes        yes    32
  4 girls         no        yes    53
  5  boys        yes         no    29
  6  boys         no         no    27
  7 girls        yes         no    10
  8 girls         no         no    23',
                  stringsAsFactors = FALSE)

prt2 <- read.table(header = TRUE, text = '
      sex internship enrollment count  group
  1  boys        yes        yes    35      1
  2  boys         no        yes    14      1
  3 girls        yes        yes    32      1
  4 girls         no        yes    53      1
  5  boys        yes         no    29      2
  6  boys         no         no    27      2
  7 girls        yes         no    10      2
  8 girls         no         no    23      2',
                   stringsAsFactors = FALSE)


ageg <- read.table(header = TRUE, text = '
      VAR    LABEL  CAT2  N      CNTPCT
1  AGECAT "18 to 24" "ARM A" 85  "0 (  0.0%)"
2  AGECAT "18 to 24" "ARM B" 85  "1 (  1.2%)"
3  AGECAT "18 to 24" "ARM C" 85  "3 (  3.5%)"
4  AGECAT "18 to 24" "ARM D" 85  "1 (  1.2%)"
5  AGECAT "25 to 44" "ARM A" 85  "4 (  4.7%)"
6  AGECAT "25 to 44" "ARM B" 85  "8 (  9.4%)"
7  AGECAT "25 to 44" "ARM C" 85  "4 (  4.7%)"
8  AGECAT "25 to 44" "ARM D" 85  "7 (  8.2%)"
9  AGECAT "45 to 64" "ARM A" 85 "13 ( 15.3%)"
10 AGECAT "45 to 64" "ARM B" 85  "7 (  8.2%)"
11 AGECAT "45 to 64" "ARM C" 85 "12 ( 14.1%)"
12 AGECAT "45 to 64" "ARM D" 85 "12 ( 14.1%)"
13 AGECAT    ">= 65" "ARM A" 85  "3 (  3.5%)"
14 AGECAT    ">= 65" "ARM B" 85  "5 (  5.9%)"
15 AGECAT    ">= 65" "ARM C" 85  "2 (  2.4%)"
16 AGECAT    ">= 65" "ARM D" 85  "3 (  3.5%)"',
                   stringsAsFactors = FALSE)




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
                    options = v(out, notype, nonobs))

  res <- proc_transpose(mres, id = "VAR", name = "STAT")

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

  stats <- proc_means(datm, options = v(out,
                                      notype, nonobs))


  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = "Group",
                         name = "STAT", id = "VAR")


  res2
  expect_equal(nrow(res2), 5)
  expect_equal(ncol(res2), 6)
  expect_equal("Group" %in% names(res2), TRUE)



})


test_that("transpose12: copy parameter works recycle smaller", {

  stats <- proc_means(datm, stats = c("n", "mean", "median"),
                      options = v(out, notype, nonobs))


  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = "Group",
                         name = "STAT", id = "VAR")


  res2
  expect_equal(nrow(res2), 3)
  expect_equal(ncol(res2), 6)
  expect_equal("Group" %in% names(res2), TRUE)



})


test_that("transpose13: copy parameter works recycle smaller", {


  mns <- proc_means(datm, stats = c("n", "mean", "median", "min", "max"),
                    var = "Age", class = "Flavor",
                    options = v(notype, nonobs))

  mns[1, 1] <- "Total"
  mns

  res <- proc_transpose(mns, var = c("N", "MEAN", "MEDIAN", "MIN", "MAX"),
                        copy = "VAR", name = "STAT", id = "CLASS")

  res
  res <- res[, c("VAR", "STAT", "Chocolate", "Rum", "Spice", "Vanilla", "Total")]

  res

  expect_equal(nrow(res), 5)
  expect_equal(ncol(res), 7)

})

# Is working
test_that("transpose14: copy parameter works with by groups", {

  stats <- proc_means(datm, stats = c("n", "mean", "median"),
                      var = c("Age", "PresentScore", "TasteScore"),
                      by = "Layers", options = v(out, notype, nonobs))

  stats

  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = "Group", by = "BY",
                         name = "STAT", id = "VAR")

  res2

  expect_equal(nrow(res2), 9)
  expect_equal(ncol(res2), 6)
  expect_equal("Group" %in% names(res2), TRUE)



})


test_that("transpose15: all vars eliminates by, copy, and id from transpose", {

  stats <- proc_means(datm, stats = c("n", "mean", "median"),
                      by = "Layers",
                      options = v(out, nonobs, notype))

  stats

  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = "Group", by = "BY",
                         name = "STAT", id = "VAR")

  res2

  expect_equal(nrow(res2), 9)
  expect_equal(ncol(res2), 6)
  expect_equal("Group" %in% names(res2), TRUE)



})


test_that("tranpose16: get_output_twoway() works as expected.", {



  res2 <- get_output_twoway(prt, "internship", "enrollment", "count",  NULL,
                            FALSE, by = c(by1 = "A", by2 = "B"))

  res2

  proc_transpose(res2, name = "STAT", id = c("CAT1", "CAT2"),
                 copy = c("by1", "by2", "VAR1", "VAR2"))

  expect_equal(nrow(res2), 4)
  expect_equal(ncol(res2), 10)

})


test_that("transpose17: NSE works on transpose", {

  stats <- proc_means(datm, stats = v(n, mean, median),
                      by = Layers,
                      options = v(out, nonobs, notype))

  stats

  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = Group, by = BY,
                         name = STAT, id = VAR)

  res2

  expect_equal(nrow(res2), 9)
  expect_equal(ncol(res2), 6)
  expect_equal("Group" %in% names(res2), TRUE)
  expect_equal(names(res2), c("Group", "BY", "STAT", "Age",
                              "PresentScore", "TasteScore"))


})

test_that("transpose18: Where clause works as expected.", {

  stats <- proc_means(datm, stats = v(n, mean, median),
                      by = Layers,
                      options = v(out, nonobs, notype))

  stats

  res1 <- data.frame(Group = "Group1", stats)

  res2 <- proc_transpose(res1, copy = Group, by = BY,
                         name = STAT, id = VAR,
                         where = expression(!BY %in% c(2, 3) | STAT != "N"))

  res2

  expect_equal(nrow(res2), 7)
  expect_equal(ncol(res2), 6)

})


test_that("transpose19: Factor with unused level works.", {

  ageg2 <- ageg


  ageg2$LABEL <- factor(ageg2$LABEL, levels = c("18 to 24", "25 to 44",
                                                "45 to 64", ">= 65",
                                                "Out of range"))


  res <- proc_transpose(ageg2,
                       var = v(N, CNTPCT),
                       copy = VAR,
                       id = CAT2,
                       by = LABEL)

  res

  expect_equal(nrow(res), 8)
  expect_equal(ncol(res), 7)

})


test_that("transpose20: log_transpose() works as expected.", {


  res <- log_transpose(mtcars, var = c("mpg", "cyl"),
                   id = c("n", "mean", "median"),
                   idlabel = c("n", "mean", "median"),
                   copy = "count", name = "sam", namelabel = "fork",
                   where = expression(x == 1),
                   by = "cyl", outdata = mtcars)

  res

  expect_equal(length(res), 10)

})


test_that("transpose21: transposing two id variables.", {

  sp <- prt2

  sp[1, 2] <- "no"



  res <- proc_freq(sp,
                   tables = c("internship"),
                   titles = "My first Frequency Table",
                   by = c("sex"),
                   weight = "count",
                   options = v(out, outcum))

  res

  res1 <- proc_transpose(res, id = c("BY", "CAT"), copy = c("VAR"))
  res1

  expect_equal(ncol(res1), 6)
  expect_equal(nrow(res1), 5)

})


test_that("transpose22: transposing inconsistent categories.", {

  sp <- prt2

  sp[1, 2] <- "no"

  res <- proc_freq(sp,
                   tables = c("internship"),
                   titles = "My first Frequency Table",
                   by = c("sex", "enrollment"),
                   weight = "count",
                   options = out)

  res

  res1 <- proc_transpose(res, id = c("BY1", "CAT"), copy = c("VAR"),
                         by = "BY2")
  res1

  expect_equal(ncol(res1), 7)
  expect_equal(nrow(res1), 6)

})


test_that("transpose23: transpose retains original class.", {

  library(tibble)

  tmp <- as_tibble(prt)

  res <- proc_transpose(tmp, by = sex)

  res

  expect_equal("tbl_df" %in% class(res), TRUE)

})


test_that("transpose24: Transpose with single by var works", {

  ageg

  res <- proc_transpose(ageg, by = VAR, id = CAT2, var = v(N, CNTPCT))

  expect_equal(nrow(res), 2)  # And gets no error


})


test_that("transpose25: transpose by retains data type.", {


  s2 <- score
  s2$Section <- as.integer(s2$Section)

  res <- proc_transpose(s2, by = Section)


  res

  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 6)
  expect_equal(typeof(res$Section), "integer")



  s2$Section <- as.double(s2$Section)

  res <- proc_transpose(s2, by = Section)


  res

  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 6)
  expect_equal(typeof(res$Section), "double")



  s2$Section <- ifelse(s2$Section == 1, TRUE, FALSE)

  res <- proc_transpose(s2, by = Section)


  res

  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 6)
  expect_equal(typeof(res$Section), "logical")



  s2$Section <- ifelse(s2$Section == TRUE, as.Date("2020-01-01"), as.Date("2021-01-01"))
  s2$Section <- as.Date(s2$Section, origin = "1970-01-01")

  res <- proc_transpose(s2, by = Section)


  res

  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 6)
  expect_equal(typeof(res$Section), "double")
  expect_equal(class(res$Section), "Date")



})


test_that("transpose26: prefix, and suffix works with no ID.", {

  res <- proc_transpose(score, prefix = "sn", suffix = "x")

  res


  expect_equal(names(res[2]), "sn1x")


})





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
                   options = "nodupkey")

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
                       options = c("dupkey", "forker"),
                       outdata = mtcars)

  res

  expect_equal(length(res), 6)

})


test_that("sort10: get_dupkey() works.", {

  prtsp <- prt
  prtsp[1, 1] <- "unk"

  prtsp

  res <- get_dupkey(prtsp,  c("sex", "internship"))

  expect_equal(nrow(prtsp), 8)
  expect_equal(nrow(res), 6)

  tbl <- table(res$sex)
  expect_equal(tbl[["boys"]], 2)
  expect_equal(tbl[["girls"]], 4)
})

test_that("sort11: dupkey works.", {

  prtsp <- prt
  prtsp[1, 1] <- "unk"

  prtsp

  res <- proc_sort(prtsp, by = c("sex", "internship"),
                   keep = c("sex", "internship"),
                   options = "dupkey"
                   )

  res

  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 2)

})


test_that("sort12: dupkey without keep works.", {

  prtsp <- prt
  prtsp[1, 1] <- "unk"

  prtsp

  res <- proc_sort(prtsp, by = c("sex", "internship"),
                   options = "dupkey"
  )

  res

  expect_equal(nrow(res), 6)
  expect_equal(ncol(res), 4)

})

test_that("sort13: Another dupkey test works.", {

  spsrt <- read.table(header = TRUE, text = '
                    ID	Name	Score
                    1	David	 74
                    2	Sam	   45
                    3	Bane	 87
                    3	Mary	 92
                    4	Dane	 23
                    5	Jenny	 87
                    6	Simran 63
                    8	Priya	 72
                    1	David	 45
                    2	Ram	   54
                    3	Bane	 87
                    5	Ken	   87')

  res <- proc_sort(spsrt, by = Name, options = "dupkey")

  expect_equal(res$Name, c("Bane", "Bane", "David", "David"))

})

test_that("sort14: proc_sort works with factors.", {

  prttmp <- prt

  prttmp$sex <- factor(prttmp$sex, c("girls", "boys"))


  res <- proc_sort(prttmp, by = c("sex", "count"))

  res

  expect_equal(as.character(res[8, 1]), "boys")
  expect_equal(as.character(res[4, 1]), "girls")


})


test_that("sort15: as.character parameter works as expected.", {

  prttmp <- prt

  prttmp$sex <- factor(prttmp$sex, c("girls", "boys"))


  res <- proc_sort(prttmp, by = c("sex", "count"), as.character = TRUE)

  res

  expect_equal(res[8, 1], "boys")
  expect_equal(res[4, 1], "girls")


})


test_that("sort16: parameter checks work.", {

  expect_error(proc_sort("bork", by = c("sex")))
  expect_error(proc_sort(prt, by = c("fork")))

  # No longer throws error
  # expect_error(proc_sort(prt[0, ], by = c("sex")))

})

test_that("sort17: na.sort parameter works.", {


  spsrt <- read.table(header = TRUE, text = '
                    ID	Name	Score
                    1	David	 74
                    2	Sam	   45
                    3	Mary	 NA
                    4	Dane	 23
                    5	Jenny	 87
                    6	Simran 27
                    7	Priya	 72
                    8	David	 45
                    9	Ram	   54
                    10	Bane	 87')

  # "last"
  res <- proc_sort(spsrt, by = Score, na.sort = "last")

  expect_equal(is.na(res$Score[10]), TRUE)

  # "first"
  res <- proc_sort(spsrt, by = Score, na.sort = "first")

  expect_equal(is.na(res$Score[1]), TRUE)


  # "sas" ascending
  res <- proc_sort(spsrt, by = Score, na.sort = "sas")

  expect_equal(is.na(res$Score[1]), TRUE)


  # "sas" decending
  res <- proc_sort(spsrt, by = Score, order = "d", na.sort = "sas")

  expect_equal(is.na(res$Score[10]), TRUE)

  # Test global options
  options("procs.na.sort" = "sas")

  # "sas" ascending
  res <- proc_sort(spsrt, by = Score)

  expect_equal(is.na(res$Score[1]), TRUE)


  options("procs.na.sort" = NULL)


  df <- data.frame(
    id = c(1,2,3,4),
    a  = c(NA, 2, 2, 1),
    b  = c(1,  NA,  1, 2)
  )

  # First
  res <- proc_sort(df, by = c("a", "b"), order = "d", na.sort = "first")
  expect_equal(is.na(res$a[1]), TRUE)
  expect_equal(is.na(res$b[2]), TRUE)

  # Last
  res <- proc_sort(df, by = c("a", "b"), order = "d", na.sort = "last")
  expect_equal(is.na(res$a[4]), TRUE)
  expect_equal(is.na(res$b[2]), TRUE)


  # SAS desc
  res <- proc_sort(df, by = c("a", "b"), order = c("d", "a"), na.sort = "sas")

  expect_equal(is.na(res$a[4]), TRUE)
  expect_equal(is.na(res$b[1]), TRUE)



  df <- data.frame(
    id = c(1,2,3,4),
    a  = c(NA, NA, 2, 1),
    b  = c(NA,  2,  1, 2)
  )


  # SAS desc - Good
  res <- proc_sort(df, by = c("a", "b"), order = c("d", "d"), na.sort = "sas")

  expect_equal(is.na(res$a[4]), TRUE)
  expect_equal(is.na(res$a[3]), TRUE)
  expect_equal(is.na(res$b[4]), TRUE)


  # SAS desc  - Good
  res <- proc_sort(df, by = c("a", "b"), order = c("a", "a"), na.sort = "sas")

  expect_equal(is.na(res$a[1]), TRUE)
  expect_equal(is.na(res$a[2]), TRUE)
  expect_equal(is.na(res$b[1]), TRUE)

  # SAS desc - Also Good!
  res <- proc_sort(df, by = c("a", "b"), order = c("a", "d"), na.sort = "sas")

  expect_equal(is.na(res$a[1]), TRUE)
  expect_equal(is.na(res$a[2]), TRUE)
  expect_equal(is.na(res$b[2]), TRUE) # ? Yes, actually correct

  # SAS desc - Hurray!
  res <- proc_sort(df, by = c("a", "b"), order = c("d", "a"), na.sort = "sas")

  expect_equal(is.na(res$a[4]), TRUE)
  expect_equal(is.na(res$a[3]), TRUE)
  expect_equal(is.na(res$b[3]), TRUE) # ? Yes, actually correct


  # User data
  df <- data.frame(
    id = c(1,2,3,4),
    a  = c(NA, 2, NA, 1),
    b  = c(2,  1,  1, 2)
  )

  # User test
  res <- proc_sort(df, by = c("a", "b"), na.sort = "sas")

  expect_equal(is.na(res$a[1]), TRUE)
  expect_equal(is.na(res$a[2]), TRUE)


  # Unquoted
  res <- proc_sort(df, by = c("a", "b"), na.sort = sas)

  expect_equal(is.na(res$a[1]), TRUE)
  expect_equal(is.na(res$a[2]), TRUE)


  dat <- data.frame(
    x = c(1, NA, 3)
  )

  first <- "sas"

  proc_sort(dat, by = x, na.sort = first)



})


test_that("sort18: na.sort parameter checks work.", {


  spsrt <- read.table(header = TRUE, text = '
                    ID	Name	Score
                    1	David	 74
                    2	Sam	   45
                    3	Mary	 NA
                    4	Dane	 23
                    5	Jenny	 87
                    6	Simran 27
                    7	Priya	 72
                    8	David	 45
                    9	Ram	   54
                    10	Bane	 87')

  # "last"
  expect_error(proc_sort(spsrt, by = Score, na.sort = "fork"))


})

test_that("sort19: Returns empty data.frame unchanged when input has 0 rows", {

  df <- data.frame(X = character())

  expect_silent({
    out <- proc_sort(df, by = X)
    expect_identical(out, df)
    expect_equal(nrow(out), 0L)
  })

})


test_that("sort20: where expression works as expected.", {

  res <- proc_sort(prt,
                   where = expression(sex == "boys"))


  expect_equal(all(res$sex == "boys") , TRUE)


})

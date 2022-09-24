base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE


# This is really an interactive test.
# Works if html shows up in RStudio viewer.
test_that("utils0: show_viewer works as expected with local path.", {

    html <- "<html><body><p>test0</p></body></html>"


    fp <- file.path(base_path, "/utils/test0.html")

    dir <- dirname(fp)
    if (dir.exists(dir) == FALSE)
      dir.create(dir)

    if (file.exists(fp))
      file.remove(fp)

    fl <- file(fp, open = "w")

    writeLines(html, con = fl)

    close(fl)


    res <- show_viewer(fp)

    expect_equal(file.exists(res), TRUE)

})


# This is really an interactive test.
# Works if html shows up in RStudio viewer.
test_that("utils1: show_viewer works as expected with temp path.", {


    html <- "<html><body><p>test1</p></body></html>"

    td <- tempdir()
    if (!file.exists(td))
      dir.create(td)

    td <- file.path(td, "utils")
    if (!file.exists(td))
      dir.create(td)

    fp <- file.path(td, "test1.html")

    if (file.exists(fp))
      file.remove(fp)

    fl <- file(fp, open = "w")

    writeLines(html, con = fl)

    close(fl)


    res <- show_viewer(fp)

    expect_equal(file.exists(res), TRUE)


})


test_that("utils2: output_report works as expected.", {


  fp <- file.path(base_path, "/utils/test2.html")

  lst <- list(mtcars)

  res <- output_report(lst,
                       dir_name = dirname(fp),
                       file_name = "test2")

  ex <- file.exists(fp)

  expect_equal(ex, TRUE)


})


test_that("utils3: filenm works as expected.", {


  fl <- file.path(base_path, "/utils/test3.html")

  res <- filenm(fl)
  res

  expect_equal(res, "test3")


  fl <- file.path(base_path, "test3")

  res <- filenm(fl)
  res

  expect_equal(res, "test3")

})

test_that("utils4: option_true() works as expected.", {

  opt <- NULL

  expect_equal(option_true(opt, "cumsum"), FALSE)

  opt <- v(fork)

  expect_equal(option_true(opt, "cumsum"), FALSE)


  opt <- v(fork, cumsum = FALSE)

  expect_equal(option_true(opt, "cumsum"), FALSE)


  opt <- v(fork, cumsum = TRUE)

  expect_equal(option_true(opt, "cumsum"), TRUE)

  opt <- v(fork, cumsum)

  expect_equal(option_true(opt, "cumsum"), TRUE)

})

test_that("utils4: has_option() works as expected.", {

  opt <- NULL

  expect_equal(has_option(opt, "cumsum"), FALSE)

  opt <- v(fork = TRUE)

  expect_equal(has_option(opt, "cumsum"), FALSE)


  opt <- v(fork = TRUE, cumsum = FALSE)

  expect_equal(has_option(opt, "cumsum"), TRUE)


  opt <- v(fork, cumsum)

  expect_equal(has_option(opt, "cumsum"), TRUE)

})

test_that("utils5: get_name() works as expected.", {


  expect_equal(get_name(NULL, "myvar", NULL), "myvar")
  expect_equal(get_name("myname", "myvar", NULL), "myname")
  expect_equal(get_name("myname", "myvar", "Grp=1, "), "Grp=1, myname")
  expect_equal(get_name(NULL, "myvar", "Grp=1, "), "Grp=1, myvar")


})



test_that("utils8: Options are case-insensitive", {

  opts <- v(Fisher = TRUE)

  res <- get_option(opts, "fisher", FALSE)

  res

  expect_equal(res, TRUE)

  res <- get_option(opts, "fishEr", FALSE)

  res

  expect_equal(res, TRUE)

  expect_equal(has_option(opts, "FISHER"), TRUE)
  expect_equal(has_option(opts, "FASHER"), FALSE)


  opts <- v(Fisher)

  res <- get_option(opts, "fishEr", FALSE)

  res

  expect_equal(res, TRUE)


})

test_that("utils9: get_option() returns appropriate data type.", {

  opts <- v(Fisher = TRUE, fork, bork = c(1, 2, 3), stork = c("A", "B", "C"))

  res <- get_option(opts, "fishEr", FALSE)

  res

  expect_equal(typeof(res), "logical")

  res <- get_option(opts, "bork", FALSE)

  res

  expect_equal(typeof(res), "double")


  res <- get_option(opts, "fork", FALSE)

  res

  expect_equal(typeof(res), "logical")


  res <- get_option(opts, "stork", FALSE)

  res

  expect_equal(typeof(res), "character")

  res <- get_option(opts, "stack", FALSE)

  res

  expect_equal(typeof(res), "logical")

})

#
# test_that("utils9: out() function works as expected.", {
#
#
#
#   out <- out(stats = c("mean", "median", "min", "max"), shape = "wide",
#                 fork = "sammy", bork = c("one", "two", "three"), table = "A")
#
#   out
#
#   expect_equal(length(out$stats), 4)
#   expect_equal(out$shape, "wide")
#   expect_equal(out$parameters$fork, "sammy")
#   expect_equal(length(out$parameters$bork), 3)
#   expect_equal(out$table, "A")
#
#
# })



# test_that("utils10: has_report() function works.", {
#
#   expect_equal(has_report(NULL), FALSE)
#
#   opts1 <- list(out = out(stats = "n"))
#
#   expect_equal(has_report(opts1), FALSE)
#
#   opts2 <- list(out = out(stats = "n"),
#                 out2 = out(report = TRUE))
#
#
#   expect_equal(has_report(opts2), TRUE)
#
#   nm <- get_report_name(opts2)
#   expect_equal(nm, "out2")
#
# })


# test_that("utils11: opts() works with list.", {
#
#   lst <- list(A = 1, B = 2)
#
#   res <- opts(lst)
#
#   expect_equal(class(res), c("opts", "list"))
#   expect_equal(res$A, 1)
#   expect_equal(res$B, 2)
#
#   res2 <- opts(C = 3, D = 4)
#
#   expect_equal(class(res2), c("opts", "list"))
#   expect_equal(res2$C, 3)
#   expect_equal(res2$D, 4)
#
#
#   func1 <- function(mylist) {
#
#     ret <- opts(mylist)
#
#     return(ret)
#   }
#
#   res3 <- func1(lst)
#
#   expect_equal(class(res3), c("opts", "list"))
#   expect_equal(res3$A, 1)
#   expect_equal(res3$B, 2)
#
#
#   res4 <- opts()
#
#   expect_equal(class(res4), c("opts", "list"))
#   expect_equal(length(res4), 0)
#
#
# })

test_that("utils12: fill_missing() function works as expected.", {


  df <- data.frame(one = 1, two = 2, three = "3", stringsAsFactors = FALSE)

  expect_equal(nrow(df), 1)
  expect_equal(ncol(df), 3)

  res <- fill_missing(df, 4)

  res

  expect_equal(nrow(res), 4)
  expect_equal(ncol(res), 3)

})


# test_that("utils13: out() NSE works as expected.", {
#
#
#   res <- out(table = flork, stats = bork, drop = sam,
#              keep = fred, sammy = TRUE)
#
#
#   res
#
#   expect_equal(res$table, "flork")
#   expect_equal(res$stats, "bork")
#   expect_equal(res$keep, "fred")
#   expect_equal(res$parameters$sammy, TRUE)
#   expect_equal(res$drop, "sam")
#
# })

test_that("utils14: get_option works with formats.", {

  v1 <- v(format = "%1.2f%%")


  res <- get_option(v1, "format")


  expect_equal(res, "%1.2f%%")

  v2 <- v(format = c(1, 2, 3))

  res <- get_option(v2, "format")

  res

  expect_equal(res, c(1, 2, 3))


})

#
# test_that("utils12: stackds() function works as expected.", {
#
#
#   stats <- proc_means(datm, stats = v(n, mean, median),
#                       by = Layers)
#
#   stats
#
#
#   res2 <- proc_transpose(res1, by = v(VAR, Layers),
#                          name = STAT)
#
#   res2
#
#   # expect_equal(nrow(res2), 9)
#   # expect_equal(ncol(res2), 6)
#   # expect_equal("Group" %in% names(res2), TRUE)
#
#
#
#
# })


# test_that("utils14: parse_tables() works as expected.", {
#
#   nms <- v(A, B, C, D)
#
#
#   v1 <- c("A--C)")
#   v1
#
#   res <- parse_tables(nms, v1)
#
#
#   v2 <- c("A * (B C)")
#   nms
#   v2
#
#   res <- parse_tables(nms, v2)
#
#   res
#
#   expect_equal(res, c("A * B", "A * C"))
#
#   v2 <- c("(A B) * (C D)")
#   res2 <- parse_tables(nms, v2)
#
#   expect_equal(res, c("A * C", "B * C", "A * C", "B * D"))
#
#
#
# })

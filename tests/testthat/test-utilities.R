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

  res <- output_report(lst, proc_type = "freq",
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

  opt <- list(fork = TRUE)

  expect_equal(option_true(opt, "cumsum"), FALSE)


  opt <- list(fork = TRUE, cumsum = FALSE)

  expect_equal(option_true(opt, "cumsum"), FALSE)


  opt <- list(fork = TRUE, cumsum = TRUE)

  expect_equal(option_true(opt, "cumsum"), TRUE)

})

test_that("utils4: option_true() works as expected.", {

  opt <- NULL

  expect_equal(has_option(opt, "cumsum"), FALSE)

  opt <- list(fork = TRUE)

  expect_equal(has_option(opt, "cumsum"), FALSE)


  opt <- list(fork = TRUE, cumsum = FALSE)

  expect_equal(has_option(opt, "cumsum"), TRUE)

})

test_that("utils5: get_name() works as expected.", {


  expect_equal(get_name(NULL, "myvar", NULL), "myvar")
  expect_equal(get_name("myname", "myvar", NULL), "myname")
  expect_equal(get_name("myname", "myvar", "Grp=1, "), "Grp=1, myname")
  expect_equal(get_name(NULL, "myvar", "Grp=1, "), "Grp=1, myvar")


})


test_that("utils6: v() function works", {

  res <- v(a, b, c)


  expect_equal(res, c("a", "b", "c"))

})

test_that("utils6: v() function works with interaction", {

  res <- v(a, b, c * d)

  res

  expect_equal(res, c("a", "b", "c * d"))

})

test_that("utils7: round_up() function works as expected.", {

  x <- seq(0.5,9.5,by=1)

  res <- round_up(x, 0)

  res


  expect_equal(res, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

})

test_that("utils8: Options are case-insensitive", {

  opts <- list(Fisher = TRUE)

  res <- get_option(opts, "fisher", FALSE)

  res

  expect_equal(res, TRUE)

  res <- get_option(opts, "fishEr", FALSE)

  res

  expect_equal(res, TRUE)

  expect_equal(has_option(opts, "FISHER"), TRUE)
  expect_equal(has_option(opts, "FASHER"), FALSE)


})


test_that("utils9: round_up() matches SAS and not R", {


  vct0 <- c(-2.5, -1.5, -.5, 1.5, 2.5)
  vct1 <- c(8.75, 8.85, 8.95, 9.05, 9.15, 9.25)

  res0 <- round_up(vct0)

  sasres0 <- c(-3, -2, -1, 2,3)
  expect_equal(all(res0 == sasres0), TRUE)

  rres0 <- round(vct0)
  expect_equal(all(res0 == rres0), FALSE)

  res1 <- round_up(vct1, 1)

  sasres1 <- c(8.8, 8.9, 9.0, 9.1, 9.2, 9.3)
  expect_equal(all(res1 == sasres1), TRUE)

  rres1 <- round(vct1)
  expect_equal(all(res1 == rres1), FALSE)


})



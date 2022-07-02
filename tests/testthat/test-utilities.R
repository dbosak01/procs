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


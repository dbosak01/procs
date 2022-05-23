base_path <- "c:/packages/procs/tests/testthat/utils"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE


# This is really an interactive test.
# Works if html shows up in RStudio viewer.
test_that("utils0: show_viewer works as expected with local path.", {

  if (dev) {
    html <- "<html><body><p>test0</p></body></html>"


    fp <- file.path(base_path, "test0.html")

    if (file.exists(fp))
      file.remove(fp)

    fl <- file(fp, open = "w")

    writeLines(html, con = fl)

    close(fl)


    res <- show_viewer(fp)
  }

  expect_equal(1, 1)

})


# This is really an interactive test.
# Works if html shows up in RStudio viewer.
test_that("utils1: show_viewer works as expected with temp path.", {

  if (dev) {
    html <- "<html><body><p>test1</p></body></html>"

    td <- tempdir()
    if (!file.exists(td))
      dir.create(td)

    fp <- file.path(td, "test1.html")

    if (file.exists(fp))
      file.remove(fp)

    fl <- file(fp, open = "w")

    writeLines(html, con = fl)

    close(fl)


    res <- show_viewer(fp)

  }


  expect_equal(1, 1)

})


test_that("utils2: output_report works as expected.", {


  fp <- file.path(base_path, "test2.html")

  lst <- list(mtcars)

  res <- output_report(lst, proc_type = "freq", fp)

  ex <- file.exists(res)

  expect_equal(ex, TRUE)

  # if (!dev & ex)
  #   file.remove(ex)

})


test_that("utils3: filenm works as expected.", {


  fl <- file.path(base_path, "test3.html")

  res <- filenm(fl)
  res

  expect_equal(res, "test3")


  fl <- file.path(base_path, "test3")

  res <- filenm(fl)
  res

  expect_equal(res, "test3")

})

base_path <- "c:/packages/procs/tests/testthat/utils"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dev <- FALSE


# This is really an interactive test.
# Works if html shows up in RStudio viewer.
test_that("utils0: show_viewer works as expected with local path.", {

    html <- "<html><body><p>test0</p></body></html>"


    fp <- file.path(base_path, "test0.html")

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


  fp <- file.path(base_path, "test2.html")

  lst <- list(mtcars)

  res <- output_report(lst, proc_type = "freq", dir_name = base_path,
                       file_name = "test2")

  ex <- file.exists(fp)

  expect_equal(ex, TRUE)


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

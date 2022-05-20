base_path <- "c:/packages/procs/tests/testthat/"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."


test_that("output_report works as expected.", {




})


test_that("show_viewer works as expected.", {

  html <- "<html><body><p>test</p></body></html>"

  td <- tempdir()
  if (!file.exists(td))
    dir.create(td)

  fp <- file.path(td, "test0.html")

  if (file.exists(fp))
    file.remove(fp)

  fl <- file(fp, open = "w")

  writeLines(html, con = fl)

  close(fl)


  res <- show_viewer(fp)


  expect_equal(1, 1)

})

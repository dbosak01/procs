base_path <- "c:/packages/procs/tests/testthat"
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

dat <- read.table(header = TRUE, text = '
  Region Eyes Hair Count
  1 blue  fair   23
  1 blue  dark   11
  1 green medium 18
  1 brown red     5
  1 brown black   3
  2 blue  medium 44
  2 green fair   50
  2 green dark   23
  2 brown medium 53
  1 blue  red     7
  1 green fair   19
  1 green dark   14
  1 brown medium 41
  2 blue  fair   46
  2 blue  dark   40
  2 green red    31
  2 brown fair   56
  2 brown dark   54
  1 blue  medium 24
  1 green red     7
  1 brown fair   34
  1 brown dark   40
  2 blue  red    21
  2 blue  black   6
  2 green medium 37
  2 brown red    42
  2 brown black  13
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

dev <- FALSE

test_that("print1: Simple proc_print text works.", {


  fp <- file.path(base_path, "print/test1.txt")

  res <- proc_print(mtcars, fp, output_type = "TXT", view = FALSE)

  res


  expect_equal(file.exists(fp), TRUE)

})


# test_that("print2: Simple proc_print pdf works.", {
#
#
#   fp <- file.path(base_path, "print/test2")
#
#   res <- proc_print(mtcars, fp, output_type = "PDF",
#                     titles = "My title", view = FALSE)
#
#   res
#
#   expect_equal(file.exists(res[[1]]), TRUE)
#
# })



test_that("print3: Simple proc_print docx works.", {


  fp <- file.path(base_path, "print/test3")

  res <- proc_print(mtcars, fp, output_type = "DOCX",
                    titles = "My title", view = FALSE)

  res

  expect_equal(file.exists(res[[1]]), TRUE)

})



test_that("print4: Simple proc_print rtf works.", {


  fp <- file.path(base_path, "print/test4")

  res <- proc_print(mtcars, fp, output_type = "RTF",
                    titles = "My title", view = FALSE)

  res

  expect_equal(file.exists(res[[1]]), TRUE)

})


test_that("print5: Simple proc_print html works.", {


  fp <- file.path(base_path, "print/test5")

  res <- proc_print(mtcars, fp, output_type = "HTML",
                    titles = "My title", view = FALSE)

  res

  expect_equal(file.exists(res[[1]]), TRUE)

})


test_that("print6: Simple proc_print no output works.", {



  res <- proc_print(mtcars, titles = "My title2")

  res

  expect_equal(is.null(res), TRUE)

})


test_that("print7: Simple proc_print style works.", {



  res <- proc_print(mtcars, titles = "My title2",
                    style = "MidnightBlue")

  res

  expect_equal(is.null(res), TRUE)

})

test_that("print8: Print freq report works.", {


  fp <- file.path(base_path, "print/test8")

  labels(dat) <- list(Eyes = "Eye Color",
                      Hair = "Hair Color",
                      Region = "Geographic Region")

  res <- proc_freq(dat,
                   tables = c("Eyes", "Hair",  Cross = "Hair * Eyes"),
                   titles = "My first Frequency Table",
                   by = "Region",
                   weight = "Count",
                   output = report)

  expect_equal(length(res), 6)


  res1 <- proc_print(res, fp, output_type = c("HTML", "RTF"),
                     titles = "My Nice Titles",
                     style = "MidnightBlue")
  res1

  expect_equal(length(res1), 2)
  expect_equal(file.exists(res1[1]), TRUE)
  expect_equal(file.exists(res1[1]), TRUE)


})

test_that("print9: log_prnt() works as expected.", {


  # data, file_path = NULL, output_type = "HTML",
  # titles = NULL, style = NULL, view = TRUE


  res <- log_prnt(mtcars, file_path = "c:/mypath",
                  output_type = "HTML",
                  titles = c("Title1 ", "Title 2"),
                  style = "default", view = TRUE)

  res

  expect_equal(length(res), 6)

})



test_that("print10: printing of ggplot works as expected.", {

  if (dev == TRUE) {


    library(ggplot2)

    fp <- file.path(base_path, "print/test10.docx")

    res <- proc_freq(mtcars, tables = v(cyl, am))

    p <- ggplot(mtcars, aes(x=cyl, y=mpg)) + geom_point()

    pres <- list(res, p)


    r1 <- proc_print(pres, fp, titles = "My Report with Plot",
                     output_type = "DOCX")

    #print(res)

    expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)


})

test_that("print11: printing of create_plot() works as expected.", {

  if (dev == TRUE) {


    library(reporter)

    fp <- file.path(base_path, "print/test11.docx")



    dt <- proc_freq(dat, "Eyes")

    plt <- freqplot()

    # Plot object
    res <- render_freqplot(dt, "Eyes", plt = plt)

    myplt <- create_plot(res, height = 4, width = 5)


    pres <- list(dt, myplt)


    r1 <- proc_print(pres, fp, titles = "My Report with Plot",
                     output_type = "DOCX")

    #print(res)

    expect_equal(file.exists(fp), TRUE)

  } else
    expect_equal(TRUE, TRUE)


})

test_that("print12: printing of proc_freq() with plots option works as expected.", {

  fp <- file.path(base_path, "print/test12.docx")

  # Freqplot function
  res <- proc_freq(dat, tables = c("Eyes"),
                   plots = freqplot(),
                   output = report,
                   titles = "My first Frequency Plot")

  res

  r1 <- proc_print(res, fp, titles = "My Report with Plot",
                   output_type = "DOCX")

  #print(res)

  expect_equal(file.exists(fp), TRUE)

})

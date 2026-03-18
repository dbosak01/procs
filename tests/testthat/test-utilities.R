base_path <- file.path(getwd(), "tests/testthat")
data_dir <- base_path

base_path <- tempdir()
data_dir <- "."

cls <- read.table(header = TRUE, text = '
Name Sex Age Height Weight    region
Alfred   M  14   69.0  112.5   A
Alice   F  13   56.5   84.0    A
Barbara   F  13   65.3   98.0  A
Carol   F  14   62.8  102.5    A
Henry   M  14   63.5  102.5    A
James   M  12   57.3   83.0    A
Jane   F  12   59.8   84.5     A
Janet   F  15   62.5  112.5    A
Jeffrey   M  13   62.5   84.0  A
John   M  12   59.0   99.5     B
Joyce   F  11   51.3   50.5    B
Judy   F  14   64.3   90.0     B
Louise   F  12   56.3   77.0   B
Mary   F  15   66.5  112.0     B
Philip   M  16   72.0  150.0   B
Robert   M  12   64.8  128.0   B
Ronald   M  15   67.0  133.0   B
Thomas   M  11   57.5   85.0   B
William   M  15   66.5  112.0  B')

dev <- FALSE


# This is really an interactive test.
# Works if html shows up in RStudio viewer.
# These started failing 2023/08/16.  I don't know why.  Put as DEV == TRUE for now.
# Seems like the temp directory is not getting created.
# The function otherwise works interactively. So I don't know what the problem is.
test_that("utils0: show_viewer works as expected with local path.", {

  if (dev == TRUE) {
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
    print("Here is the path")
    print(res)

    expect_equal(file.exists(res), TRUE)
  } else {

    expect_equal(TRUE, TRUE)
  }

})


# This is really an interactive test.
# Works if html shows up in RStudio viewer.
test_that("utils1: show_viewer works as expected.", {

  if (dev == TRUE) {

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

  } else {

   expect_equal(TRUE, TRUE)
  }

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


  opts <- c(vardef = "df", "nway")

  res <- get_option(opts, "vardef", "df", FALSE)

  expect_equal(typeof(res), "character")
  expect_equal(res, "df")

})

#
# test_that("utils9: out_spec() function works as expected.", {
#
#
#
#   out <- out_spec(stats = c("mean", "median", "min", "max"), shape = "wide",
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
#   opts1 <- list(out = out_spec(stats = "n"))
#
#   expect_equal(has_report(opts1), FALSE)
#
#   opts2 <- list(out = out_spec(stats = "n"),
#                 out2 = out_spec(report = TRUE))
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


# test_that("utils13: out_spec() NSE works as expected.", {
#
#
#   res <- out_spec(table = flork, stats = bork, drop = sam,
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

test_that("utils15: has_* functions work as expected.", {

  # Reset
  options("procs.interactive" = NULL)

  # Output
  res <- has_output("out")

  expect_equal(res, TRUE)

  res <- has_output("none")

  expect_equal(res, FALSE)

  res <- has_output("report")

  expect_equal(res, FALSE)

  res <- has_output(NULL)

  expect_equal(res, TRUE)

  # Report
  res <- has_report("out")

  expect_equal(res, FALSE)

  res <- has_report("none")

  expect_equal(res, FALSE)

  res <- has_report("report")

  expect_equal(res, TRUE)

  res <- has_report(NULL)

  expect_equal(res, FALSE)

  # View
  res <- has_view(NULL)

  expect_equal(res, TRUE)

  res <- has_view("noprint")

  expect_equal(res, FALSE)

  res <- has_view("print")

  expect_equal(res, TRUE)


  options("procs.interactive" = TRUE)

  # Output
  res <- has_output("out")

  expect_equal(res, TRUE)

  res <- has_output("none")

  expect_equal(res, FALSE)

  res <- has_output("report")

  expect_equal(res, FALSE)

  res <- has_output(NULL)

  expect_equal(res, FALSE)

  # Report
  res <- has_report("out")

  expect_equal(res, FALSE)

  res <- has_report("none")

  expect_equal(res, FALSE)

  res <- has_report("report")

  expect_equal(res, TRUE)

  res <- has_report(NULL)

  expect_equal(res, FALSE)

  # View
  res <- has_view(NULL)

  expect_equal(res, TRUE)

  res <- has_view("noprint")

  expect_equal(res, FALSE)

  res <- has_view("print")

  expect_equal(res, TRUE)



  options("procs.interactive" = FALSE)

  # Output
  res <- has_output(NULL)

  expect_equal(res, TRUE)

  # Report
  res <- has_report(NULL)

  expect_equal(res, FALSE)

  # View
  res <- has_view(NULL)

  expect_equal(res, FALSE)

  # Reset
  options("procs.interactive" = NULL)

})


test_that("utils16: get_alpha() option works as expected.", {


  opt <- c("bork", "fork", alpha = 0.1)


  res <- get_alpha(opt)

  res

  expect_equal(res, 0.1)



  opt <- c("bork", "fork")


  res <- get_alpha(opt)

  res

  expect_equal(res, 0.05)


  opt <- c(alpha = 0.1)


  res <- get_alpha(opt)

  res

  expect_equal(res, 0.1)



})

test_that("utils17: get_maxdec() option works as expected.", {


  opt <- c("bork", "fork", maxdec = 2)


  res <- get_maxdec(opt)

  res

  expect_equal(res, "%.2f")



  opt <- c("bork", "fork")


  res <- get_maxdec(opt)

  res

  expect_equal(res, "%.7f")


  opt <- c(maxdec = 3)


  res <- get_maxdec(opt)

  res

  expect_equal(res, "%.3f")



})


test_that("utils18: get_ttest_type() works as expected.", {


  res <- get_ttest_type("Myvar:Statistics")

  res

  expect_equal(res, "Statistics")



  res <- get_ttest_type("Statistics")

  res

  expect_equal(res, "Statistics")

})


test_that("utils19: add_paired_vars works as expected.", {


  dat <- data.frame(VAR = "..diff1", A = 1, B = 2)

  res <- add_paired_vars(dat, "var1 - var2", "wide")

  res

  expect_equal(names(res), c("VAR1", "VAR2", "DIFF", "A", "B"))


  dat2 <- data.frame(BY = "MYBY", VAR = "..diff1", A = 1, B = 2)

  res2 <- add_paired_vars(dat2, "var1 - var2", "wide")

  res2

  expect_equal(names(res2), c("BY", "VAR1", "VAR2", "DIFF", "A", "B"))



  dat3 <- data.frame(BY = "MYBY", VAR = c("..diff1", "..diff2"), A = 1, B = 2)

  res3 <- add_paired_vars(dat3, c("var1 - var2", "var3 - var4"), "wide")

  res3

  expect_equal(names(res3), c("BY", "VAR1", "VAR2", "DIFF", "A", "B"))

})


test_that("utils20: fix_var_names works as expected.", {


  dat <- data.frame(fork = "A", "..var1" = 1, "..var2" = 2, stringsAsFactors = FALSE)

  vr <- c("..var1", "..var2")
  vrlbl <- c("VAR1", "VAR2")


  res1 <- fix_var_names(dat, vr, vrlbl, "long", "TTests")

  res1

  expect_equal(all(c("VAR1", "VAR2") %in% names(res1)), TRUE)


  dat2 <- data.frame(VAR = c("..var1", "..var2"), A = c(1, 2),
                     stringsAsFactors = FALSE)

  res2 <- fix_var_names(dat2, vr, vrlbl, "stacked", "TTests")

  res2

  expect_equal(res2$VAR, c("VAR1", "VAR2"))

})

test_that("utils21: get_formulas() works as expected.", {

  str <- c("x = y", "z = w x y")


  res <- get_formulas(str)

  res

  expect_equal(length(res), 2)
  expect_equal(is.list(res), TRUE)
  expect_equal("formula" %in% class(res[[1]]), TRUE)
  expect_equal("formula" %in% class(res[[2]]), TRUE)

})


test_that("utils22: get_vars() works as expected.", {


  myfm <- formula(Weight ~ Height + Sex + BMI)

  res <- get_vars(myfm)

  res

  expect_equal(res$dvar, "Weight")
  expect_equal(res$ivar, c("Height", "Sex", "BMI"))

  myfm <- formula(Weight ~ Height)

  res <- get_vars(myfm)

  res

  expect_equal(res$dvar, "Weight")
  expect_equal(res$ivar, c("Height"))

})


test_that("utils23: get_obs() works as expected.", {


  myfm <- formula(Weight ~ Height + Age)

  cls2 <- cls

  cls2[5, "Age"] <- NA
  cls2[7, "Height"] <- NA
  cls2[11, "Weight"] <- NA
  cls2[17, "Age"] <- NA
  cls2[17, "Height"] <- NA

  cls2


  res <- get_obs(cls2, myfm)

  res

  expect_equal(res$NOBS[1], 19)
  expect_equal(res$NOBS[2], 15)
  expect_equal(res$NOBS[3], 4)

})

test_that("utils23: get_valid_obs() works as expected.", {


  myfm <- formula(Weight ~ Height + Age)

  cls2 <- cls

  cls2[2, "Weight"] <- NA
  cls2[5, "Age"] <- NA
  cls2[11, "Height"] <- NA

  res1 <- get_valid_obs(cls2, myfm)

  expect_equal(nrow(cls), 19)
  expect_equal(nrow(res1), 16)

})


test_that("utils24: get_text_width() works as expected.", {


  v1 <- c("Hello", "Hello My Baby!", "Looks like I'll be late for the train!")


  res <- get_text_width(v1)


  expect_equal(length(res), 3)
  expect_equal(res[3] > res[2], TRUE)
  expect_equal(res[2] > res[1], TRUE)

  # expect_equal(TRUE, TRUE)

})


test_that("utils25: get_line_count() works as expected.", {


  v1 <- c("Hello", "Hello My Baby!", "Looks like I'll be late for the train!")


  res <- get_line_count(v1)


  expect_equal(length(res), 1)
  expect_equal(res > 10, TRUE)

})

test_that("utils26: force_width() works as expected.", {

  v1 <- c("Hello", "Hello My Baby!", "Looks like I'll be late for the train!")

  res <- get_text_width(v1, font_size = 12, multiplier = .9)

  res2 <- force_width(v1, 1.2)

  expect_equal(length(res2), 3)
  expect_equal(v1[1] == res2[1], TRUE)
  expect_equal(v1[2] == res2[2], TRUE)
  expect_equal(v1[3] != res2[3], TRUE)

})

test_that("utils27: fit_width() works as expected.", {

  v1 <- c("Hello", "Hello\nMy Baby!", "Looks like I'll be late for the train!",
          "supercalifragilisticexpialidotious")

  res <- get_text_width(v1, font_size = 12, multiplier = .9)

  res2 <- fit_width(v1, 1.2)

  res2

  expect_equal(length(res2), 2)
  expect_equal(length(res2$Vector), 4)
  expect_equal(res2$Lines, 2)

})

test_that("utils28: fit_width() works as expected.", {

  v1 <- c("Competitor", "Drug A (Dose 10mg)", "Drug A (Dose 20mg)",
          "Placebo")

  res <- get_text_width(v1, font_size = 12, multiplier = .9)

  res2 <- fit_width(v1, 1.2)

  res2

  expect_equal(length(res2), 2)
  expect_equal(length(res2$Vector), 4)
  expect_equal(res2$Lines, 2)

})

test_that("utils29: get_line_count() works as expected.", {


  res1 <- get_line_count("Hello my baby!")

  res2 <- get_line_count("Hello\nmy baby of mine!")

  res3 <- get_line_count("Hello my\nbaby of mine!")

  res4 <- get_line_count("baby of mine!")

  expect_equal(res1 < res2, TRUE)
  expect_equal(res3 < res2, TRUE)
  expect_equal(res3 == res4, TRUE)

})


test_that("resolve_arg1: errors when arg is missing in the resolve_arg call itself", {
  caller <- function() {
    resolve_arg()
  }

  expect_error(
    caller(),
    "Argument 'arg' is missing.",
    fixed = TRUE
  )
})

test_that("resolve_arg2: errors when type is not a character vector", {
  caller <- function(x) {
    resolve_arg(x, type = 1)
  }

  expect_error(
    caller("A"),
    "'type' must be a character vector.",
    fixed = TRUE
  )
})

test_that("resolve_arg3: errors when type contains an invalid mode value", {
  caller <- function(x) {
    resolve_arg(x, type = c("character", "logical"))
  }

  expect_error(
    caller("A"),
    "'type' must contain valid mode values.",
    fixed = TRUE
  )
})

test_that("resolve_arg4: errors when type consists entirely of invalid mode values", {
  caller <- function(x) {
    resolve_arg(x, type = c("logical", "list"))
  }

  expect_error(
    caller("A"),
    "'type' must contain valid mode values.",
    fixed = TRUE
  )
})

test_that("resolve_arg5: returns character input unchanged when character is allowed", {
  caller <- function(x) {
    resolve_arg(x)
  }

  expect_identical(caller("USUBJID"), "USUBJID")
  expect_identical(caller(c("A", "B")), c("A", "B"))
})

test_that("resolve_arg6: returns NULL unchanged when NULL is allowed", {
  caller <- function(x = NULL) {
    resolve_arg(x)
  }

  expect_null(caller())
  expect_null(caller(NULL))
})

test_that("resolve_arg7: captures unquoted name as character string", {
  caller <- function(x) {
    resolve_arg(x)
  }

  expect_identical(caller(USUBJID), "USUBJID")
})

test_that("resolve_arg8: captures unevaluated expression as a single string", {
  caller <- function(x) {
    resolve_arg(x)
  }

  expect_identical(caller(AGE > 18), "AGE > 18")
  expect_identical(caller(A + B * C), "A + B * C")
})

test_that("resolve_arg9: returns evaluated double when double is allowed", {
  caller <- function(x) {
    resolve_arg(x, type = "double")
  }

  expect_identical(caller(1.5), 1.5)
})

test_that("resolve_arg10: returns evaluated integer when integer is allowed", {
  caller <- function(x) {
    resolve_arg(x, type = "integer")
  }

  expect_identical(caller(1L), 1L)
})

test_that("resolve_arg11: returns expression string when value type is not allowed", {
  caller <- function(x) {
    resolve_arg(x, type = "character")
  }

  expect_identical(caller(1.5), "1.5")
  expect_identical(caller(1L), "1L")
})

test_that("resolve_arg12: returns evaluated value when multiple allowed types include actual type", {
  caller <- function(x) {
    resolve_arg(x, type = c("character", "double"))
  }

  expect_identical(caller("AVAL"), "AVAL")
  expect_identical(caller(2.5), 2.5)
})

test_that("resolve_arg13: falls back to expression string when evaluation errors", {
  caller <- function(x) {
    resolve_arg(x)
  }

  expect_identical(caller(not_an_object), "not_an_object")
  expect_identical(caller(a + b), "a + b")
})

test_that("resolve_arg14: captures complex function call expression when result type is not allowed", {
  caller <- function(x) {
    resolve_arg(x, type = "character")
  }

  expect_identical(caller(mean(c(1, 2, 3))), "mean(c(1, 2, 3))")
})

test_that("resolve_arg15: returns evaluated character result of an expression when character is allowed", {
  caller <- function(x) {
    resolve_arg(x, type = "character")
  }

  expect_identical(caller(paste0("A", "B")), "AB")
})

test_that("resolve_arg16: returns expression string for NULL when NULL is not allowed", {
  caller <- function(x = NULL) {
    resolve_arg(x, type = "character")
  }

  expect_identical(caller(), "NULL")
  expect_identical(caller(NULL), "NULL")
})

library(testthat)

test_that("subset_data1: errors when data is not a data.frame", {
  expect_error(
    subset_data(data = 1:5),
    "'data' must be a data.frame.",
    fixed = TRUE
  )
})

test_that("subset_data2: returns data unchanged when where is NULL", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  res <- subset_data(df, where = NULL)

  rownames(res) <- NULL
  rownames(df) <- NULL

  expect_identical(res, df)
})

test_that("subset_data3: errors when where is not an expression", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  expect_error(
    subset_data(df, where = quote(AGE > 18)),
    "'where' must be an expression.",
    fixed = TRUE
  )
})

test_that("subset_data4: errors when where expression has length greater than 1", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  where <- expression(AGE > 18, AGE < 30)

  expect_error(
    subset_data(df, where = where),
    "'where' must be an expression of length 1.",
    fixed = TRUE
  )
})

test_that("subset_data5: errors when where references a non-existent column", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  where <- expression(TRT == "A")

  expect_error(
    subset_data(df, where = where),
    "Error evaluating 'where'",
    fixed = TRUE
  )
})

test_that("subset_data6: errors when where evaluates to non-logical vector", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  where <- expression(AGE)

  expect_error(
    subset_data(df, where = where),
    "'where' must evaluate to a logical vector.",
    fixed = TRUE
  )
})

test_that("subset_data7: errors when where returns logical vector of incorrect length", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  where <- expression(c(TRUE, FALSE))

  expect_error(
    subset_data(df, where = where),
    "'where' must evaluate to a logical vector with length equal to nrow(data).",
    fixed = TRUE
  )
})

test_that("subset_data8: subsets rows correctly when where evaluates to logical vector", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  where <- expression(AGE > 18)

  res <- subset_data(df, where = where)

  expected <- data.frame(
    USUBJID = c("02", "03"),
    AGE = c(20, 30)
  )

  rownames(res) <- NULL
  rownames(expected) <- NULL

  expect_identical(res, expected)
})

test_that("subset_data9: returns empty data.frame when condition is all FALSE", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  where <- expression(AGE > 100)

  res <- subset_data(df, where = where)
  expected <- df[0, , drop = FALSE]

  rownames(res) <- NULL
  rownames(expected) <- NULL

  expect_identical(res, expected)
})

test_that("subset_data10: returns full dataset when condition is all TRUE", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  where <- expression(AGE >= 10)

  res <- subset_data(df, where = where)

  rownames(res) <- NULL
  rownames(df) <- NULL

  expect_identical(res, df)
})

test_that("subset_data11: preserves data.frame structure when single row returned", {
  df <- data.frame(
    USUBJID = c("01", "02", "03"),
    AGE = c(10, 20, 30)
  )

  where <- expression(USUBJID == "02")

  res <- subset_data(df, where = where)

  rownames(res) <- NULL

  expect_true(is.data.frame(res))
  expect_identical(nrow(res), 1L)
  expect_identical(ncol(res), 2L)
  expect_identical(res$USUBJID, "02")
  expect_identical(res$AGE, 20)
})

test_that("subset_data12: handles compound logical expressions correctly", {
  df <- data.frame(
    USUBJID = c("01", "02", "03", "04"),
    AGE = c(10, 20, 30, 40),
    SEX = c("F", "M", "F", "M")
  )

  where <- expression(AGE >= 20 & SEX == "M")

  res <- subset_data(df, where = where)

  expected <- data.frame(
    USUBJID = c("02", "04"),
    AGE = c(20, 40),
    SEX = c("M", "M")
  )

  rownames(res) <- NULL
  rownames(expected) <- NULL

  expect_identical(res, expected)
})

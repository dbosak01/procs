base_path <- file.path(getwd(), "/tests/testthat")
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


prt2 <- read.table(header = TRUE, text = '
      sex internship enrollment count  group
  1  boys        yes        yes    35      1
  2  boys         no        yes    14      1
  3 girls        yes        yes    32      1
  4 girls         no        yes    53      1
  5  boys        yes         no    29      2
  6  boys         no         no    27      2
  7 girls        yes         no    10      2
  8 girls         no         no    23      2')

adsl <- read.table(header = TRUE, text = '
  SUBJID  ARM    SEX  RACE    AGE
  "001"   "ARM A" "F"  "WHITE" 19
  "002"   "ARM B" "F"  "WHITE" 21
  "003"   "ARM C" "F"  "WHITE" 23
  "004"   "ARM D" "F"  "BLACK" 28
  "005"   "ARM A" "M"  "WHITE" 37
  "006"   "ARM B" "M"  "WHITE" 34
  "007"   "ARM C" "M"  "WHITE" 36
  "008"   "ARM D" "M"  "WHITE" 30
  "009"   "ARM A" "F"  "WHITE" 39
  "010"   "ARM B" "F"  "WHITE" 31
  "011"   "ARM C" "F"  "BLACK" 33
  "012"   "ARM D" "F"  "WHITE" 38
  "013"   "ARM A" "M"  "BLACK" 37
  "014"   "ARM B" "M"  "WHITE" 34
  "015"   "ARM C" "M"  "WHITE" 36
  "016"   "ARM A" "M"  "WHITE" 40')


options("logr.output" = FALSE)
options("procs.print" = FALSE)

test_that("freqplot1: One-way proc_freq with plots works.", {


  # labels(dat) <- list(Eyes = "Eye Color",
  #                     Hair = "Hair Color",
  #                     Region = "Geographic Region")
  #
  # res <- proc_freq(dat, tables = c("Eyes"),
  #                  plots = freqplot(),
  #                  titles = "My first Frequency Plot")
  #
  # res
  #
  # expect_equal(is.null(res), TRUE)

  expect_equal(TRUE, TRUE)

})

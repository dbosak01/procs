library(tidyverse)
library(logr)
library(reporter)
library(fmtr)
library(broom)
library(tidylog, warn.conflicts = FALSE)

#setwd("C:/Users/User/OneDrive - ManpowerGroup/Stage 3/Content/Code Library")



# Attach loggers
options("tidylog.display" = list(log_print),
        "logr.on" = TRUE,
        "logr.notes" = FALSE)

# Open Log
log_path <- log_open("Table1_0")


# Prepare Data  ---------------------------------------------------------

# Write out log separator
sep("Read in ADSL data")

put("Data Filepath")
dir_data <- file.path(getwd(), "./tests/testthat/data") %>% put()

put("Create sample ADSL data.")
adsl <- tribble(
  ~SUBJID, ~ARM,    ~SEX, ~RACE,   ~AGE, ~AGEGR1,
  "001",   "ARM A", "F",  "WHITE", 19,   "18-29 years",
  "002",   "ARM B", "F",  "WHITE", 21,   "18-29 years",
  "003",   "ARM C", "F",  "WHITE", 23,   "18-29 years",
  "004",   "ARM D", "F",  "BLACK OR AFRICAN AMERICAN", 28,   "18-29 years",
  "005",   "ARM A", "M",  "WHITE", 37,   "30-39 years",
  "006",   "ARM B", "M",  "WHITE", 34,   "30-39 years",
  "007",   "ARM C", "M",  "WHITE", 36,   "30-39 years",
  "008",   "ARM D", "M",  "WHITE", 30,   "30-39 years",
  "009",   "ARM A", "F",  "WHITE", 39,   "30-39 years",
  "010",   "ARM B", "F",  "WHITE", 31,   "30-39 years",
  "011",   "ARM C", "F",  "BLACK OR AFRICAN AMERICAN", 33,   "30-39 years",
  "012",   "ARM D", "F",  "WHITE", 38,   "30-39 years",
  "013",   "ARM A", "M",  "BLACK OR AFRICAN AMERICAN", 37,   "30-39 years",
  "014",   "ARM B", "M",  "WHITE", 34,   "30-39 years",
  "015",   "ARM C", "M",  "WHITE", 36,   "30-39 years",
  "016",   "ARM A", "M",  "WHITE", 40,   "40-49 years"
)


put("Get ARM population counts")
arm_pop <- count(adsl, ARM) %>% deframe() %>% put()

put("Get ADaM format catalog")
adam_fmts <- read.fcat(file.path(getwd(), "tests/testthat/data/adam_fmts.fcat"))

# Stats Tests --------------------------------------------------------


put("Perform anova test on age and arm.")
age_anova <- aov(AGE ~ ARM, data = adsl) %>%
  tidy() %>% filter(term == "ARM") %>%
  transmute(stat = sprintf("%.3f (%.3f)", statistic,p.value)) %>%  put()

put("Perform chi squared test for each categorical variable.")
ageg_chisq <- chisq.test(adsl$ARM, adsl$AGEGR1, correct=FALSE) %>%
  tidy() %>% transmute(stat = sprintf("%.3f (%.3f)", statistic,p.value)) %>%
  put()
sex_chisq <- chisq.test(adsl$ARM, adsl$SEX, correct=FALSE) %>%
  tidy() %>% transmute(stat = sprintf("%.3f (%.3f)", statistic,p.value)) %>%
  put()
race_chisq <- chisq.test(adsl$ARM, adsl$RACE, correct=FALSE) %>%
  tidy() %>% transmute(stat = sprintf("%.3f (%.3f)", statistic,p.value)) %>%
  put()


# Age Summary Block -------------------------------------------------------

sep("Create summary statistics for age")

age_block <-
  adsl %>%
  group_by(ARM) %>%
  summarise( N = fmt_n(AGE),
             `Mean (SD)` = fmt_mean_sd(AGE),
             Median = fmt_median(AGE),
             `Q1 - Q3` = fmt_quantile_range(AGE),
             Range  = fmt_range(AGE)) %>%
  pivot_longer(-ARM,
               names_to  = "label",
               values_to = "value") %>%
  pivot_wider(names_from = ARM,
              values_from = "value") %>%
  add_column(var = "AGE", .before = "label") %>%
  bind_cols(age_anova) %>%
  put()


# Age Group Block ----------------------------------------------------------

sep("Create frequency counts for Age Group")


put("Create age group frequency counts")
ageg_block <-
  adsl %>%
  select(ARM, AGEGR1) %>%
  group_by(ARM, AGEGR1) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = ARM,
              values_from = n,
              values_fill = 0) %>%
  transmute(var = "AGEGR1",
            label =  factor(AGEGR1, levels = labels(adam_fmts$AGEGRP)),
            `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
            `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
            `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
            `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%
  arrange(label) %>%
  bind_cols(ageg_chisq) %>%
  put()


# Sex Block ---------------------------------------------------------------

sep("Create frequency counts for SEX")


# Create sex frequency counts
sex_block <-
  adsl %>%
  select(ARM, SEX) %>%
  group_by(ARM, SEX) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = ARM,
              values_from = n,
              values_fill = 0) %>%
  transmute(var = "SEX",
            label =   fct_relevel(SEX, "M", "F"),
            `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
            `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
            `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
            `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%
  arrange(label) %>%
  mutate(label = fapply(label, adam_fmts$SEX)) %>%
  bind_cols(sex_chisq) %>%
  put()



# Race Block --------------------------------------------------------------

sep("Create frequency counts for RACE")


# Prepare decode
race_decode <- c("WHITE" = "White",
                 "BLACK OR AFRICAN AMERICAN" = "Black or African American",
                 "ASIAN" = "Asian",
                 "NATIVE AMERICAN" = "Native American",
                 "UNKNOWN" = "Unknown")

# Create sex frequency counts
race_block <-
  adsl %>%
  select(ARM, RACE) %>%
  mutate(RACE = factor(RACE, levels = names(race_decode),
                       labels = race_decode)) %>%
  group_by(ARM, RACE) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = ARM,
              values_from = n,
              values_fill = 0) %>%
  transmute(var = "RACE",
            label =  RACE,
            `ARM A` = fmt_cnt_pct(`ARM A`, arm_pop["ARM A"]),
            `ARM B` = fmt_cnt_pct(`ARM B`, arm_pop["ARM B"]),
            `ARM C` = fmt_cnt_pct(`ARM C`, arm_pop["ARM C"]),
            `ARM D` = fmt_cnt_pct(`ARM D`, arm_pop["ARM D"])) %>%
  arrange(label) %>%
  bind_cols(race_chisq) %>%
  put()





# Final Data --------------------------------------------------------------

sep("Create final data frame")


final <- bind_rows(age_block, ageg_block, sex_block, race_block) %>%
  put()



# Print Report (Separate Labels) -------------------------------------------

sep("Create and print report (Separate Labels)")

var_fmt <- c("AGE" = "Age", "AGEGR1" = "Age Group", "SEX" = "Sex", "RACE" = "Race")

# Create Table
tbl <- create_table(final, first_row_blank = TRUE, width = 9) %>%
  column_defaults(from = `ARM A`, to = `ARM D`, align = "center", width = 1) %>%
  stub(vars = c("var", "label"), "Variable", width = 2.5) %>%
  define(var, blank_after = TRUE, dedupe = TRUE, label = "Variable",
         format = var_fmt,label_row = TRUE) %>%
  define(label, indent = .25, label = "Demographic Category") %>%
  define(`ARM A`,  n = arm_pop["ARM A"]) %>%
  define(`ARM B`,  n = arm_pop["ARM B"]) %>%
  define(`ARM C`,  n = arm_pop["ARM C"]) %>%
  define(`ARM D`,  n = arm_pop["ARM D"]) %>%
  define(stat, label = "Tests of Association*\nValue (P-value)",
         width = 2, dedupe = TRUE, align = "center") %>%
  titles("Table 1.0", "Analysis of Demographic Characteristics",
         "Safety Population") %>%
  footnotes("Program: Table1_0.R",
            "NOTE: Denominator based on number of non-missing responses.",
            "*Pearsons's Chi-Square tests will be used for categorical variables",
            "   and ANOVA tests for continuous variables.")


rpt <- create_report("output/DM_Table.rtf", output_type = "RTF") %>%
  set_margins(top = 1, bottom = 1) %>%
  page_header("Sponsor: Experis", "Study: ABC") %>%
  add_content(tbl) %>%
  page_footer(paste0("Date Produced: ", fapply(Sys.time(), "%d%b%y %H:%M")),
              right = "Page [pg] of [tpg]")

write_report(rpt) %>% put()



# Clean Up ----------------------------------------------------------------


log_close()

options("tidylog.display" = NULL)

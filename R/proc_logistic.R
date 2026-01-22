


proc_logistic <- function() {


  # /* Simple PROC LOGISTIC Example */
  #
  #   /* 1. Create Sample Data (e.g., predicting success in a test) */
  #   data test_data;
  # input success score1 score2;
  # datalines;
  # 1 85 90
  # 0 70 75
  # 1 95 92
  # 0 60 65
  # 1 88 80
  # ;
  # run;
  #
  # /* 2. Run PROC LOGISTIC */
  #   proc logistic data=test_data;
  # /* Model 'success' (the binary outcome) as a function of 'score1' and 'score2' */
  #   /* DESCENDING tells SAS to model the higher value (1=success) */
  #   model success (descending) = score1 score2;
  # run;



  test_data <- read.table(header = TRUE, text = '
   success score1 score2
   1       85     90
   0       70     75
   1       95     92
   0       60     65
   1       88     80
  ')


  res <- glm(success ~ score1 + score2, data = test_data, family = binomial(link = "logit"))


  res


  # Results do not match SAS.  CAMIS page says they should.  Not sure
  # what is going on.
}

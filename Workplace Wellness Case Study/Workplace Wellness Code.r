# Load required libraries
library(dplyr)
library(tidyr)

#turn off scientific notation
options(scipen=999)

#load into directory
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_dir <- dirname(sub("^--file=", "", file_arg))
  } else {
    script_dir <- file.path(getwd(), "data", "csv")
  }
}

#read csv
csv_path <- file.path(script_dir, "claims.csv")
claims_df <- read.csv(csv_path, stringsAsFactors = FALSE)

#select outcome variables for before start of treatment
before_2016_claims <- claims_df %>% 
  select(matches("0715")) %>%
  bind_cols(claims_df %>% select(treat))

#create vectors for outcomes
before_2016_control_outcomes <- c()
before_2016_treatment_outcomes <- c()
before_2016_p_values <- c()

#create vector of variables to run regressions with
outcome_vars <- names(before_2016_claims)[-7]

#iterate through variables to run regressions
for (i in outcome_vars) {
  formula_str <- paste(i, "~ treat")
  formula_obj <- as.formula(formula_str)
  lm <- summary(lm(formula_obj, data = before_2016_claims))
  #get important values from regression
  intercept = lm$coefficients["(Intercept)","Estimate"]
  beta1 <- lm$coefficients["treat","Estimate"]
  p_value <- lm$coefficients["treat","Pr(>|t|)"]
  #add values to output vectors
  before_2016_control_outcomes <- c(before_2016_control_outcomes, intercept)
  before_2016_treatment_outcomes <- c(before_2016_treatment_outcomes, intercept + beta1)
  before_2016_p_values <- c(before_2016_p_values, p_value)
}

#put results together in dataframe
before_2016_outcomes <- data.frame(
  variable = outcome_vars,
  control_mean = before_2016_control_outcomes,
  treatment_mean = before_2016_treatment_outcomes,
  p_value = before_2016_p_values
)

print(before_2016_outcomes)

#select outcome variables for 1 year after treatment
post_treatment_claims <- claims_df %>% 
  select(matches("0717")) %>%
  bind_cols(claims_df %>% select(c(male, age50, age37_49, white, treat)))

#create vector of variables to run regressions with
outcome_vars <- names(post_treatment_claims)[c(-17,-18,-19,-20,-21)]

#create vectors for outcomes
differences_no_demographic <- c()
std_err_no_demographic <- c()
differences_with_demographic <- c()
std_err_with_demographic <- c()

#iterate through variables to run regressions for no demographic controls
for (i in outcome_vars) {
  formula_str <- paste(i, "~ treat")
  formula_obj <- as.formula(formula_str)
  lm <- summary(lm(formula_obj, data = post_treatment_claims))
  #get important values from regression
  difference <- lm$coefficients["treat","Estimate"]
  std_err <- lm$coefficients["treat","Std. Error"]
  #add values to output vectors
  differences_no_demographic <- c(differences_no_demographic, difference)
  std_err_no_demographic <- c(std_err_no_demographic, std_err)
}

#iterate through variables to run regressions with demographic controls
for (i in outcome_vars) {
  formula_str <- paste(i, "~ treat + male + age50 + age37_49 + white")
  formula_obj <- as.formula(formula_str)
  lm <- summary(lm(formula_obj, data = post_treatment_claims))
  #get important values from regression
  difference <- lm$coefficients["treat","Estimate"]
  std_err <- lm$coefficients["treat","Std. Error"]
  #add values to output vectors
  differences_with_demographic <- c(differences_with_demographic, difference)
  std_err_with_demographic <- c(std_err_with_demographic, std_err)
}

#format as difference(standard error)
diff_se_no_demographic <- sprintf("%.4f (%.4f)", 
                                      as.numeric(differences_no_demographic), 
                                      std_err_no_demographic)
diff_se_with_demographic <- sprintf("%.4f (%.4f)", 
                                      as.numeric(differences_with_demographic), 
                                      std_err_with_demographic)

#put results together in dataframe
post_treatment_outcomes <- data.frame(
  variable = outcome_vars,
  diff_se_no_demographic = diff_se_no_demographic,
  diff_se_with_demographic = diff_se_with_demographic
)

print(post_treatment_outcomes)

#select outcome variables for 1 year after treatment for participant vs non-participant
post_treatment_claims <- claims_df %>% 
  select(matches("0717")) %>%
  bind_cols(claims_df %>% select(c(male, age50, age37_49, white, hra_c_yr1)))

#create vector of variables to run regressions with
outcome_vars <- names(post_treatment_claims)[c(-17,-18,-19,-20,-21)]

#create vectors for outcomes
differences_no_demographic <- c()
std_err_no_demographic <- c()
differences_with_demographic <- c()
std_err_with_demographic <- c()

#iterate through variables to run regressions for no demographic controls
for (i in outcome_vars) {
  formula_str <- paste(i, "~ hra_c_yr1")
  formula_obj <- as.formula(formula_str)
  lm <- summary(lm(formula_obj, data = post_treatment_claims))
  #get important values from regression
  difference <- lm$coefficients["hra_c_yr1","Estimate"]
  std_err <- lm$coefficients["hra_c_yr1","Std. Error"]
  #add values to output vectors
  differences_no_demographic <- c(differences_no_demographic, difference)
  std_err_no_demographic <- c(std_err_no_demographic, std_err)
}

#iterate through variables to run regressions with demographic controls
for (i in outcome_vars) {
  formula_str <- paste(i, "~ hra_c_yr1 + male + age50 + age37_49 + white")
  formula_obj <- as.formula(formula_str)
  lm <- summary(lm(formula_obj, data = post_treatment_claims))
  #get important values from regression
  difference <- lm$coefficients["hra_c_yr1","Estimate"]
  std_err <- lm$coefficients["hra_c_yr1","Std. Error"]
  #add values to output vectors
  differences_with_demographic <- c(differences_with_demographic, difference)
  std_err_with_demographic <- c(std_err_with_demographic, std_err)
}

#format as difference(standard error)
diff_se_no_demographic <- sprintf("%.4f (%.4f)", 
                                      as.numeric(differences_no_demographic), 
                                      std_err_no_demographic)
diff_se_with_demographic <- sprintf("%.4f (%.4f)", 
                                      as.numeric(differences_with_demographic), 
                                      std_err_with_demographic)

#put results together in dataframe
post_treatment_outcomes_p <- data.frame(
  variable = outcome_vars,
  diff_se_no_demographic = diff_se_no_demographic,
  diff_se_with_demographic = diff_se_with_demographic
)

print(post_treatment_outcomes_p)

print(sum(claims_df$completed_screening_nomiss_2016))
library(contdid)
library(dplyr)
library(readr)
library(tidyr)
library(knitr)

outcome_list <- c("gys_mth_g03", "gys_rla_g03", "gys_mth_g08", "gys_rla_g08")
control_vars <- c("enrollment_dist", "perc_black", "log_median_income", 
                  "poverty_rate", "unemployment_rate", "urban", "suburb", "town")

run_real_analysis <- function(df, outcome_var) {
  reg_df <- df %>%
    filter(year >= 2016, !year %in% c(2019, 2020)) %>%
    drop_na(all_of(c(outcome_var, control_vars, "dosage", "stateabb")))
  
  formula_str <- as.formula(paste(outcome_var, "~", paste(control_vars, collapse = " + "), "+ factor(stateabb)"))
  reg_df$adj_outcome <- resid(lm(formula_str, data = reg_df))
  
  analysis_df <- reg_df %>%
    mutate(time_step = case_when(
      year == 2016 ~ 1, year == 2017 ~ 2, year == 2018 ~ 3,
      year == 2021 ~ 4, year == 2022 ~ 5, year == 2023 ~ 6
    )) %>%
    mutate(G_step = ifelse(dosage > 0, 4, 0)) %>% 
    group_by(sedaadmin) %>%
    filter(n() == 6) %>% 
    ungroup()
  
  res <- cont_did(
    data = analysis_df, yname = "adj_outcome", tname = "time_step", idname = "sedaadmin",
    dname = "dosage", gname = "G_step", target_parameter = "level",
    aggregation = "dose", 
    treatment_type = "continuous", control_group = "nevertreated",
    biters = 100
  )
  
  message(paste("\n--- FULL SUMMARY: REAL MODEL (2021) -", outcome_var, "---"))
  print(summary(res))
  
  sum_obj <- summary(res)
  return(list(
    att = sum_obj$overall_att, 
    acrt = sum_obj$overall_acrt, 
    n = n_distinct(analysis_df$sedaadmin)
  ))
}

run_placebo_analysis <- function(df, outcome_var) {
  reg_df <- df %>%
    filter(year %in% c(2016, 2017, 2018)) %>%
    drop_na(all_of(c(outcome_var, control_vars, "dosage", "stateabb")))
  
  formula_str <- as.formula(paste(outcome_var, "~", paste(control_vars, collapse = " + "), "+ factor(stateabb)"))
  reg_df$adj_outcome <- resid(lm(formula_str, data = reg_df))
  
  analysis_df <- reg_df %>%
    mutate(time_step = case_when(year == 2016 ~ 1, year == 2017 ~ 2, year == 2018 ~ 3)) %>%
    mutate(G_step = ifelse(dosage > 0, 3, 0)) %>% 
    group_by(sedaadmin) %>%
    filter(n() == 3) %>% 
    ungroup()
  
  res <- cont_did(
    data = analysis_df, yname = "adj_outcome", tname = "time_step", idname = "sedaadmin",
    dname = "dosage", gname = "G_step", target_parameter = "level",
    aggregation = "dose", 
    treatment_type = "continuous", control_group = "nevertreated",
    biters = 100
  )
  
  # Print the full model summary to console
  message(paste("\n--- FULL SUMMARY: PLACEBO MODEL (2018) -", outcome_var, "---"))
  print(summary(res))
  
  sum_obj <- summary(res)
  return(list(
    att = sum_obj$overall_att, 
    acrt = sum_obj$overall_acrt, 
    n = n_distinct(analysis_df$sedaadmin)
  ))
}

# --- MAIN ---
main <- function() {
  did_df <- read_csv("./Data/clean/did_panel.csv")
  final_results <- list()
  
  for (out in outcome_list) {
    message(paste("\n**************************************************"))
    message(paste("STARTING ANALYSIS FOR:", out))
    message(paste("**************************************************"))
    
    real <- try(run_real_analysis(did_df, out))
    placebo <- try(run_placebo_analysis(did_df, out))
    
    if (!inherits(real, "try-error") && !inherits(placebo, "try-error")) {
      final_results[[out]] <- data.frame(
        Outcome = out,
        Real_ATT = real$att,
        Real_ACRT = real$acrt,
        Placebo_ATT = placebo$att,
        Placebo_ACRT = placebo$acrt,
        N_Real = real$n,
        N_Placebo = placebo$n
      )
    }
  }
  
  message("\n--- FINAL CONSOLIDATED TABLE ---")
  summary_table <- bind_rows(final_results)
  print(kable(summary_table, digits = 4))
  write_csv(summary_table, "./output/tables/final_validation_table.csv")
}

main()
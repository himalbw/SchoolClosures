library(contdid)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

run_contdid <- function(df, outcome_var) {
  analysis_df <- df %>%
    select(district_id, year, dosage, !!sym(outcome_var)) %>%
    drop_na() %>%
    mutate(time_step = case_when(
      year == 2016 ~ 1, year == 2017 ~ 2, year == 2018 ~ 3,
      year == 2021 ~ 4, year == 2022 ~ 5
    )) %>%
    filter(!is.na(time_step)) %>%
    mutate(G_step = ifelse(dosage > 0, 4, 0)) %>%
    group_by(district_id) %>%
    mutate(dosage = first(dosage)) %>%
    filter(n() == 5) %>% # BALANCED PANEL MAY NEED TO REVISE!!!
    ungroup()
  
  if(nrow(analysis_df) < 50) {
    warning(paste("Insufficient data for", outcome_var))
    return(NULL)
  }
  
  res_level <- cont_did(
    data = analysis_df,
    yname = outcome_var,
    tname = "time_step",
    idname = "district_id",
    dname = "dosage",
    gname = "G_step",
    target_parameter = "level",
    aggregation = "dose",
    treatment_type = "continuous",
    control_group = "nevertreated",
    biters = 100, # Use 1000 for final paper-quality SEs
    cband = TRUE
  )
  
  p_att <- ggcont_did(res_level, type = "att") + labs(title = paste("Dose-Response (ATT):", outcome_var), 
         subtitle = "Difference-in-Differences with Continuous Treatment",
         x = "Dosage (Closure Intensity)", y = "Effect on Score") + theme_minimal()
  
  ggsave(filename = paste0("./output/figures/att_", outcome_var, ".png"), 
         plot = p_att, width = 8, height = 6)
  
  sink(paste0("./output/tables/summary_", outcome_var, ".txt"))
  cat(paste("Analysis for Outcome:", outcome_var, "\n"))
  cat("=========================================\n")
  print(summary(res_level))
  sink()
  
  res_df <- data.frame(
    dose = res_level$d_grid,
    att = res_level$att_dose,
    se = res_level$se_dose
  )
  write_csv(res_df, paste0("./output/tables/estimates_", outcome_var, ".csv"))
  
  return(res_level)
}

main <- function() {
  outcome_df <- read_csv("./Data/clean/admindist_outcomes_and_controls.csv")
  school_df <- read_csv("./Data/clean/panel_closures.csv")
  merged_df <- outcome_df %>% left_join(school_df, by = c("district_id", "year"))
  
  # 3. Define the full list of variables from your output
  outcome_list <- c(
    "ach_math_g8_gys", "ach_rla_g8_gys", "ach_math_g3_gys", 
    "ach_math_g4_gys", "ach_math_g5_gys", "ach_math_g6_gys", 
    "ach_math_g7_gys", "ach_rla_g3_gys", "ach_rla_g4_gys", 
    "ach_rla_g5_gys", "ach_rla_g6_gys", "ach_rla_g7_gys",
    "ach_math_g8_gys_z", "ach_rla_g8_gys_z", "ach_math_g3_gys_z", 
    "ach_math_g4_gys_z", "ach_math_g5_gys_z", "ach_math_g6_gys_z", 
    "ach_math_g7_gys_z", "ach_rla_g3_gys_z", "ach_rla_g4_gys_z", 
    "ach_rla_g5_gys_z", "ach_rla_g6_gys_z", "ach_rla_g7_gys_z"
  )
  
  results_storage <- list()
  for (out in outcome_list) {
    message(paste("Running Callaway-Sant-Anna ContDid (2021) for:", out, "---"))
    
    model_result <- try(run_contdid(merged_df, out))
    
    if (!inherits(model_result, "try-error")) {
      results_storage[[out]] <- model_result
    }
  }
  
  message("All done!!")
  return(results_storage)
}

# results <- main() 
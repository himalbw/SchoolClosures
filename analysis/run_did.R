library(contdid)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

library(contdid)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(patchwork)

main <- function() {
  did_df <- read_csv("./Data/clean/did_panel.csv")
  
  # Target grades for comparison
  target_grades <- c("03", "05", "08")
  
  for (grade in target_grades) {
    message(paste("\n--- Processing Grade", grade, "Combined Panels ---"))
    
    # Define outcome names
    mth_outcome <- paste0("gys_mth_g", grade)
    rla_outcome <- paste0("gys_rla_g", grade)
    
    # Run the FE Analysis for both subjects
    res_mth <- try(run_contdid_controlled_FE(did_df, mth_outcome, "Math"))
    res_rla <- try(run_contdid_controlled_FE(did_df, rla_outcome, "Reading"))
    
    # Check if both models succeeded
    if (!inherits(res_mth, "try-error") && !inherits(res_rla, "try-error") && 
        !is.null(res_mth) && !is.null(res_rla)) {
      
      # Extract the ATT plots from the returned objects
      # (Note: We modify the internal function to return the plot object too)
      p_combined <- res_mth$plot + res_rla$plot + 
        plot_annotation(
          title = paste("Continuous DiD: Grade", as.numeric(grade), "Achievement Impact"),
          subtitle = "Specification: State FE + Controls | Dose-Response Function (ATT)",
          theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                        plot.subtitle = element_text(hjust = 0.5))
        )
      
      # Save the side-by-side panel
      out_path <- paste0("./output/figures/Combined_Grade_", grade, "_ATT.png")
      ggsave(filename = out_path, plot = p_combined, width = 14, height = 6, bg = "white")
      message(paste("SUCCESS: Saved combined plot to", out_path))
    }
  }
}

run_contdid_controlled_FE <- function(df, outcome_var, label) {
  control_vars <- c("enrollment_dist", "perc_black", "log_median_income", 
                    "poverty_rate", "unemployment_rate", "urban", "suburb", "town")
  
  reg_df <- df %>%
    filter(year >= 2016, !year %in% c(2019, 2020)) %>%
    drop_na(all_of(c(outcome_var, control_vars, "dosage", "stateabb")))
  
  formula_str <- as.formula(paste(outcome_var, "~", 
                                  paste(control_vars, collapse = " + "), 
                                  "+ factor(stateabb)"))
  
  # Residualize
  resid_model <- lm(formula_str, data = reg_df)
  reg_df$adj_outcome <- resid(resid_model)
  
  # Prepare for ContDid
  analysis_df <- reg_df %>%
    mutate(time_step = case_when(
      year == 2016 ~ 1, year == 2017 ~ 2, year == 2018 ~ 3,
      year == 2021 ~ 4, year == 2022 ~ 5, year == 2023 ~ 6
    )) %>%
    mutate(G_step = ifelse(dosage > 0, 4, 0)) %>%
    group_by(sedaadmin) %>%
    mutate(dosage = first(dosage)) %>%
    filter(n() == 6) %>% 
    ungroup()
  
  n_count <- n_distinct(analysis_df$sedaadmin)
  if(n_count < 50) return(NULL)
  
  # Run the model
  res <- cont_did(
    data = analysis_df, yname = "adj_outcome", tname = "time_step", 
    idname = "sedaadmin", dname = "dosage", gname = "G_step",
    target_parameter = "level", aggregation = "dose", 
    treatment_type = "continuous", control_group = "nevertreated",
    biters = 100, cband = TRUE
  )
  
  # Generate ATT Plot
  p <- ggcont_did(res, type = "att") + 
    labs(title = label, x = "Dosage (Closure Intensity)", y = "Effect (Residualized)") +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    ylim(-0.6, 0.4) # Keeping scales fixed helps cross-subject comparison
  
  # Return both the result and the plot
  return(list(result = res, plot = p))
}

main()

# results <- main() 
# depending on variable between 3000 and 5000 districts incldued
run_contdid <- function(df, outcome_var) {
  analysis_df <- df %>%
    select(sedaadmin, year, dosage, !!sym(outcome_var)) %>%
    drop_na() %>%
    mutate(time_step = case_when(
      year == 2016 ~ 1,
      year == 2017 ~ 2,
      year == 2018 ~ 3,
      year == 2021 ~ 4, # Jump to post-treatment
      year == 2022 ~ 5,
      year == 2023 ~ 6
    )) %>%
    filter(!is.na(time_step)) %>%
    # Treatment starts in 2021, which is now step 4
    mutate(G_step = ifelse(dosage > 0, 4, 0)) %>%
    group_by(sedaadmin) %>%
    mutate(dosage = first(dosage)) %>%
    # Must have all 6 years: 2016, 17, 18, 21, 22, 23
    filter(n() == 6) %>% 
    ungroup()
  
  n_count <- n_distinct(analysis_df$sedaadmin)
  if(n_count < 50) {
    message(paste("Skipping", outcome_var, "- only", n_count, "districts found."))
    return(NULL)
  }
  
  message(paste("Running", outcome_var, "with", n_count, "districts..."))
  
  res_level <- cont_did(
    data = analysis_df,
    yname = outcome_var,
    tname = "time_step",
    idname = "sedaadmin",
    dname = "dosage",
    gname = "G_step",
    target_parameter = "level",
    aggregation = "dose",
    treatment_type = "continuous",
    control_group = "nevertreated",
    biters = 100, 
    cband = TRUE
  )
  
  p_att <- ggcont_did(res_level, type = "att") + 
    labs(title = paste("Dose-Response (ATT):", outcome_var), 
         subtitle = paste("Sample Size:", n_count, "districts (Balanced 2016-2023)"),
         x = "Dosage (Closure Intensity)", y = "Effect on Score") +
    theme_minimal()
  
  ggsave(filename = paste0("./output/figures/att_", outcome_var, ".png"), 
         plot = p_att, width = 8, height = 6)
  
  sink(paste0("./output/tables/summary_", outcome_var, ".txt"))
  cat(paste("Analysis for Outcome:", outcome_var, "\n"))
  cat(paste("Balanced districts N =", n_count, "\n"))
  cat("=========================================\n")
  print(summary(res_level))
  sink()
  
  # Export raw estimates
  res_df <- data.frame(
    dose = res_level$d_grid,
    att = res_level$att_dose,
    se = res_level$se_dose
  )
  write_csv(res_df, paste0("./output/tables/estimates_", outcome_var, ".csv"))
  
  return(res_level)
}
run_contdid_residualized <- function(df, outcome_var) {
  controls <- c("enrollment_dist", "perc_black", "log_median_income", 
                "poverty_rate", "unemployment_rate", "urban", "town", "suburb")
  formula_str <- as.formula(paste(outcome_var, "~", paste(controls, collapse = " + ")))
  reg_df <- df %>% drop_na(all_of(c(outcome_var, controls)))
  resid_model <- lm(formula_str, data = reg_df)
  reg_df$adj_outcome <- resid(resid_model)
  
  analysis_df <- reg_df %>%
    select(sedaadmin, year, dosage, adj_outcome) %>%
    mutate(time_step = case_when(
      year == 2016 ~ 1, year == 2017 ~ 2, year == 2018 ~ 3,
      year == 2021 ~ 4, year == 2022 ~ 5, year == 2023 ~ 6
    )) %>%
    filter(!is.na(time_step)) %>%
    mutate(G_step = ifelse(dosage > 0, 4, 0)) %>%
    group_by(sedaadmin) %>%
    mutate(dosage = first(dosage)) %>%
    filter(n() == 6) %>% 
    ungroup()
  
  res_dose <- cont_did(
    data = analysis_df,
    yname = "adj_outcome", # This is your 'controlled' outcome
    tname = "time_step",
    idname = "sedaadmin",
    dname = "dosage",
    gname = "G_step",
    target_parameter = "level",
    aggregation = "dose",
    treatment_type = "continuous",
    control_group = "nevertreated",
    biters = 100,
    cband = TRUE
  )
  
  p_att <- ggcont_did(res_dose, type = "att") + 
    labs(title = paste("Controlled ATT:", outcome_var),
         subtitle = "Residualized for demographics, income, and urbanicity")
  
  ggsave(filename = paste0("./output/figures/controlled_att_", outcome_var, ".png"), plot = p_att)
  
  return(res_dose)
}
run_contdid_controlled_FE <- function(df, outcome_var) {
  control_vars <- c("enrollment_dist", "perc_black", "log_median_income", 
                    "poverty_rate", "unemployment_rate", "urban", "suburb", "town")
  reg_df <- df %>%
    filter(year >= 2016, !year %in% c(2019, 2020)) %>%
    drop_na(all_of(c(outcome_var, control_vars, "dosage", "stateabb")))
  formula_str <- as.formula(paste(outcome_var, "~", 
                                  paste(control_vars, collapse = " + "), 
                                  "+ factor(stateabb)"))
  
  message(paste("Residualizing", outcome_var, "with State FE and Controls..."))
  resid_model <- lm(formula_str, data = reg_df)
  reg_df$adj_outcome <- resid(resid_model)
  
  analysis_df <- reg_df %>%
    mutate(time_step = case_when(
      year == 2016 ~ 1, year == 2017 ~ 2, year == 2018 ~ 3,
      year == 2021 ~ 4, year == 2022 ~ 5, year == 2023 ~ 6
    )) %>%
    mutate(G_step = ifelse(dosage > 0, 4, 0)) %>%
    group_by(sedaadmin) %>%
    mutate(dosage = first(dosage)) %>%
    filter(n() == 6) %>% 
    ungroup()
  
  n_count <- n_distinct(analysis_df$sedaadmin)
  if(n_count < 50) {
    print("Insufficient Observations")
    return(NULL)
  }
  res <- cont_did(
    data = analysis_df,
    yname = "adj_outcome",
    tname = "time_step",
    idname = "sedaadmin",
    dname = "dosage",
    gname = "G_step",
    target_parameter = "level",
    aggregation = "dose",
    treatment_type = "continuous",
    control_group = "nevertreated",
    biters = 100,
    cband = TRUE
  )
  
  p <- ggcont_did(res, type = "att") + 
    labs(title = paste("State FE + Controlled ATT:", outcome_var),
         subtitle = paste("Residualized on State FE, Demographics, and Income | N =", n_count)) +
    theme_minimal()
  
  ggsave(filename = paste0("./output/figures/stateFE_att_", outcome_var, ".png"), plot = p)
  
  # Change type = "att" to type = "acrt"
  p_acrt <- ggcont_did(res, type = "acrt") + 
    labs(title = paste("ACRT (Marginal Effect):", outcome_var),
         subtitle = "Shows how the effect changes per unit of dosage",
         x = "Dosage (Closure Intensity)", 
         y = "Marginal Effect on Score") +
    theme_minimal()
  
  ggsave(filename = paste0("./output/figures/acrt_", outcome_var, ".png"), 
         plot = p_acrt, width = 8, height = 6)
  
  return(res)
}
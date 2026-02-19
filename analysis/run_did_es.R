library(tidyverse)
library(did)

main <- function() {
  did_df <- read_csv("./Data/clean/did_panel.csv")
  did_df$state_id <- as.numeric(as.factor(did_df$stateabb))
  math_vars <- c("gys_mth_g03", "gys_mth_g04", "gys_mth_g05", "gys_mth_g06", "gys_mth_g07", "gys_mth_g08")
  grade_labels <- c("3rd Grade Math", "4th Grade Math", "5th Grade Math", "6th Grade Math", "7th Grade Math", "8th Grade Math")
  control_vars <- c("enrollment_dist", "perc_black", "log_median_income", "poverty_rate", "unemployment_rate", "urban", "suburb", "town")
  
  run_did_suite(did_df, math_vars, grade_labels, "baseline", NULL)
  run_did_suite(did_df, math_vars, grade_labels, "controls", control_vars)
  run_did_suite(did_df, math_vars, grade_labels, "controls+FE", control_vars)
}

run_did_suite <- function(data, vars, labels, type, controls) {
  out_dir <- paste0("./output/figures/es_results/", type, "/")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  target_years <- c(2016, 2017, 2018, 2021, 2022, 2023)
  
  for (i in seq_along(vars)) {
    outcome <- vars[i]
    
    needed_cols <- c("sedaadmin", "year", "dosage", outcome)
    if (!is.null(controls)) needed_cols <- c(needed_cols, controls)
    if (type == "controls+FE") needed_cols <- c(needed_cols, "state_id")
    
    loop_df <- data %>%
      filter(year %in% target_years) %>%
      select(all_of(needed_cols)) %>%
      drop_na() %>%
      mutate(gname = ifelse(dosage > 0.25, 2021, 0)) %>%
      group_by(sedaadmin) %>%
      filter(n() == length(target_years)) %>%
      ungroup() %>%
      as.data.frame() # Crucial for formula evaluation
    
    if (type == "baseline") {
      f_str <- "~1"
    } else if (type == "controls") {
      f_str <- paste("~", paste(controls, collapse = " + "))
    } else {
      f_str <- paste("~", paste(controls, collapse = " + "), " + state_id")
    }
    
    res <- att_gt(
      yname = outcome, tname = "year", idname = "sedaadmin", gname = "gname",
      xformla = as.formula(f_str), data = loop_df,
      panel = TRUE, control_group = "nevertreated", est_method = "dr"
    )
    
    p <- ggdid(res) + 
      labs(title = paste(labels[i], "-", type), subtitle = "Treatment: Dosage > 0.25", y = "Effect (SD)", x = "Year") +
      theme_minimal()
    
    ggsave(paste0(out_dir, outcome, "_", type, ".png"), plot = p, width = 9, height = 6, bg = "white")
    message(paste("Completed:", labels[i], "| Type:", type))
  }
}

main()
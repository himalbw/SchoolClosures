main <- function () {
  did_panel <- read_csv("./Data/clean/did_panel.csv")
  balanced_districts <- did_df %>%
    filter(year >= 2016, !year %in% c(2019, 2020)) %>%
    drop_na(gys_mth_g08, dosage) %>%
    group_by(sedaadmin) %>%
    filter(n() == 6) %>% 
    pull(sedaadmin) %>%
    unique()
  
  balanced_df <- did_df %>% filter(sedaadmin %in% balanced_districts)
  state_diag(balanced_df)
  
  balanced_df <- balanced_df %>% mutate(dosage_tercile = ntile(dosage, 3))
  balanced_df <- balanced_df %>% drop_na(dosage)
  tercile_stats <- summarize_by_tercile(balanced_df)
  
  tasks <- list(
    # Grade 3
    c("gys_mth_g03", "Math", "Grade 3"),
    c("gys_rla_g03", "Reading", "Grade 3"),
    
    # Grade 5
    c("gys_mth_g05", "Math", "Grade 5"),
    c("gys_rla_g05", "Reading", "Grade 5"),
    
    # Grade 8
    c("gys_mth_g08", "Math", "Grade 8"),
    c("gys_rla_g08", "Reading", "Grade 8")
  )
  
  all_tercile_tables <- list()
  
  for (task in tasks) {
    col_name <- task[1]
    if (col_name %in% names(balanced_df)) {
      message(paste("Generating trends for:", task[3], task[2]))
      all_tercile_tables[[col_name]] <- summarize_and_plot(
        df = balanced_df, 
        score_var = col_name, 
        subject_label = task[2], 
        grade_label = task[3]
      )
    } else {
      warning(paste("Column", col_name, "not found in the data. Skipping..."))
    }
  }

  }

summarize_and_plot <- function(df, score_var, subject_label = "Math", grade_label = "Grade 8") {
  out_df <- df %>%
    group_by(dosage_tercile, year) %>%
    summarise(
      n = n_distinct(sedaadmin),
      avg_dosage = mean(dosage, na.rm = TRUE),
      # Use !!sym() to evaluate the string as a column name
      mean_score = mean(!!sym(score_var), na.rm = TRUE),
      poverty = mean(poverty_rate, na.rm = TRUE),
      black_pct = mean(perc_black, na.rm = TRUE),
      enrollment = mean(enrollment_dist, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(label = case_when(
      dosage_tercile == 1 ~ "Low",
      dosage_tercile == 2 ~ "Mid",
      dosage_tercile == 3 ~ "High"
    ))
  
  p <- ggplot(out_df, aes(x = year, y = mean_score, 
                          color = factor(dosage_tercile), 
                          group = dosage_tercile)) +
    annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf, 
             alpha = 0.1, fill = "gray50") +
    annotate("text", x = 2019.5, y = max(out_df$mean_score, na.rm = TRUE), 
             label = "Pandemic", angle = 90, vjust = 1.5, size = 3.5, color = "gray40") +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(
      values = c("1" = "#2c7bb6", "2" = "#fdae61", "3" = "#d7191c"),
      labels = c("1" = "Low Dosage", "2" = "Mid Dosage", "3" = "High Dosage")
    ) +
    labs(
      title = paste("Raw Trends in", grade_label, subject_label, "Scores"),
      subtitle = paste("Comparing School Closure Intensity (n =", format(max(out_df$n), big.mark=","), "districts)"),
      x = "Year", 
      y = paste("Average", subject_label, "Score (GYS)"),
      color = "Treatment Group"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  file_name <- paste0("./output/figures/raw_trends_", score_var, ".png")
  ggsave(file_name, plot = p, width = 8, height = 6, dpi = 300)
  print(p)
  
  return(out_df)
}

state_diag <- function(balanced_df) {
state_diagnostics <- balanced_df %>%
  group_by(stateabb) %>%
  summarise(
    n_districts = n_distinct(sedaadmin),
    avg_dosage = mean(dosage, na.rm = TRUE),
    sd_dosage = sd(dosage, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_dosage))

print(state_diagnostics)
write.csv(state_diagnostics, "./output/tables/descriptives/state_avg_and_sd_dosage.csv")
}

tercile_stats <- function(did_df) {
  balanced_ids <- did_df %>%
    filter(year %in% c(2016, 2017, 2018, 2021, 2022, 2023)) %>%
    drop_na(gys_mth_g08, dosage) %>%
    group_by(sedaadmin) %>%
    filter(n() == 6) %>%
    pull(sedaadmin) %>%
    unique()
  
  tercile_balance_pre <- did_df %>%
    filter(sedaadmin %in% balanced_ids) %>%
    filter(year %in% c(2016, 2017, 2018)) %>%
    group_by(sedaadmin) %>%
    summarise(
      dosage = first(dosage),
      avg_poverty = mean(poverty_rate, na.rm = TRUE),
      avg_black = mean(perc_black, na.rm = TRUE),
      avg_income = mean(log_median_income, na.rm = TRUE),
      avg_enroll = mean(enrollment_dist, na.rm = TRUE),
      pre_math_g8 = mean(gys_mth_g03, na.rm = TRUE),
      pre_rla_g8 = mean(gys_rla_g03, na.rm = TRUE)
    ) %>%
    filter(!is.na(dosage)) %>% 
    
    mutate(dosage_tercile = ntile(dosage, 3)) %>%
    group_by(dosage_tercile) %>%
    summarise(
      n_districts = n(),
      mean_dosage = mean(dosage, na.rm = TRUE),
      poverty = mean(avg_poverty, na.rm = TRUE),
      black_pct = mean(avg_black, na.rm = TRUE),
      log_income = mean(avg_income, na.rm = TRUE),
      enrollment = mean(avg_enroll, na.rm = TRUE),
      math_score_pre = mean(pre_math_g8, na.rm = TRUE),
      rla_score_pre = mean(pre_rla_g8, na.rm = TRUE)
    )
  
  print("--- Final NA-Robust Pre-Treatment Balance Table ---")
  print(tercile_balance_pre)
  
  write.csv(tercile_balance_pre, "./output/tables/descriptives/tercile_balance_table.csv")
}
library(tidyverse)

main <- function () {
  # Note: Ensuring did_df is defined correctly from the start
  did_df <- read_csv("./Data/clean/did_panel.csv")
  
  balanced_districts <- did_df %>%
    filter(year >= 2016, !year %in% c(2019, 2020)) %>%
    drop_na(gys_mth_g08, dosage) %>%
    group_by(sedaadmin) %>%
    filter(n() == 6) %>% 
    pull(sedaadmin) %>%
    unique()
  
  balanced_df <- did_df %>% filter(sedaadmin %in% balanced_districts)
  
  # Run state diagnostics
  state_diag(balanced_df)
  
  tercile_breaks <- balanced_df %>%
    filter(dosage > 0) %>%
    group_by(sedaadmin) %>%
    summarise(dosage = first(dosage)) %>%
    mutate(ntile_val = ntile(dosage, 3)) %>%
    select(sedaadmin, ntile_val)
  
  # NEW LOGIC: Separate Dosage == 0 from the Terciles
  balanced_df <- balanced_df %>%
    filter(year >= 2016) %>%
    left_join(tercile_breaks, by = "sedaadmin") %>%
    mutate(dosage_group = case_when(
      dosage == 0 ~ 0,
      !is.na(ntile_val) ~ as.numeric(ntile_val)
    )) %>%
    filter(!is.na(dosage_group)) # Drops rows that didn't fit (if any)
  
  tercile_breaks <- balanced_df %>%
    filter(dosage > 0) %>%
    group_by(sedaadmin) %>%
    summarise(dosage = first(dosage)) %>%
    mutate(ntile_val = ntile(dosage, 3)) %>%
    select(sedaadmin, ntile_val)
  
  # Update tercile stats to use the new group logic
  tercile_stats(balanced_df)
  
  tasks <- list(
    c("gys_mth_g03", "Math", "Grade 3"),
    c("gys_rla_g03", "Reading", "Grade 3"),
    c("gys_mth_g05", "Math", "Grade 5"),
    c("gys_rla_g05", "Reading", "Grade 5"),
    c("gys_mth_g08", "Math", "Grade 8"),
    c("gys_rla_g08", "Reading", "Grade 8")
  )
  
  all_plots <- list()
  for (task in tasks) {
    col_name <- task[1]
    if (col_name %in% names(balanced_df)) {
      message(paste("Generating trends for:", task[3], task[2]))
      all_plots[[col_name]] <- summarize_and_plot(
        df = balanced_df, 
        score_var = col_name, 
        subject_label = task[2], 
        grade_label = task[3]
      )
    }
  }
}

summarize_and_plot <- function(df, score_var, subject_label, grade_label) {
  # Aggregate data by group and year
  out_df <- df %>%
    group_by(dosage_group, year) %>%
    summarise(
      mean_score = mean(!!sym(score_var), na.rm = TRUE),
      .groups = "drop"
    ) 
  
  p <- ggplot(out_df, aes(x = year, y = mean_score, 
                          color = factor(dosage_group), 
                          group = dosage_group)) +
    # Pandemic shaded area
    annotate("rect", xmin = 2018.5, xmax = 2020.5, ymin = -Inf, ymax = Inf, 
             alpha = 0.1, fill = "gray50") +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    # Custom colors and the specific requested labels
    scale_color_manual(
      values = c("0" = "#555555", "1" = "#2c7bb6", "2" = "#fdae61", "3" = "#d7191c"),
      labels = c("0" = "Fully In-Person", 
                 "1" = "Low Remote", 
                 "2" = "Mid Remote", 
                 "3" = "High Remote")
    ) +
    labs(
      title = paste("Trends in", subject_label, "Grade-Levels 2016-2023"),
      x = "Year", 
      y = paste("Average", subject_label, "Score (Grade-Level)"),
      color = "Treatment Group"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14)
    )
  
  # Save and return
  file_name <- paste0("./output/figures/raw_trends_", score_var, ".png")
  ggsave(file_name, plot = p, width = 8, height = 6, dpi = 300)
  
  return(p)
}
# Adjusted tercile_stats to handle the 4-group logic
tercile_stats <- function(balanced_df) {
  summary_tab <- balanced_df %>%
    filter(year %in% c(2016, 2017, 2018)) %>%
    group_by(dosage_group) %>%
    summarise(
      n_districts = n_distinct(sedaadmin),
      mean_dosage = mean(dosage, na.rm = TRUE),
      poverty = mean(poverty_rate, na.rm = TRUE),
      black_pct = mean(perc_black, na.rm = TRUE),
      math_score_pre = mean(gys_mth_g03, na.rm = TRUE)
    )
  
  print(summary_tab)
  write.csv(summary_tab, "./output/tables/descriptives/tercile_balance_table.csv")
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


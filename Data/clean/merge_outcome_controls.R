# set working directory to repo root 

outcomes_df <- read_csv("./Data/clean/admindist_outcomes.csv")
controls_df <- read_csv("./Data/clean/admin_district_controls.csv")
outcomes_df <- read_csv("./Data/clean/admindist_outcomes.csv", show_col_types = FALSE) %>%
  select(-`...1`) %>% mutate(
    sedaadmin = as.numeric(sedaadmin),
    year = as.integer(year)
  )

controls_df <- controls_df %>% mutate(sedaadmin = as.numeric(sedaadmin))

control_vars_to_z <- c(
  "enrollment_dist",
  "perc_black",
  "log_median_income",
  "poverty_rate",
  "unemployment_rate"
)

controls_std <- controls_df %>%
  mutate(across(all_of(control_vars_to_z), ~ as.numeric(scale(.)), .names = "z_{.col}"))

# If you ALSO want to standardize outcomes:
outcome_vars_to_z <- outcomes_df %>%
  select(starts_with("gys_")) %>%
  names()

outcomes_std <- outcomes_df %>% mutate(across(all_of(outcome_vars_to_z), ~ as.numeric(scale(.)), .names = "z_{.col}"))

panel_df <- outcomes_std %>% left_join(controls_std, by = "sedaadmin")

panel_df %>% summarise(share_missing_controls = mean(is.na(enrollment_dist)))  # or any control
write.csv(panel)

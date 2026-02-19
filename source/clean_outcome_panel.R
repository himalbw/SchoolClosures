# set working directory to repo root 
controls_df <- read_csv("./Data/clean/admindist_controls.csv") %>% mutate(sedaadmin = as.numeric(sedaadmin))
outcomes_df <- read_csv("./Data/clean/admindist_outcomes.csv", show_col_types = FALSE) %>% select(-`...1`) %>% mutate(sedaadmin = as.numeric(sedaadmin), year = as.integer(year))
panel_df <- outcomes_df %>% left_join(controls_df, by = "sedaadmin")
closure_df <- read_csv("./Data/clean/panel_closures.csv") %>% rename(sedaadmin = district_id) %>% mutate(sedaadmin = as.numeric(sedaadmin))

panel_joined <- panel_df %>% left_join(closure_df, by = c("sedaadmin", "year")) #93% match rate
write.csv(panel_joined, "./Data/clean/did_panel.csv")


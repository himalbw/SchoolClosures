library(tidyverse)
library(did)
library(patchwork)

main <- function() {
  did_df <- read_csv("./Data/clean/did_panel.csv")
  did_df$state_id <- as.numeric(as.factor(did_df$stateabb))
  
  target_grades <- c("03", "05", "08")
  control_vars <- c("enrollment_dist", "perc_black", "log_median_income", 
                    "poverty_rate", "unemployment_rate", "urban", "suburb", "town")
  
  suites <- list(
    list(name = "baseline", controls = NULL, use_fe = FALSE),
    list(name = "controls", controls = control_vars, use_fe = FALSE),
    list(name = "controls+FE", controls = control_vars, use_fe = TRUE)
  )
  
  for (suite in suites) {
    message(paste("--- Running Suite:", suite$name, "---"))
    run_combined_did_suite(did_df, target_grades, suite$name, suite$controls, suite$use_fe)
  }
}

run_combined_did_suite <- function(data, grades, type_name, controls, use_fe) {
  out_dir <- paste0("./output/figures/es_results/", type_name, "/")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  target_years <- c(2016, 2017, 2018, 2021, 2022, 2023)
  
  for (grade in grades) {
    plots <- list()
    
    for (subject in c("mth", "rla")) {
      outcome <- paste0("gys_", subject, "_g", grade)
      subj_label <- ifelse(subject == "mth", "Math", "Reading/RLA")
      
      if (is.null(controls)) {
        f_str <- "~1"
      } else if (!use_fe) {
        f_str <- paste("~", paste(controls, collapse = " + "))
      } else {
        f_str <- paste("~", paste(controls, collapse = " + "), " + state_id")
      }
      
      loop_df <- data %>%
        filter(year %in% target_years) %>%
        mutate(gname = ifelse(dosage > 0.25, 2021, 0)) %>%
        select(sedaadmin, year, gname, !!sym(outcome), any_of(controls), any_of("state_id")) %>%
        drop_na() %>%
        group_by(sedaadmin) %>%
        filter(n() == length(target_years)) %>%
        ungroup()
      
      res <- att_gt(
        yname = outcome, tname = "year", idname = "sedaadmin", gname = "gname",
        xformla = as.formula(f_str),
        data = as.data.frame(loop_df),
        panel = TRUE, control_group = "nevertreated", est_method = "dr"
      )
      
      # Modification: Set facet_titles = FALSE to remove "Group 2021"
      p <- ggdid(res, facet_titles = FALSE) + 
        labs(title = subj_label, y = "Effect (Grade Levels)", x = "Year") +
        theme_minimal(base_size = 14) + 
        theme(
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          legend.position = "none",
          strip.text = element_blank(), # Extra insurance to hide facet headers
          panel.grid.minor = element_blank()
        )
      
      plots[[subject]] <- p
    }
    
    # Combined plot without the subtitle
    combined_plot <- plots[["mth"]] + plots[["rla"]] + 
      plot_annotation(
        title = paste("Grade", as.numeric(grade), ": Dynamic Effects of Closure"),
        theme = theme(plot.title = element_text(size = 22, face = "bold", hjust = 0.5))
      )
    
    file_name <- paste0(out_dir, "Grade_", grade, "_Combined.png")
    ggsave(file_name, plot = combined_plot, width = 11, height = 5.5, bg = "white")
  }
}

main()
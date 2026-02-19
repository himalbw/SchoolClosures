library(tidyverse)
library(did)

main <- function() {
    INDIR <- "data/clean"
    OUTDIR <- "figures/figure_att_aggregated"
    dir.create(OUTDIR, recursive = TRUE, showWarnings = FALSE)

    panel_closures <- read_csv(file.path(INDIR_NEWSPAPERS, "panel_newspapers_county.csv"), show_col_types = FALSE)

    panel_outcomes <- read_csv(file.path(INDIR_TWITTER, "panel_twitter_county.csv"), show_col_types = FALSE)

    panel <- prepare_panel(panel_closures, panel_outcomes)


}


prepare_panel <- function(panel_closures, panel_outcomes) {

}


main()


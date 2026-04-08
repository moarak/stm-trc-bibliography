# ============================================================
# Script: 01_load_data.R
# Purpose: 
#   1. Load raw dataset
#   2. Verify required variables
#   3. Select relevant columns for analysis
#   4. Save processed dataset as RDS
# Inputs:
#   data/raw/peru_trc_bibliography_2003_2024.csv
# Outputs:
#   data/processed/data_selected.rds
# ============================================================

rm(list = ls())

# ---- 0. Libraries ----
library(tidyverse)
library(here)

# ---- 1. Load raw dataset ----
raw_data <- read_csv(
  here("data/raw/peru_trc_bibliography_2003_2024.csv"),
  show_col_types = FALSE
)

# ---- 2. Verify required variables ----
required_cols <- c(
  "doc_type", # Type of document
  "title_eng", # Title in English
  "abstract_eng", # Abstract in English
  "publication_year", # Year of publication
  "language_orig", # Original language of publication
  "unbis_concept" # Concept by UNBIS Thesaurus
)

missing_cols <- setdiff(required_cols, colnames(raw_data))

if (length(missing_cols) > 0) {
  stop(
    paste("Missing required columns:",
          paste(missing_cols, collapse = ", "))
  )
}

# ---- 3. Select relevant columns for analysis ----
data_selected <- raw_data %>%
  select(all_of(required_cols))

glimpse(data_selected)

# ---- 4. Save processed dataset as RDS ----

# Create directories (if not existing)
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

saveRDS(
  data_selected,
  here("data/processed/dataset_selected.rds")
)

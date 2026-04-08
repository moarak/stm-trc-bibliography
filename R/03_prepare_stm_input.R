# ============================================================
# Script: 03_prepare_stm_input.R
# Purpose: Prepare STM-ready dataset
#   1. Construct `text` variable (merge titles and abstracts)
#   2. Prepare covariates for topic prevalence analysis
#   3. Lump uncommon factor levels:
#       * Keep 4 most common levels in document type
#       * Keep 4 most common levels in concept
#       * Keep 2 most common levels in original language
#   4. Construct and save final modeling dataset for STM ingestion
#
# Inputs:
#   - data/processed/dataset_selected.rds
#
# Outputs:
#   - data/processed/stm_input.rds
# ============================================================


# ---- Libraries ----
library(tidyverse)
library(here)
library(forcats)
library(stringr)

# ---- Load data ----
bib_data <- readRDS(here("data", "processed", "dataset_selected.rds"))

# ============================================================
# 1. Construct `text` variable (unit of analysis)
# ============================================================

# Combine titles and abstracts in English into a single column `text`
# NA values are replaced with empty strings to avoid propagation

data_prepared <- bib_data %>%
  mutate(
    text = str_trim(
      paste(
        coalesce(title_eng, ""),
        coalesce(abstract_eng, ""),
        sep = " "
      )
    )
  )


# ============================================================
# 2. Prepare covariates
# ============================================================

data_prepared <- data_prepared %>%
  mutate(
    # Ensure publication_year is numeric (continuous prevalence effect)
    publication_year = as.numeric(publication_year),
    
    # Convert to factors before lumping
    doc_type = as.factor(doc_type),
    unbis_concept = as.factor(unbis_concept),
    language_orig = as.factor(language_orig)
  )


# ============================================================
# 3. Lump uncommon factor levels
# ============================================================

data_prepared <- data_prepared %>%
  mutate(
    # Keep 4 most common document types
    doc_type = fct_lump_n(doc_type, n = 4, other_level = "other"),
    
    # Keep 4 most common concepts
    unbis_concept = fct_lump_n(unbis_concept, n = 4, other_level = "other"),
    
    # Keep 2 most common languages
    language_orig = fct_lump_n(language_orig, n = 2, other_level = "other")
  )


# ==============================================================
# 4. Construct and save final modeling dataset for STM ingestion
# ==============================================================

stm_input <- data_prepared %>%
  select(
    text,
    publication_year,
    doc_type,
    unbis_concept,
    language_orig
  )

# Minimal diagnostics
message("Empty text fields: ", sum(stm_input$text == ""))
message("Missing values per variable:")
print(sapply(stm_input, function(x) sum(is.na(x))))


saveRDS(
  stm_input,
  here("data", "processed", "stm_input.rds")
)

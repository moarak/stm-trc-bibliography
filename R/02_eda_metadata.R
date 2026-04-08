# ============================================================
# Script: 02_eda_metadata.R
# Purpose:
#   Construct 4 descriptive objects for EDA (bibliographic metadata):
#       1. Publication year trend (ggplot2 line chart)
#       2. Distribution of documents by type (gt table)
#       3. Distribution of documents by concept (gt table)
#       4. Distribution of documents by original language of publication (gt table)
#
# Inputs:
#   - data/processed/dataset_selected.rds
#
# Outputs:
#   - output/figures/fig_publication_year.png
#   - output/tables/tbl_doc_type.rtf
#   - output/tables/tbl_concept.rtf
#   - output/tables/tbl_language_orig.rtf
# ============================================================

# ---- Libraries ----
library(tidyverse)
library(gt)
library(here)

# ---- Load data ----
bib_data <- readRDS(here("data", "processed", "dataset_selected.rds"))

# ---- Create output directories (if not existing) ----
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 1. Figure — Documents by year of publication
# ============================================================

publication_year_df <- bib_data %>%
  count(publication_year, name = "n") %>%
  arrange(publication_year)

fig_publication_year <-
  ggplot(publication_year_df,
         aes(x = publication_year, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") +
  labs(
    title = "Documents by year of publication",
    x = "Year",
    y = "Number of documents"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(publication_year_df$publication_year),
      max(publication_year_df$publication_year),
      by = 3
    )
  ) +
  scale_y_continuous(limits = c(0, 25)) +
  theme_minimal()

# PNG for compendium reproducibility; EPS for journal submission generated separately
ggsave(
  filename = here("output", "figures", "fig_publication_year.png"),
  plot = fig_publication_year,
  width = 8,
  height = 4,
  dpi = 300
)

# ============================================================
# 2. Table — Documents by type
# ============================================================

doc_type_df <- bib_data %>%
  count(doc_type, name = "n") %>%
  mutate(prop = round(n / sum(n), digits = 3)) %>%
  arrange(desc(n))

tbl_doc_type <- doc_type_df %>%
  gt() %>%
  tab_header(
    title = "Distribution of documents by type"
  ) %>%
  cols_label(
    doc_type = "Document Type",
    n = "Frequency",
    prop = "Proportion"
  )

gtsave(
  tbl_doc_type,
  here("output", "tables", "tbl_doc_type.rtf")
)

# ============================================================
# 3. Table — Documents by concept
# ============================================================

unbis_concept_df <- bib_data %>%
  count(unbis_concept, name = "n") %>%
  mutate(prop = round(n / sum(n), digits = 3)) %>%
  arrange(desc(n))

tbl_concept <- unbis_concept_df %>%
  gt() %>%
  tab_header(
    title = "Distribution of documents by UNBIS concept"
  ) %>%
  cols_label(
    unbis_concept = "Concept",
    n = "Frequency",
    prop = "Proportion"
  )

gtsave(
  tbl_concept,
  here("output", "tables", "tbl_concept.rtf")
)

# ============================================================
# 4. Table — Documents by original language of publication
# ============================================================

language_orig_df <- bib_data %>%
  count(language_orig, name = "n") %>%
  mutate(prop = round(n / sum(n), digits = 3)) %>%
  arrange(desc(n))

tbl_language_orig <- language_orig_df %>%
  gt() %>%
  tab_header(
    title = "Distribution of documents by original language of publication"
  ) %>%
  cols_label(
    language_orig = "Language",
    n = "Frequency",
    prop = "Proportion"
  )

gtsave(
  tbl_language_orig,
  here("output", "tables", "tbl_language_orig.rtf")
)

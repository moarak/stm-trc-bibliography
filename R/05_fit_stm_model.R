# ============================================================
# Script: 05_fit_stm_model.R
# Purpose: Estimate model
#   1. Select number of topics (K)
#   2. Estimate STM models across candidate K values
#   3. Evaluate topic quality (semantic coherence & exclusivity)
#   4. Select and save final STM model
#
# Inputs:
#   - data/processed/stm_documents.rds
#   - data/processed/stm_vocab.rds
#   - data/processed/stm_meta.rds
#
# Outputs:
#   - output/figures/fig_searchK_diagnostics.png
#   - output/figures/fig_topic_quality_k{k}.png
#   - data/models/searchK_results.rds
#   - data/models/stm_models_list.rds
#   - data/models/stm_model_final.rds
# ============================================================


# ---- Libraries ----
library(stm)
library(tidyverse)
library(here)

# ---- Load processed STM objects ----
documents <- readRDS(here("data", "processed", "stm_documents.rds"))
vocab <- readRDS(here("data", "processed", "stm_vocab.rds"))
meta <- readRDS(here("data", "processed", "stm_meta.rds"))

# ---- Create directories (if not existing) ----
dir.create(here("data", "models"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Select number of topics (K)
# ============================================================

# ---- Placeholder: K range ----
K_RANGE <- 5:20  # <-- EDIT if needed

searchK_results <- searchK(
  documents,
  vocab,
  init.type = "Spectral",
  K = K_RANGE,
  prevalence = ~ s(publication_year) + doc_type + unbis_concept + language_orig,
  data = meta,
  max.em.its = 75
)

saveRDS(
  searchK_results,
  here("data", "models", "searchK_results.rds")
)


# ---- Save searchK diagnostics as figure ----

png(
  here("output", "figures", "fig_searchK_diagnostics.png"),
  width = 8, height = 6, units = "in", res = 300
)
plot(searchK_results)
dev.off()

# ============================================================
# 2. Estimate STM models across candidate K values
# ============================================================

stm_models_list <- list()

K_RANGE_candidates <- 5:7  # <-- EDIT if needed

for (k in K_RANGE_candidates) {
  
  message("Estimating STM with K = ", k)
  
  stm_models_list[[paste0("K_", k)]] <- stm(
    documents = documents,
    vocab = vocab,
    K = k,
    prevalence = ~ s(publication_year) + doc_type + unbis_concept + language_orig,
    data = meta,
    max.em.its = 75,
    init.type = "Spectral",
    seed = 1234
  )
}

saveRDS(
  stm_models_list,
  here("data", "models", "stm_models_list.rds")
)

# ============================================================
# 3. Evaluate topic quality (semantic coherence & exclusivity)
# ============================================================

# Evaluate semantic coherence & exclusivity for each model
for (k in names(stm_models_list)) {
  
  model_obj <- stm_models_list[[k]]
  
  # Extract numeric K for filename
  k_value <- gsub("K_", "", k)
  
  # Save topic quality plot
  png(
    here("output", "figures",
         paste0("fig_topic_quality_k", k_value, ".png")),
    width = 8, height = 6, units = "in", res = 300
  )
  topicQuality(model_obj, documents = documents)
  dev.off()
  
}


# ============================================================
# 4. Select and save final STM model
# ============================================================

# ---- Placeholder: Final selected K ----
FINAL_K <- 6  # <-- SET after inspecting diagnostics

stm_model_final <- stm_models_list[[paste0("K_", FINAL_K)]]

saveRDS(
  stm_model_final,
  here("data", "models", "stm_model_final.rds")
)

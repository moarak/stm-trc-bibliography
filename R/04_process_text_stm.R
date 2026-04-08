# ============================================================
# Script: 04_process_text_stm.R
# Purpose: Process textual data for later model estimation
#   1. Normalize textual data:
#       * lowercase
#       * stem (English corpus)
#       * remove punctuation, numbers, stop words and rare terms
#   2. Construct STM input objects:
#       * documents
#       * vocab
#       * meta
#   3. Save processed STM objects
#
# Inputs:
#   - data/processed/stm_input.rds
#
# Outputs:
#   - data/processed/stm_documents.rds
#   - data/processed/stm_vocab.rds
#   - data/processed/stm_meta.rds
# ============================================================


# ---- Libraries ----
library(stm)
library(tidyverse)
library(here)

# ---- Load STM input ----
stm_input <- readRDS(
  here("data", "processed", "stm_input.rds")
)


# ============================================================
# 1. Normalize textual data
# ============================================================

# ---- Custom stopwords vector ----
custom_stopwords <- c(
  "cvr", "trc", "peru", "peruvian", "truth", 
  "reconciliation", "commission", "final", "report"
)

processed <- textProcessor(
  documents = stm_input$text,
  metadata = stm_input,
  lowercase = TRUE,
  removestopwords = TRUE,
  removenumbers = TRUE,
  removepunctuation = TRUE,
  stem = TRUE,                     # Stemming enabled (English corpus)
  wordLengths = c(3, Inf),
  customstopwords = custom_stopwords
)

# ============================================================
# 2. Construct STM input objects
# ============================================================

# ---- Rare word threshold ----
LOWER_THRESH <- 10  # <-- EDIT if needed

out <- prepDocuments(
  processed$documents,
  processed$vocab,
  processed$meta,
  lower.thresh = LOWER_THRESH
)

documents <- out$documents
vocab     <- out$vocab
meta      <- out$meta

# Basic diagnostics
message("Number of documents: ", length(documents))
message("Vocabulary size: ", length(vocab))


# ============================================================
# 3. Save processed STM objects
# ============================================================

saveRDS(
  documents,
  here("data", "processed", "stm_documents.rds")
)

saveRDS(
  vocab,
  here("data", "processed", "stm_vocab.rds")
)

saveRDS(
  meta,
  here("data", "processed", "stm_meta.rds")
)

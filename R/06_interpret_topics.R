# ============================================================
# Script: 06_interpret_topics.R
# Purpose:
#   Interpret STM results at a descriptive level
#   Generate figures:
#       * Fig — Topic prevalence summary (corpus-level)
#       * Fig — Top terms by beta (faceted horizontal bar plot)
#       * Fig — Expected topic proportions by publication year
#       * Fig — Expected topic proportions by document type
#       * Fig — Expected topic proportions by UNBIS concept
#       * Fig — Expected topic proportion difference by language
#
# Inputs:
#   - data/models/stm_model_final.rds
#   - data/processed/stm_meta.rds
#
# Outputs:
#   - output/figures/fig_topic_prevalence_summary.png
#   - output/figures/fig_topic_terms_beta.png
#   - output/figures/fig_topic_prevalence_publication_year.png
#   - output/figures/fig_topic_prevalence_doc_type.png
#   - output/figures/fig_topic_prevalence_concept.png
#   - output/figures/fig_topic_prevalence_language_orig.png
#   - data/models/stm_effects.rds
# ============================================================


# ---- Libraries ----
library(stm)
library(stminsights)
library(tidyverse)
library(here)
library(tidytext)
library(viridis)

# ---- Create output directories (if not existing) ----
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Load final STM model
# ============================================================

stm_model_final <- readRDS(
  here("data", "models", "stm_model_final.rds")
)

meta <- readRDS(
  here("data", "processed", "stm_meta.rds")
)


# ============================================================
# 2. Figure — Topic prevalence summary (corpus-level)
# ============================================================

# Base R plot saved with png()/dev.off() — STM summary plot has no ggplot2 equivalent
png(
  here("output", "figures", "fig_topic_prevalence_summary.png"),
  width = 8, height = 6, units = "in", res = 300
)
plot(stm_model_final, type = "summary", n = 5)
dev.off()


# ============================================================
# 3. Figure — Top terms by beta
# ============================================================

TOP_N <- 10  # <-- Edit number of terms per topic if needed

beta_terms <- tidy(stm_model_final, matrix = "beta")

top_terms_beta <- beta_terms %>%
  group_by(topic) %>%
  slice_max(beta, n = TOP_N) %>%
  ungroup() %>%
  arrange(topic, desc(beta))

fig_topic_terms_beta <-
  top_terms_beta %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = beta, y = term)) +
  geom_col() +
  facet_wrap(~ topic, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Top terms by topic (beta)",
    x = "Beta",
    y = "Term"
  ) +
  theme_minimal()

ggsave(
  filename = here("output", "figures", "fig_topic_terms_beta.png"),
  plot = fig_topic_terms_beta,
  width = 10, height = 8, dpi = 300
)


# ============================================================
# 4. Estimate covariate effects
# ============================================================

# Determine K dynamically from model
K <- stm_model_final$settings$dim$K

effect_model <- estimateEffect(
  1:K ~ s(publication_year) + doc_type + unbis_concept + language_orig,
  stm_model_final,
  metadata = meta,
  uncertainty = "Global"
)

saveRDS(
  effect_model,
  here("data", "models", "stm_effects.rds")
)


# ============================================================
# 5. Figure — Expected topic proportions by publication year
# ============================================================

effects_publication_year_df <- get_effects(
  estimates = effect_model,
  variable = "publication_year",
  type = "continuous"
)

fig_topic_prevalence_publication_year <- effects_publication_year_df %>%
  ggplot(aes(x = value, y = proportion)) +
  geom_line() +
  facet_wrap(~ topic) +
  labs(
    title = "Expected topic proportions by publication year",
    x = "Year",
    y = "Expected proportion"
  ) +
  theme_minimal()

ggsave(
  filename = here("output", "figures",
                  "fig_topic_prevalence_publication_year.png"),
  plot = fig_topic_prevalence_publication_year,
  width = 8, height = 5, dpi = 300
)


# ============================================================
# 6. Figure — Expected topic proportions by document type
# ============================================================

effects_doc_type_df <- get_effects(
  estimates = effect_model,
  variable = "doc_type",
  type = "pointestimate"
)

fig_topic_prevalence_doc_type <- ggplot(
  effects_doc_type_df, aes(x = topic, y = value, fill = proportion)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       limits = c(0.0, 1.0)) +
  theme_minimal() +
  labs(
    title = "Expected topic proportions by document type",
    x = "Topic",
    y = "",
    fill = "Proportion"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0) # Keep topic numbers horizontal
  )

ggsave(
  filename = here("output", "figures",
                  "fig_topic_prevalence_doc_type.png"),
  plot = fig_topic_prevalence_doc_type,
  width = 8, height = 5, dpi = 300
)


# ============================================================
# 7. Figure — Expected topic proportions by UNBIS concept
# ============================================================

effects_concept_df <- get_effects(
  estimates = effect_model,
  variable = "unbis_concept",
  type = "pointestimate"
)

# Clamp negative proportions to 0 (artefact of estimation uncertainty)
effects_concept_df <- effects_concept_df %>%
  mutate(proportion = ifelse(proportion < 0, 0, proportion))

fig_topic_prevalence_concept <- ggplot(
  effects_concept_df, aes(x = topic, y = value, fill = proportion)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       limits = c(0.0, 1.0)) +
  theme_minimal() +
  labs(
    title = "Expected topic proportions by UNBIS concept",
    x = "Topic",
    y = "",
    fill = "Proportion"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 0) # Keep topic numbers horizontal
  )

ggsave(
  filename = here("output", "figures",
                  "fig_topic_prevalence_concept.png"),
  plot = fig_topic_prevalence_concept,
  width = 8, height = 5, dpi = 300
)


# ============================================================
# 8. Figure — Expected topic proportion difference by language
# ============================================================

# Base R plot saved with png()/dev.off() — uses STM native plot method
# Update custom.labels if K or topic interpretation changes
png(
  here("output", "figures", "fig_topic_prevalence_language_orig.png"),
  width = 8, height = 5, units = "in", res = 300
)
plot(effect_model,
     covariate = "language_orig",
     topics = 1:K,
     model = stm_model_final,
     method = "difference",
     cov.value1 = "eng", cov.value2 = "spa",
     main = "Expected topic proportion: difference by language of publication",
     xlab = "More Spanish ... More English",
     labeltype = "custom",
     custom.labels = paste("Topic", 1:K),
     xlim = c(-0.5, 0.5))
dev.off()

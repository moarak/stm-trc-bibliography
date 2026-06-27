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
#   - data/models/stm_effects.rds
#   - data/processed/stm_meta.rds
#
# Outputs:
#   - output/figures/fig_topic_prevalence_summary.png
#   - output/figures/fig_topic_terms_beta.png
#   - output/figures/fig_topic_prevalence_publication_year.png
#   - output/figures/fig_topic_prevalence_doc_type.png
#   - output/figures/fig_topic_prevalence_concept.png
#   - output/figures/fig_topic_prevalence_language_orig.png
# ============================================================


# ---- Libraries ----
library(stm)
library(stminsights)
library(tidyverse)
library(here)
library(tidytext)

# ---- Create output directories (if not existing) ----
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)


# ============================================================
# 1. Load final STM model
# ============================================================

stm_model_final <- readRDS(
  here("data", "models", "stm_model_final.rds")
)

effect_model <- readRDS(
  here("data", "models", "stm_effects.rds")
)

meta <- readRDS(
  here("data", "processed", "stm_meta.rds")
)

K <- stm_model_final$settings$dim$K


# ============================================================
# 2. Figure — Topic prevalence summary (corpus-level)
# ============================================================

# PNG for compendium reproducibility
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
# 4. Figure — Expected topic proportions by publication year
# ============================================================

effects_publication_year_df <- get_effects(
  estimates = effect_model,
  variable = "publication_year",
  type = "continuous"
)

fig_topic_prevalence_publication_year <- effects_publication_year_df %>%
  ggplot(aes(x = value, y = proportion, ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.2) +
  geom_line() +
  facet_wrap(~ topic,
             labeller = as_labeller(\(x) paste("Topic", x))) +
  labs(
    title = "Topical prevalence by year of publication",
    x = "Year",
    y = "Estimated proportion"
  )

ggsave(
  filename = here("output", "figures",
                  "fig_topic_prevalence_publication_year.png"),
  plot = fig_topic_prevalence_publication_year,
  width = 8, height = 5, dpi = 300
)


# ============================================================
# 5. Figure — Expected topic proportions by document type
# ============================================================

effects_doc_type_df <- get_effects(
  estimates = effect_model,
  variable = "doc_type",
  type = "pointestimate"
)

fig_topic_prevalence_doc_type <- effects_doc_type_df %>%
  ggplot(aes(x = value, y = proportion, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  facet_wrap(~ topic, 
             labeller = as_labeller(\(x) paste("Topic", x))) +
  coord_flip(ylim = c(-0.25, 1)) +
  labs(
    title = "Topical prevalence by document type",
    x = "Document type",
    y = "Estimated proportion"
  )

ggsave(
  filename = here("output", "figures",
                  "fig_topic_prevalence_doc_type.png"),
  plot = fig_topic_prevalence_doc_type,
  width = 8, height = 5, dpi = 300
)


# ============================================================
# 6. Figure — Expected topic proportions by UNBIS concept
# ============================================================

effects_concept_df <- get_effects(
  estimates = effect_model,
  variable = "unbis_concept",
  type = "pointestimate"
)

fig_topic_prevalence_concept <- effects_concept_df %>%
  ggplot(aes(x = value, y = proportion, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  facet_wrap(~ topic,
             labeller = as_labeller(\(x) paste("Topic", x))) +
  coord_flip(ylim = c(-0.25, 1)) +
  labs(
    title = "Topical prevalence by UNBIS concept",
    x = "UNBIS concept",
    y = "Estimated proportion"
  )

ggsave(
  filename = here("output", "figures",
                  "fig_topic_prevalence_concept.png"),
  plot = fig_topic_prevalence_concept,
  width = 8, height = 5, dpi = 300
)


# ============================================================
# 7. Figure — Expected topic proportion difference by language
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
     main = "Topical prevalence contrast by language of publication",
     xlab = "Spanish ... English",
     labeltype = "custom",
     custom.labels = paste("Topic", 1:K),
     xlim = c(-0.5, 0.5))
dev.off()
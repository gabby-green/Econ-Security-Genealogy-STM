# =============================================================================
# STM Analysis: US, EU
# =============================================================================

# Load shared configuration
library(here)
source(here("01_Scripts", "00_configuration.R"))

# Toggle which analyses to run
run_us <- TRUE
run_eu <- TRUE

# =============================================================================
# OUTPUT DIRECTORY SETUP
# =============================================================================

stm_outputs <- here("03_STM_Outputs")
output_dir_us <- here("03_STM_Outputs", "US")
output_dir_eu <- here("03_STM_Outputs", "EU")

dir.create(stm_outputs, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_us, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_eu, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# LOAD DATA (using shared function)
# =============================================================================

corpus_all <- load_corpus()
corpus_us <- corpus_all %>% filter(institution == "US")
corpus_eu <- corpus_all %>% filter(institution == "EU")

# =============================================================================
# MODEL FITTING FUNCTION
# =============================================================================

fit_stm <- function(out, output_dir, prefix, prevalence_formula, K = 20) {
  
  setwd(output_dir)
  
  message("\n=== Fitting STM: ", prefix, " (K=", K, ") ===")
  
  stm_model <- stm(
    documents = out$documents,
    vocab = out$vocab,
    K = K,
    prevalence = prevalence_formula,
    data = out$meta,
    max.em.its = 150,
    init.type = "Spectral",
    verbose = TRUE
  )
  
  saveRDS(stm_model, paste0(prefix, "_stm_model_k", K, ".rds"))
  saveRDS(out, paste0(prefix, "_stm_preprocessed.rds"))
  
  png(paste0(prefix, "_topic_summary.png"), width = 1000, height = 800)
  plot(stm_model, type = "summary", xlim = c(0, 0.3))
  dev.off()
  
  png(paste0(prefix, "_topic_correlations.png"), width = 800, height = 800)
  plot(topicCorr(stm_model))
  dev.off()
  
  topic_words <- capture.output(labelTopics(stm_model, n = 20))
  writeLines(topic_words, paste0(prefix, "_topic_words.txt"))
  
  topic_words <- capture.output(labelTopics(stm_model, n = 30))
  writeLines(topic_words, paste0(prefix, "_topic_words_extended.txt"))
  
  write_csv(make.dt(stm_model, meta = out$meta), paste0(prefix, "_topic_proportions.csv"))
  
  return(stm_model)
}

# =============================================================================
# YEAR EFFECTS FUNCTION
# =============================================================================

analyze_year_effects <- function(stm_model, out, output_dir, prefix, K) {
  
  setwd(output_dir)
  
  # Construct formula as string then convert
  form_year <- as.formula(paste0("c(", paste(1:K, collapse = ","), ") ~ year"))
  prep_year <- estimateEffect(form_year, stm_model, meta = out$meta)
  saveRDS(prep_year, paste0(prefix, "_effects_year.rds"))
  
  # ---------------------------------------------
  # 1. Grid of all topic temporal trends
  # ---------------------------------------------
  
  # Calculate grid dimensions
  n_cols <- 5
  n_rows <- ceiling(K / n_cols)
  
  png(paste0(prefix, "_year_trends_all.png"), width = 1400, height = 200 * n_rows)
  par(mfrow = c(n_rows, n_cols), mar = c(3, 3, 2, 1))
  for (i in 1:K) {
    plot(prep_year, covariate = "year", topics = i,
         method = "continuous", main = paste("Topic", i),
         xlab = "Year", ylab = "Prevalence")
  }
  dev.off()
  
  # ---------------------------------------------
  # 2. Individual topic plots in subfolder
  # ---------------------------------------------
  
  dir.create("topic_trends", showWarnings = FALSE)
  
  for (i in 1:K) {
    png(paste0("topic_trends/topic_", sprintf("%02d", i), "_trend.png"), 
        width = 600, height = 400)
    par(mar = c(4, 4, 3, 1))
    plot(prep_year, covariate = "year", topics = i,
         method = "continuous", main = paste("Topic", i, "- Temporal Trend"),
         xlab = "Year", ylab = "Expected Topic Proportion")
    dev.off()
  }
  
  # ---------------------------------------------
  # 3. Extract coefficients and identify trends
  # ---------------------------------------------
  
  declining <- c()
  rising <- c()
  year_summary <- tibble(Topic = 1:K, Estimate = NA_real_, StdError = NA_real_, P_value = NA_real_)
  
  for (i in 1:K) {
    coef_info <- summary(prep_year, topics = i)$tables[[1]]
    year_row <- coef_info["year", ]
    year_summary$Estimate[i] <- year_row["Estimate"]
    year_summary$StdError[i] <- year_row["Std. Error"]
    year_summary$P_value[i] <- year_row["Pr(>|t|)"]
    
    if (year_row["Pr(>|t|)"] < 0.05) {
      if (year_row["Estimate"] < 0) declining <- c(declining, i)
      else rising <- c(rising, i)
    }
  }
  
  year_summary <- year_summary %>%
    mutate(
      Direction = ifelse(Estimate > 0, "Rising", "Declining"),
      Significant = ifelse(P_value < 0.05, "*", ""),
      Estimate = round(Estimate, 5),
      StdError = round(StdError, 5),
      P_value = round(P_value, 4)
    ) %>%
    arrange(Estimate)
  
  write_csv(year_summary, paste0(prefix, "_year_effects_summary.csv"))
  
  # ---------------------------------------------
  # 4. Combined plots for significant trends
  # ---------------------------------------------
  
  if (length(declining) > 0) {
    png(paste0(prefix, "_year_declining.png"), width = 1200, height = 800)
    plot(prep_year, covariate = "year", topics = declining,
         method = "continuous", main = paste(toupper(prefix), "- Declining Topics"))
    dev.off()
  }
  
  if (length(rising) > 0) {
    png(paste0(prefix, "_year_rising.png"), width = 1200, height = 800)
    plot(prep_year, covariate = "year", topics = rising,
         method = "continuous", main = paste(toupper(prefix), "- Rising Topics"))
    dev.off()
  }
  
  # ---------------------------------------------
  # 5. Print summary
  # ---------------------------------------------
  
  message("\n=== ", toupper(prefix), " MODEL EFFECTS SUMMARY ===")
  message("\nSignificant year effects:")
  message("  Rising: ", paste(rising, collapse = ", "))
  message("  Declining: ", paste(declining, collapse = ", "))
  
  return(list(prep = prep_year, declining = declining, rising = rising, summary = year_summary))
}

# =============================================================================
# RUN ANALYSES
# =============================================================================

if (run_us) {
  message("\n========== US CORPUS ==========")
  out_us <- preprocess_corpus(corpus_us, phrases_us_full, min_docfreq = 7, corpus = "US")
  stm_us <- fit_stm(out_us, output_dir_us, "us", ~ year, K = 20)
  effects_us <- analyze_year_effects(stm_us, out_us, output_dir_us, "us", K = 20)
}

if (run_eu) {
  message("\n========== EU CORPUS ==========")
  out_eu <- preprocess_corpus(corpus_eu, phrases_eu_full, min_docfreq = 5, corpus = "EU")
  stm_eu <- fit_stm(out_eu, output_dir_eu, "eu", ~ year, K = 15)
  effects_eu <- analyze_year_effects(stm_eu, out_eu, output_dir_eu, "eu", K = 15)
}

# =============================================================================
# SUMMARY
# =============================================================================

message("\n=== ANALYSIS COMPLETE ===")
message("Outputs saved to: ", stm_outputs)
message("\nOutput structure:")
message("  STM_Outputs/")
message("    ├── US/")
message("    │   ├── us_stm_model_k20.rds")
message("    │   ├── us_stm_preprocessed.rds")
message("    │   ├── us_effects_year.rds")
message("    │   ├── us_topic_summary.png")
message("    │   ├── us_topic_correlations.png")
message("    │   ├── us_topic_words.txt")
message("    │   ├── us_topic_words_extended.txt")
message("    │   ├── us_topic_proportions.csv")
message("    │   ├── us_year_trends_all.png")
message("    │   ├── us_year_effects_summary.csv")
message("    │   ├── us_year_rising.png")
message("    │   ├── us_year_declining.png")
message("    │   └── topic_trends/")
message("    └── EU/")
message("        └── [same structure with eu_ prefix]")
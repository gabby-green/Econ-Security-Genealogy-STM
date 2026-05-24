# =============================================================================
# preText Analysis Using the preText Package
# Following Denny & Spirling (2018)
# =============================================================================

# =============================================================================
# SET-UP
# =============================================================================

# Load required packages 
library(preText)
library(quanteda)
library(tidyverse)
library(here)

# *** SET WHICH CORPUS TO ANALYZE - CHANGE THIS ***
CORPUS <- "EU"  # Change to "US" for EU corpus analysis

# Validate CORPUS selection
if (!CORPUS %in% c("US", "EU")) {
  stop("CORPUS must be either 'US' or 'EU'. Current value: ", CORPUS)
}

# Source your configuration
source(here("01_Scripts", "00_configuration.R"))

# Create output directory with corpus identifier
output_dir <- here("06_preText_Analysis", CORPUS)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
setwd(output_dir)

message("\n========================================")
message("preText Analysis Using preText Package")
message("========================================\n")
message("Corpus: ", CORPUS)
message("Output directory: 06_preText_Analysis/", CORPUS, "/")
message("All results will be prefixed with '", tolower(CORPUS), "_'\n")

# =============================================================================
# LOAD DATA
# =============================================================================

message("Loading corpus data...")
corpus_all <- load_corpus()

if (CORPUS == "US") {
  corpus_data <- corpus_all %>% filter(institution == "US")
} else {
  corpus_data <- corpus_all %>% filter(institution == "EU")
}

message("Documents loaded: ", nrow(corpus_data))

# Sample if corpus is very large (for computational feasibility)
MAX_DOCS <- 200

if (nrow(corpus_data) > MAX_DOCS) {
  message("Sampling ", MAX_DOCS, " documents for analysis...")
  set.seed(12345)
  corpus_data <- corpus_data %>% slice_sample(n = MAX_DOCS)
}

# Extract text as character vector (required for preText)
documents <- corpus_data$text

# =============================================================================
# FACTORIAL PREPROCESSING
# =============================================================================

message("\n=== RUNNING FACTORIAL PREPROCESSING ===")
message("Generating 2^7 = 128 preprocessing specifications...")
message("This will take 30-90 minutes.\n")

# preText's factorial_preprocessing generates all combinations of:
# - Punctuation removal (on/off)
# - Number removal (on/off)
# - Lowercase (on/off)
# - Stemming (on/off)
# - Stopword removal (on/off)
# - Infrequent term removal (on/off)
# - N-grams (on/off)

preprocessed <- factorial_preprocessing(
  documents,
  use_ngrams = TRUE,
  infrequent_term_threshold = 0.01,
  verbose = TRUE
)

message("\nFactorial preprocessing complete!")
message("Specifications generated: ", nrow(preprocessed$preprocessing_steps))

# =============================================================================
# RUN preText ANALYSIS
# =============================================================================

message("\n=== CALCULATING preText SCORES ===")
message("Comparing document distances across specifications...")
message("This will take 30-60 minutes.\n")

# Run preText on the factorial preprocessed documents
pretext_results <- preText(
  preprocessed,
  dataset_name = paste(CORPUS, "Corpus"),
  distance_method = "cosine",
  num_comparisons = 50,  # Number of document pairs to compare
  verbose = TRUE
)

message("\npreText analysis complete!")

# =============================================================================
# RESULTS AND INTERPRETATION
# =============================================================================

message("\n=== GENERATING RESULTS ===")

# 1. preText Score Plot
png(paste0(tolower(CORPUS), "_pretext_scores_plot.png"), width = 1200, height = 1000, res = 120)
preText_score_plot(pretext_results)
dev.off()

message("✓ Created ", tolower(CORPUS), "_pretext_scores_plot.png")

# 2. Regression Coefficient Plot
png(paste0(tolower(CORPUS), "_regression_coefficients_plot.png"), width = 1000, height = 600, res = 120)
regression_coefficient_plot(pretext_results, remove_intercept = TRUE)
dev.off()

message("✓ Created ", tolower(CORPUS), "_regression_coefficients_plot.png")

# Extract results for further analysis
pretext_scores <- pretext_results$preText_scores
regression_results <- pretext_results$regression_results

# Save results with corpus prefix
write_csv(pretext_scores, paste0(tolower(CORPUS), "_pretext_scores.csv"))
write_csv(regression_results, paste0(tolower(CORPUS), "_regression_results.csv"))
saveRDS(pretext_results, paste0(tolower(CORPUS), "_pretext_full_results.rds"))

message("✓ Saved results files")

# =============================================================================
# GENERATE INTERPRETATION REPORT
# =============================================================================

message("\n=== GENERATING INTERPRETATION REPORT ===")

# Get regression coefficients
reg_coefs <- regression_results

# Find best and worst specifications
best_spec <- pretext_scores %>% slice_min(preText_score, n = 1)
worst_spec <- pretext_scores %>% slice_max(preText_score, n = 1)

# Identify your current specification
# (This requires mapping your choices to the preText specification format)
# For now, we'll identify the specification closest to your approach

# Check if stopword removal and stemming reduce sensitivity
stopword_effect <- reg_coefs %>% 
  filter(str_detect(Variable, "[Ss]topword")) %>% 
  pull(Coefficient)

stem_effect <- reg_coefs %>% 
  filter(str_detect(Variable, "[Ss]tem")) %>% 
  pull(Coefficient)

# Generate report
report <- c(
  "========================================",
  paste("preText Analysis Results -", CORPUS, "Corpus"),
  paste("Corpus:", CORPUS),
  paste("Documents analyzed:", length(documents)),
  "========================================",
  "",
  "SPECIFICATION RANKINGS:",
  "------------------------",
  "",
  "Most robust specification (lowest preText score):",
  paste("Score:", sprintf("%.4f", best_spec$preText_score)),
  "",
  "Least robust specification (highest preText score):",
  paste("Score:", sprintf("%.4f", worst_spec$preText_score)),
  "",
  "Score range:", sprintf("%.4f to %.4f", 
                          min(pretext_scores$preText_score),
                          max(pretext_scores$preText_score)),
  "",
  "========================================",
  "PREPROCESSING STEP EFFECTS",
  "========================================",
  "",
  "Regression coefficients show impact of each step:",
  "(Negative = reduces sensitivity/more robust)",
  "(Positive = increases sensitivity/less robust)",
  ""
)

# Add regression results
for (i in 1:nrow(reg_coefs)) {
  sig <- ifelse(reg_coefs$p_value[i] < 0.05, "***",
                ifelse(reg_coefs$p_value[i] < 0.10, "**",
                       ifelse(reg_coefs$p_value[i] < 0.15, "*", "")))
  
  report <- c(report,
              sprintf("%s: %.4f (SE: %.4f, p: %.3f) %s",
                      reg_coefs$Variable[i],
                      reg_coefs$Coefficient[i],
                      reg_coefs$SE[i],
                      reg_coefs$p_value[i],
                      sig)
  )
}

report <- c(report,
            "",
            "*** p<0.05, ** p<0.10, * p<0.15",
            "",
            "========================================",
            "INTERPRETATION FOR YOUR RESEARCH",
            "========================================",
            ""
)

# Interpret stopword removal effect
if (!is.null(stopword_effect) && length(stopword_effect) > 0) {
  if (stopword_effect < -0.001) {
    report <- c(report,
                "✓ STOPWORD REMOVAL: Significantly reduces result sensitivity",
                "  → Your use of stopwords (standard or custom) is VALIDATED",
                "  → Removing stopwords makes results MORE robust",
                ""
    )
  } else if (stopword_effect > 0.001) {
    report <- c(report,
                "⚠ STOPWORD REMOVAL: Increases result sensitivity",
                "  → Consider which stopwords you're removing",
                "  → May be removing substantively important terms",
                "  → Run comparison models with/without stopwords",
                ""
    )
  } else {
    report <- c(report,
                "→ STOPWORD REMOVAL: No significant effect",
                "  → Results are robust regardless of this choice",
                ""
    )
  }
}

# Interpret stemming effect
if (!is.null(stem_effect) && length(stem_effect) > 0) {
  if (stem_effect < -0.001) {
    report <- c(report,
                "✓ STEMMING: Reduces result sensitivity",
                "  → Your use of stemming is justified",
                ""
    )
  } else if (stem_effect > 0.001) {
    report <- c(report,
                "⚠ STEMMING: Increases result sensitivity",
                "  → Consider running models without stemming as robustness check",
                ""
    )
  }
}

report <- c(report,
            "",
            "========================================",
            "RECOMMENDATIONS",
            "========================================",
            "",
            "1. ROBUSTNESS CHECKS:",
            "   → Replicate your main analysis using the top 3 most robust specifications",
            "   → If findings are consistent, report as validation",
            "   → If findings diverge, report as sensitivity analysis",
            "",
            "2. FOR YOUR THESIS:",
            "   → Add this analysis to your methodology section",
            "   → Cite: Denny & Spirling (2018), Political Analysis 26(2): 168-189",
            "   → Report: 'Preprocessing sensitivity was assessed using preText",
            "      analysis across 128 specifications. Results showed [FINDING].'",
            "",
            paste0("3. FILES GENERATED (in 06_preText_Analysis/", CORPUS, "/):"),
            paste0("   - ", tolower(CORPUS), "_pretext_scores.csv: All specification scores"),
            paste0("   - ", tolower(CORPUS), "_regression_results.csv: Statistical analysis"),
            paste0("   - ", tolower(CORPUS), "_pretext_scores_plot.png: Visual comparison"),
            paste0("   - ", tolower(CORPUS), "_regression_coefficients_plot.png: Effect sizes"),
            paste0("   - ", tolower(CORPUS), "_pretext_report.txt: This report"),
            "",
            "========================================",
            "Next Steps:",
            paste0("1. Review ", tolower(CORPUS), "_pretext_scores_plot.png"),
            paste0("2. Examine ", tolower(CORPUS), "_regression_coefficients_plot.png"),
            "3. Compare your preprocessing to top-ranked specifications",
            "4. Run STM models with top specifications if needed",
            paste0("5. To analyze ", ifelse(CORPUS == "US", "EU", "US"), " corpus, change CORPUS variable and re-run"),
            "========================================"
)

writeLines(report, paste0(tolower(CORPUS), "_pretext_report.txt"))

message("\n\n========================================")
message("PRETEXT ANALYSIS COMPLETE")
message("========================================")
message("\nCorpus analyzed: ", CORPUS)
message("Specifications tested: 128")
message("Documents sampled: ", length(documents))
message("\nOutput saved to: ", here("06_preText_Analysis"))
message("\nOutput structure:")
message("  06_preText_Analysis/")
message("    ├── US/")
message("    │   ├── us_pretext_scores.csv")
message("    │   ├── us_regression_results.csv")
message("    │   ├── us_pretext_scores_plot.png")
message("    │   ├── us_regression_coefficients_plot.png")
message("    │   ├── us_pretext_report.txt")
message("    │   └── us_pretext_full_results.rds")
message("    └── EU/")
message("        ├── eu_pretext_scores.csv")
message("        ├── eu_regression_results.csv")
message("        ├── eu_pretext_scores_plot.png")
message("        ├── eu_regression_coefficients_plot.png")
message("        ├── eu_pretext_report.txt")
message("        └── eu_pretext_full_results.rds")
message("\nTo analyze ", ifelse(CORPUS == "US", "EU", "US"), " corpus:")
message("  1. Change CORPUS <- '", ifelse(CORPUS == "US", "EU", "US"), "' (line 17)")
message("  2. Re-run this script")
message("========================================")
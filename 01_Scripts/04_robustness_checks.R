# =============================================================================
# STM Model Quality Assessment
# =============================================================================
# Topic quality: semantic coherence vs exclusivity
#   (Roberts et al., 2014, pp. 1069-1070; Mimno et al., 2011;
#    Bischof & Airoldi, 2016; Roberts et al., 2019, pp. 10-11)
# =============================================================================

# Set to TRUE for each model to run quality checks on
run_eu    <- TRUE
run_us    <- TRUE

# =============================================================================
# LOAD DEPENDENCIES
# =============================================================================

library(here)
source(here("01_Scripts", "00_configuration.R"))

library(ggplot2)

# =============================================================================
# FILE PATHS
# =============================================================================

eu_model_file <- here("03_STM_Outputs", "EU", "eu_stm_model_k15.rds")
eu_prep_file  <- here("03_STM_Outputs", "EU", "eu_stm_preprocessed.rds")

us_model_file <- here("03_STM_Outputs", "US", "us_stm_model_k20.rds")
us_prep_file  <- here("03_STM_Outputs", "US", "us_stm_preprocessed.rds")

# Output directories
robustness_dir <- here("05_Robustness_Checks")
robustness_dir_us <- here("05_Robustness_Checks", "US")
robustness_dir_eu <- here("05_Robustness_Checks", "EU")

dir.create(robustness_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(robustness_dir_us, showWarnings = FALSE, recursive = TRUE)
dir.create(robustness_dir_eu, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# TOPIC QUALITY: SEMANTIC COHERENCE VS EXCLUSIVITY
# =============================================================================

run_topic_quality <- function(model, documents, model_name, output_dir) {
  
  prefix <- tolower(model_name)
  
  coherence <- semanticCoherence(model, documents)
  exclusivity_scores <- exclusivity(model)
  
  tq_df <- data.frame(
    Topic = 1:length(coherence),
    Coherence = coherence,
    Exclusivity = exclusivity_scores
  )
  
  mean_coh <- mean(tq_df$Coherence)
  mean_exc <- mean(tq_df$Exclusivity)
  
  # Add quadrant classification
  tq_df <- tq_df %>%
    mutate(
      Quadrant = case_when(
        Coherence > mean_coh & Exclusivity > mean_exc ~ "High Quality (Upper-Right)",
        Coherence > mean_coh & Exclusivity <= mean_exc ~ "High Coherence, Low Exclusivity",
        Coherence <= mean_coh & Exclusivity > mean_exc ~ "Low Coherence, High Exclusivity",
        TRUE ~ "Low Quality (Lower-Left)"
      )
    )
  
  # Add crosshair series columns for Excel scatter chart
  y_bottom   <- floor(min(tq_df$Exclusivity))
  y_top      <- ceiling(max(tq_df$Exclusivity))
  x_right    <- 0
  x_left     <- floor(min(tq_df$Coherence) / 5) * 5
  
  tq_df$X_vertical   <- NA_real_
  tq_df$Y_vertical   <- NA_real_
  tq_df$X_horizontal <- NA_real_
  tq_df$Y_horizontal <- NA_real_
  
  tq_df$X_vertical[1:2]   <- mean_coh
  tq_df$Y_vertical[1]     <- y_bottom
  tq_df$Y_vertical[2]     <- y_top
  tq_df$X_horizontal[1]   <- x_right
  tq_df$X_horizontal[2]   <- x_left
  tq_df$Y_horizontal[1:2] <- mean_exc
  
  # Save CSV
  write.csv(tq_df, file.path(output_dir, paste0(prefix, "_topic_quality.csv")), row.names = FALSE)
  
  # Create and save plot
  p <- ggplot(tq_df, aes(x = Coherence, y = Exclusivity, label = Topic)) +
    geom_point(size = 3, color = "steelblue") +
    geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
    geom_vline(xintercept = mean_coh, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = mean_exc, linetype = "dashed", color = "gray50") +
    labs(
      title = paste0("Topic Quality: ", model_name),
      subtitle = "Upper-right quadrant = highest quality topics",
      x = "Semantic Coherence",
      y = "Exclusivity"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(
    filename = file.path(output_dir, paste0(prefix, "_topic_quality.png")),
    plot = p, width = 8, height = 6, dpi = 300
  )
  
  return(tq_df)
}

# =============================================================================
# EXECUTE
# =============================================================================

all_results <- list()

if (run_eu) {
  message("Running EU topic quality assessment...")
  eu_model <- readRDS(eu_model_file)
  eu_prep <- readRDS(eu_prep_file)
  all_results$EU <- run_topic_quality(eu_model, eu_prep$documents, "EU", robustness_dir_eu)
  message("EU topic quality assessment complete.")
}

if (run_us) {
  message("Running US topic quality assessment...")
  us_model <- readRDS(us_model_file)
  us_prep <- readRDS(us_prep_file)
  all_results$US <- run_topic_quality(us_model, us_prep$documents, "US", robustness_dir_us)
  message("US topic quality assessment complete.")
}

# =============================================================================
# SUMMARY
# =============================================================================

if (length(all_results) > 0) {
  message("\n=== TOPIC QUALITY ASSESSMENT COMPLETE ===")
  message("Models processed: ", paste(names(all_results), collapse = ", "))
  message("Output saved to: ", robustness_dir)
  message("\nOutput structure:")
  message("  Robustness_Checks/")
  message("    ├── US/")
  message("    │   └── us_topic_quality.csv")
  message("    │   └── us_topic_quality.png")
  message("    └── EU/")
  message("    │   └── eu_topic_quality.csv")
  message("    │   └── eu_topic_quality.png")
}
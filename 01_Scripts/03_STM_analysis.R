# =============================================================================
# STM Analysis
# =============================================================================

library(here)
source(here("01_Scripts", "00_configuration.R"))

# Set correlation threshold if not defined in configuration
if (!exists("corr_cutoff")) {
  corr_cutoff <- 0.10
}

# =============================================================================
# TOGGLE OPTIONS
# =============================================================================

analyze_us <- TRUE
analyze_eu <- TRUE

# Genre topics (excluded from substantive analysis)
us_genre_topics <- c(6, 15)  # US06, US15
eu_genre_topics <- c(1, 12)  # EU01, EU12

us_topic_labels <- c(
  "US01"  = "Currency & Exchange Rate Pressure",
  "US02"  = "WTO Compliance Monitoring 1",
  "US03"  = "Strategic & Economic Dialogue Outcomes",
  "US04"  = "Trump 1.0 Trade Policy",
  "US05"  = "WTO Compliance Diplomacy & Enforcement",
  "US06"  = "Presidential Directive Terminology [Genre]",
  "US07"  = "PNTR & WTO Accession",
  "US08"  = "Liberal-Democratic Normative Standard",
  "US09"  = "Trade Remedy Instruments",
  "US10"  = "Biden Trade Agenda",
  "US11"  = "WTO Compliance Monitoring 2",
  "US12"  = "China's WTO Integration Failure",
  "US13"  = "China's Economic System as Threat",
  "US14"  = "WTO Dispute Settlement",
  "US15"  = "Tariff Genre Topic [Genre]",
  "US16"  = "Trade Engagement with China",
  "US17"  = "China Threat Response",
  "US18"  = "Strategic Industrial & Supply Chain Security",
  "US19"  = "Obama Trade Agenda",
  "US20"  = "Bush Trade Agenda"
)

eu_topic_labels <- c(
  "EU01"  = "Market Access Barrier Monitoring [Genre]",
  "EU02"  = "Critical Raw Materials Supply Chain Security",
  "EU03"  = "EU as Defender of Rules-Based Order",
  "EU04"  = "EU-China Cooperative Bilateral Diplomacy",
  "EU05"  = "Multilateral Integration Governance",
  "EU06"  = "Bilateral Trade & Investment Conditions",
  "EU07"  = "De-Risking",
  "EU08"  = "Trade Defence Instruments",
  "EU09"  = "Trade Policy Strategy",
  "EU10"  = "Liberal Engagement Advocacy",
  "EU11"  = "Economic Security",
  "EU12"  = "TIBR Market Access Reports [Genre]",
  "EU13"  = "Liberal Integration Framework",
  "EU14"  = "Values-Based Engagement",
  "EU15"  = "Civilisational Rhetoric"
)

# =============================================================================
# REGISTER GROUPINGS
# Used by: overlay plots (Section 9), prevalence metrics (Section 10)
# Must contain only the four substantive registers.
# Do NOT add plot-only groups here.
# =============================================================================

register_groups <- list(
  "Liberal_Integration" = list(
    us = c(2, 5, 7, 8, 11, 12, 16),
    eu = c(5, 10, 13, 14)
  ),
  "Bilateral_Management" = list(
    us = c(1, 3, 9, 14),
    eu = c(4, 6, 8)
  ),
  "Economic_Security" = list(
    us = c(13, 17, 18),
    eu = c(2, 7, 11)
  ),
  "Self_Conception" = list(
    us = c(4, 10, 19, 20),
    eu = c(3, 9, 15)
  )
)

# Alias kept for any downstream code that references spline_groups directly
spline_groups <- register_groups

# =============================================================================
# OVERLAY CONFIGURATION
# =============================================================================

# Toggle: Show confidence intervals in topic-level overlays?
show_topic_overlay_ci <- FALSE

# Colours for register-level overlay plots.
# Must match the cleaned register names produced by clean_register_name().
register_colours <- c(
  "Liberal Integration"  = "#4A7FB5",
  "Bilateral Management" = "#C4956A",
  "Economic Security"    = "#A63228",
  "Self-Conception"      = "#5A7A6A"
)

# Overlay plot specifications — each entry produces one output file.
#
# TWO TYPES:
#
# type = "register"
#   Aggregates theta across all topics assigned to each named register
#   (drawn from register_groups). One loess-smoothed line per register.
#   Fields:
#     corpus     — "us", "eu", or "both"
#     registers  — character vector of spline_groups keys to include
#     loess_span — (optional) smoothing span, default 0.65
#
# type = "topic"
#   Plots individual topic spline estimates on a single shared set of axes.
#   One spline line with CI ribbon per topic.
#   Fields:
#     corpus — "us", "eu", or "both"
#     us     — integer vector of US topic numbers (ignored if corpus = "eu")
#     eu     — integer vector of EU topic numbers (ignored if corpus = "us")

overlay_specs <- list(
  
  # ══════════════════════════════════════════════════════════════════════════
  # REGISTER-LEVEL OVERLAYS (by corpus)
  # ══════════════════════════════════════════════════════════════════════════
  
  # ── EU: All four registers ─────────────────────────────────────────────────
  "EU_Registers_All" = list(
    type      = "register",
    corpus    = "eu",
    registers = c("Liberal_Integration", "Bilateral_Management",
                  "Economic_Security", "Self_Conception")
  ),
  
  # ── US: All four registers ─────────────────────────────────────────────────
  "US_Registers_All" = list(
    type      = "register",
    corpus    = "us",
    registers = c("Liberal_Integration", "Bilateral_Management",
                  "Economic_Security", "Self_Conception")
  ),
  
  # ── EU: Three registers (excluding Self-Conception) ────────────────────────
  "EU_Registers_Transformation" = list(
    type      = "register",
    corpus    = "eu",
    registers = c("Liberal_Integration", "Bilateral_Management", "Economic_Security")
  ),
  
  # ── US: Three registers (excluding Self-Conception) ────────────────────────
  "US_Registers_Transformation" = list(
    type      = "register",
    corpus    = "us",
    registers = c("Liberal_Integration", "Bilateral_Management", "Economic_Security")
  ),
  
  # ── Both corpora: Three registers showing parallel transformation ──────────
  "Cross_Corpus_Transformation" = list(
    type      = "register",
    corpus    = "both",
    registers = c("Liberal_Integration", "Bilateral_Management", "Economic_Security")
  ),
  
  # ══════════════════════════════════════════════════════════════════════════
  # CROSS-CORPUS REGISTER COMPARISONS
  # ══════════════════════════════════════════════════════════════════════════
  # Individual register overlays showing parallel movements across US and EU
  
  # ── Liberal Integration register: parallel decline ─────────────────────────
  "Liberal_Integration_Cross_Corpus" = list(
    type      = "register",
    corpus    = "both",
    registers = c("Liberal_Integration")
  ),
  
  # ── Economic Security register: parallel rise ──────────────────────────────
  # Note: May show temporal lag with US articulation preceding EU
  "Economic_Security_Cross_Corpus" = list(
    type      = "register",
    corpus    = "both",
    registers = c("Economic_Security")
  ),
  
  # ── Bilateral Management register: parallel evolution ──────────────────────
  "Bilateral_Management_Cross_Corpus" = list(
    type      = "register",
    corpus    = "both",
    registers = c("Bilateral_Management")
  ),
  
  # ══════════════════════════════════════════════════════════════════════════
  # TOPIC-LEVEL OVERLAYS
  # ══════════════════════════════════════════════════════════════════════════
  
  # ── US compliance-to-threat progression: US02 → US11 → US12 → US13 ────────
  "US_Progression" = list(
    type   = "topic",
    corpus = "us",
    us     = c(2, 11, 12, 13)
  )
)

# =============================================================================
# SETUP
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

output_dir    <- here("04_STM_Analysis")
output_dir_us <- here("04_STM_Analysis", "US")
output_dir_eu <- here("04_STM_Analysis", "EU")

dir.create(output_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_us, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_eu, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# LOAD SAVED MODELS
# =============================================================================

if (analyze_us) {
  message("\nLoading US model...")
  stm_us      <- readRDS(here("03_STM_Outputs", "US", "us_stm_model_k20.rds"))
  out_us      <- readRDS(here("03_STM_Outputs", "US", "us_stm_preprocessed.rds"))
  effects_us  <- readRDS(here("03_STM_Outputs", "US", "us_effects_year.rds"))
  k_us        <- ncol(stm_us$theta)
}

if (analyze_eu) {
  message("\nLoading EU model...")
  stm_eu      <- readRDS(here("03_STM_Outputs", "EU", "eu_stm_model_k15.rds"))
  out_eu      <- readRDS(here("03_STM_Outputs", "EU", "eu_stm_preprocessed.rds"))
  effects_eu  <- readRDS(here("03_STM_Outputs", "EU", "eu_effects_year.rds"))
  k_eu        <- ncol(stm_eu$theta)
}

# =============================================================================
# 1. TOPIC CORRELATIONS (topicCorr)
# =============================================================================

message("\n=== TOPIC CORRELATIONS ===")

extract_topiccorr <- function(model, k, threshold = 0.10, corpus_prefix = "") {
  tc      <- topicCorr(model, method = "simple", cutoff = threshold)
  cor_mat <- tc$cor
  if (corpus_prefix != "") {
    colnames(cor_mat) <- rownames(cor_mat) <- paste0(corpus_prefix, sprintf("%02d", 1:k))
  } else {
    colnames(cor_mat) <- rownames(cor_mat) <- paste0("T", 1:k)
  }
  
  pairs <- data.frame()
  for (i in 1:(k - 1)) {
    for (j in (i + 1):k) {
      r <- cor_mat[i, j]
      if (abs(r) > threshold) {
        pairs <- rbind(pairs, data.frame(
          Topic1      = colnames(cor_mat)[i],
          Topic2      = colnames(cor_mat)[j],
          Correlation = round(r, 3),
          Type        = ifelse(r > 0, "Positive", "Negative")
        ))
      }
    }
  }
  pairs <- pairs %>% arrange(desc(abs(Correlation)))
  
  per_topic <- lapply(1:k, function(t) {
    topic   <- colnames(cor_mat)[t]
    matches <- pairs %>% filter(Topic1 == topic | Topic2 == topic)
    if (nrow(matches) > 0) {
      cors <- apply(matches, 1, function(row) {
        other <- ifelse(row["Topic1"] == topic, row["Topic2"], row["Topic1"])
        paste0(other, " (", sprintf("%+.2f", as.numeric(row["Correlation"])), ")")
      })
      return(paste(cors, collapse = "; "))
    } else {
      return("None >0.10")
    }
  })
  names(per_topic) <- colnames(cor_mat)
  
  return(list(tc = tc, matrix = cor_mat, pairs = pairs, per_topic = per_topic))
}

if (analyze_us) {
  us_corr <- extract_topiccorr(stm_us, k_us, threshold = corr_cutoff, corpus_prefix = "US")
  write.csv(us_corr$matrix,
            file.path(output_dir_us, "us_correlation_matrix.csv"))
  write_csv(us_corr$pairs,
            file.path(output_dir_us, "us_notable_correlations.csv"))
  write_csv(
    data.frame(Topic = names(us_corr$per_topic),
               Correlations = unlist(us_corr$per_topic)),
    file.path(output_dir_us, "us_correlations_per_topic.csv")
  )
  us_labels <- if (!is.null(us_topic_labels)) us_topic_labels else paste("Topic", 1:k_us)
  png(file.path(output_dir_us, "us_topic_correlations_network.png"),
      width = 900, height = 900)
  plot(us_corr$tc, topics = 1:k_us, vlabels = us_labels,
       vertex.color = "steelblue", main = "US Topic Correlations")
  dev.off()
  message("US: ", nrow(us_corr$pairs), " notable correlation pairs (|r| > 0.10)")
}

if (analyze_eu) {
  eu_corr <- extract_topiccorr(stm_eu, k_eu, threshold = corr_cutoff, corpus_prefix = "EU")
  write.csv(eu_corr$matrix,
            file.path(output_dir_eu, "eu_correlation_matrix.csv"))
  write_csv(eu_corr$pairs,
            file.path(output_dir_eu, "eu_notable_correlations.csv"))
  write_csv(
    data.frame(Topic = names(eu_corr$per_topic),
               Correlations = unlist(eu_corr$per_topic)),
    file.path(output_dir_eu, "eu_correlations_per_topic.csv")
  )
  eu_labels <- if (!is.null(eu_topic_labels)) eu_topic_labels else paste("Topic", 1:k_eu)
  png(file.path(output_dir_eu, "eu_topic_correlations_network.png"),
      width = 900, height = 900)
  plot(eu_corr$tc, topics = 1:k_eu, vlabels = eu_labels,
       vertex.color = "darkred", main = "EU Topic Correlations")
  dev.off()
  message("EU: ", nrow(eu_corr$pairs), " notable correlation pairs (|r| > 0.10)")
}

# =============================================================================
# 2. SPLINE TEMPORAL EFFECTS
# =============================================================================

message("\n=== SPLINE TEMPORAL EFFECTS ===")

if (analyze_us) {
  message("US: Estimating spline effects...")
  effects_us_spline <- estimateEffect(
    1:k_us ~ s(year),
    stmobj      = stm_us,
    metadata    = out_us$meta,
    uncertainty = "Global"
  )
  saveRDS(effects_us_spline,
          here("03_STM_Outputs", "US", "us_effects_spline.rds"))
  
  png(file.path(output_dir_us, "us_year_trends_spline_all.png"),
      width = 1400, height = 1000)
  par(mfrow = c(4, 5), mar = c(4, 4, 2, 1))
  for (i in 1:k_us) {
    plot(effects_us_spline, covariate = "year", topics = i,
         method = "continuous", main = paste0("US", sprintf("%02d", i)),
         xlab = "Year", ylab = "Expected Proportion")
  }
  dev.off()
  
  message("US spline effects complete.")
}

if (analyze_eu) {
  message("EU: Estimating spline effects...")
  effects_eu_spline <- estimateEffect(
    1:k_eu ~ s(year),
    stmobj      = stm_eu,
    metadata    = out_eu$meta,
    uncertainty = "Global"
  )
  saveRDS(effects_eu_spline,
          here("03_STM_Outputs", "EU", "eu_effects_spline.rds"))
  
  png(file.path(output_dir_eu, "eu_year_trends_spline_all.png"),
      width = 1200, height = 800)
  par(mfrow = c(3, 5), mar = c(4, 4, 2, 1))
  for (i in 1:k_eu) {
    plot(effects_eu_spline, covariate = "year", topics = i,
         method = "continuous", main = paste0("EU", sprintf("%02d", i)),
         xlab = "Year", ylab = "Expected Proportion")
  }
  dev.off()
  
  message("EU spline effects complete.")
}

# =============================================================================
# 3. REPRESENTATIVE DOCUMENTS
# =============================================================================

message("\n=== REPRESENTATIVE DOCUMENTS ===")

get_representative_docs <- function(model, out, k, n = 15, corpus_prefix = "") {
  map_dfr(1:k, function(t) {
    thoughts    <- findThoughts(model, texts = out$meta$file_name,
                                n = n, topics = t)
    doc_indices <- thoughts$index[[1]]
    topic_label <- if (corpus_prefix != "") {
      paste0(corpus_prefix, sprintf("%02d", t))
    } else {
      t
    }
    data.frame(
      Topic            = topic_label,
      Rank             = seq_along(doc_indices),
      File             = out$meta$file_name[doc_indices],
      Year             = out$meta$year[doc_indices],
      Title            = out$meta$title[doc_indices],
      Theta            = round(model$theta[doc_indices, t], 3),
      stringsAsFactors = FALSE
    )
  })
}

if (analyze_us) {
  us_rep_docs <- get_representative_docs(stm_us, out_us, k_us, n = 15, corpus_prefix = "US")
  write_csv(us_rep_docs, file.path(output_dir_us, "us_representative_docs.csv"))
  message("US representative documents saved (", nrow(us_rep_docs), " rows).")
}

if (analyze_eu) {
  eu_rep_docs <- get_representative_docs(stm_eu, out_eu, k_eu, n = 15, corpus_prefix = "EU")
  write_csv(eu_rep_docs, file.path(output_dir_eu, "eu_representative_docs.csv"))
  message("EU representative documents saved (", nrow(eu_rep_docs), " rows).")
}

# =============================================================================
# 4. YEARLY TOPIC PROPORTIONS
# =============================================================================

message("\n=== YEARLY TOPIC PROPORTIONS ===")

compute_yearly_props <- function(model, meta, k, corpus_prefix = "") {
  df <- as.data.frame(model$theta)
  if (corpus_prefix != "") {
    colnames(df) <- paste0(corpus_prefix, sprintf("%02d", 1:k))
  } else {
    colnames(df) <- paste0("T", 1:k)
  }
  df$year <- meta$year
  prefix_pattern <- if (corpus_prefix != "") corpus_prefix else "T"
  df %>%
    group_by(year) %>%
    summarise(across(starts_with(prefix_pattern), mean), n_docs = n(), .groups = "drop") %>%
    arrange(year)
}

if (analyze_us) {
  us_yearly <- compute_yearly_props(stm_us, out_us$meta, k_us, corpus_prefix = "US")
  write_csv(us_yearly, file.path(output_dir_us, "us_yearly_topic_proportions.csv"))
  message("US yearly proportions saved.")
}

if (analyze_eu) {
  eu_yearly <- compute_yearly_props(stm_eu, out_eu$meta, k_eu, corpus_prefix = "EU")
  write_csv(eu_yearly, file.path(output_dir_eu, "eu_yearly_topic_proportions.csv"))
  message("EU yearly proportions saved.")
}

# =============================================================================
# 5. TRAJECTORY ANALYSIS
# =============================================================================

message("\n=== TRAJECTORY ANALYSIS ===")

analyze_trajectory <- function(yearly_props, topic_col, min_change = 0.02) {
  props <- yearly_props[[topic_col]]
  years <- yearly_props$year
  if (length(props) >= 5) {
    lo       <- loess(props ~ years, span = 0.5)
    smoothed <- predict(lo, years)
  } else {
    smoothed <- props
  }
  first_deriv      <- diff(smoothed)
  sign_changes     <- which(diff(sign(first_deriv)) != 0)
  inflection_years <- years[sign_changes + 1]
  significant_inflections <- c()
  for (yr in inflection_years) {
    idx <- which(years == yr)
    if (idx > 1 && idx < length(props)) {
      before_change <- abs(smoothed[idx] - smoothed[max(1, idx - 2)])
      after_change  <- abs(smoothed[min(length(props), idx + 2)] - smoothed[idx])
      if (before_change > min_change | after_change > min_change)
        significant_inflections <- c(significant_inflections, yr)
    }
  }
  start_val      <- mean(smoothed[1:min(3, length(smoothed))])
  end_val        <- mean(smoothed[max(1, length(smoothed) - 2):length(smoothed)])
  mid_idx        <- round(length(smoothed) / 2)
  mid_val        <- mean(smoothed[max(1, mid_idx - 1):min(length(smoothed), mid_idx + 1)])
  overall_change <- end_val - start_val
  if (abs(overall_change) < min_change) {
    shape <- "Stable"
  } else if (overall_change > 0) {
    if (mid_val > end_val && mid_val > start_val) shape <- "Inverted-U"
    else if (mid_val < start_val && mid_val < end_val) shape <- "U-shaped"
    else if (length(significant_inflections) > 1) shape <- "Rising (Non-linear)"
    else shape <- "Rising (Linear)"
  } else {
    if (mid_val > end_val && mid_val > start_val) shape <- "Inverted-U"
    else if (mid_val < start_val && mid_val < end_val) shape <- "U-shaped"
    else if (length(significant_inflections) > 1) shape <- "Declining (Non-linear)"
    else shape <- "Declining (Linear)"
  }
  list(shape = shape, inflection_years = significant_inflections,
       start_val = round(start_val, 4), end_val = round(end_val, 4),
       change = round(overall_change, 4))
}

if (analyze_us) {
  us_trajectories <- map_dfr(1:k_us, function(t) {
    result <- analyze_trajectory(us_yearly, paste0("US", sprintf("%02d", t)))
    data.frame(Topic = paste0("US", sprintf("%02d", t)), Trajectory_Shape = result$shape,
               Inflection_Points = ifelse(length(result$inflection_years) > 0,
                                          paste(result$inflection_years, collapse = "; "), "None"),
               Start_Proportion = result$start_val, End_Proportion = result$end_val,
               Total_Change = result$change)
  })
  write_csv(us_trajectories, file.path(output_dir_us, "us_trajectory_analysis.csv"))
  message("US trajectory analysis complete.")
}

if (analyze_eu) {
  eu_trajectories <- map_dfr(1:k_eu, function(t) {
    result <- analyze_trajectory(eu_yearly, paste0("EU", sprintf("%02d", t)))
    data.frame(Topic = paste0("EU", sprintf("%02d", t)), Trajectory_Shape = result$shape,
               Inflection_Points = ifelse(length(result$inflection_years) > 0,
                                          paste(result$inflection_years, collapse = "; "), "None"),
               Start_Proportion = result$start_val, End_Proportion = result$end_val,
               Total_Change = result$change)
  })
  write_csv(eu_trajectories, file.path(output_dir_eu, "eu_trajectory_analysis.csv"))
  message("EU trajectory analysis complete.")
}

# =============================================================================
# 6. COEFFICIENT OF VARIATION (CV)
# =============================================================================

message("\n=== COEFFICIENT OF VARIATION ===")

compute_cv <- function(model, k, corpus_prefix = "") {
  theta <- model$theta
  if (corpus_prefix != "") {
    topic_labels <- paste0(corpus_prefix, sprintf("%02d", 1:k))
  } else {
    topic_labels <- paste0("T", 1:k)
  }
  data.frame(Topic = topic_labels,
             Mean_Prop = colMeans(theta),
             SD_Prop   = apply(theta, 2, sd)) %>%
    mutate(CV = SD_Prop / Mean_Prop, Rank = rank(CV)) %>%
    arrange(CV)
}

if (analyze_us) {
  us_cv <- compute_cv(stm_us, k_us, corpus_prefix = "US")
  write_csv(us_cv, file.path(output_dir_us, "us_topic_cv.csv"))
  message("US CV range: ", round(min(us_cv$CV), 1), " - ", round(max(us_cv$CV), 1))
  message("US most pervasive topics (lowest CV): ",
          paste(head(us_cv$Topic, 5), collapse = ", "))
}

if (analyze_eu) {
  eu_cv <- compute_cv(stm_eu, k_eu, corpus_prefix = "EU")
  write_csv(eu_cv, file.path(output_dir_eu, "eu_topic_cv.csv"))
  message("EU CV range: ", round(min(eu_cv$CV), 1), " - ", round(max(eu_cv$CV), 1))
  message("EU most pervasive topics (lowest CV): ",
          paste(head(eu_cv$Topic, 5), collapse = ", "))
}

# =============================================================================
# 7. SUMMARY STATISTICS
# =============================================================================

message("\n=== CORPUS SUMMARY ===")

summary_stats <- data.frame(
  Corpus     = c("US", "EU"),
  Documents  = c(ifelse(analyze_us, length(out_us$documents), NA),
                 ifelse(analyze_eu, length(out_eu$documents), NA)),
  Vocabulary = c(ifelse(analyze_us, length(out_us$vocab), NA),
                 ifelse(analyze_eu, length(out_eu$vocab), NA)),
  Topics     = c(ifelse(analyze_us, k_us, NA),
                 ifelse(analyze_eu, k_eu, NA)),
  Year_Range = c(ifelse(analyze_us, paste(range(out_us$meta$year), collapse = "-"), NA),
                 ifelse(analyze_eu, paste(range(out_eu$meta$year), collapse = "-"), NA))
)
write_csv(summary_stats, file.path(output_dir, "corpus_summary_stats.csv"))
print(summary_stats)

# =============================================================================
# 8. GGPLOT2 SPLINE TREND PLOTS
# =============================================================================

message("\n=== GGPLOT2 SPLINE TREND PLOTS ===")

library(scales)

# Corpus colours
col_us <- "#B25751"
col_eu <- "#1F497D"

# ── Helper: extract spline predictions ───────────────────────────────────────
extract_spline_preds <- function(effects_obj, topics, corpus_prefix = NULL) {
  map_dfr(topics, function(t) {
    pdf(nullfile())
    pred <- plot(effects_obj,
                 covariate = "year",
                 topics    = t,
                 method    = "continuous",
                 npoints   = 200,
                 ci.level  = 0.95)
    dev.off()
    topic_id <- paste0("T", t)
    label_id <- if (!is.null(corpus_prefix))
      paste0(corpus_prefix, " ", topic_id) else topic_id
    data.frame(
      topic    = label_id,
      corpus   = if (!is.null(corpus_prefix)) corpus_prefix else "single",
      year     = pred$x,
      estimate = pred$means[[1]],
      ci_lower = pred$ci[[1]]["2.5%",  ],
      ci_upper = pred$ci[[1]]["97.5%", ]
    )
  })
}

# ── Core plotting function ────────────────────────────────────────────────────
render_spline_plot <- function(preds, ordered_labels, colour_map,
                               output_path, ncols = 4,
                               year_range, group_name = "") {
  preds <- preds %>%
    mutate(label = factor(topic, levels = ordered_labels))
  
  mixed <- length(unique(preds$corpus)) > 1
  
  p <- ggplot(preds, aes(x = year, group = topic,
                         colour = if (mixed) corpus else NULL)) +
    geom_line(aes(y = ci_lower), linewidth = 0.4, linetype = "dashed",
              colour = if (!mixed) colour_map[unique(preds$corpus)] else NULL) +
    geom_line(aes(y = ci_upper), linewidth = 0.4, linetype = "dashed",
              colour = if (!mixed) colour_map[unique(preds$corpus)] else NULL) +
    geom_line(aes(y = estimate), linewidth = 0.7,
              colour = if (!mixed) colour_map[unique(preds$corpus)] else NULL) +
    facet_wrap(~ label, ncol = ncols, scales = "free_y") +
    scale_x_continuous(breaks = seq(year_range[1], year_range[2], by = 4)) +
    scale_y_continuous(labels = label_number(accuracy = 0.01),
                       expand = expansion(mult = c(0.02, 0.05))) +
    coord_cartesian(ylim = c(-0.02, NA)) +
    labs(x = "Year", y = "Expected Topic Proportion",
         colour = if (mixed) "Corpus" else NULL) +
    theme_bw(base_family = "serif", base_size = 10) +
    theme(
      strip.background = element_rect(fill = "#f0f0f0", colour = "grey70"),
      strip.text       = element_text(size = 8, face = "bold", family = "serif"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y      = element_text(size = 7),
      axis.title       = element_text(size = 9),
      legend.position  = if (mixed) "bottom" else "none"
    )
  
  if (mixed) {
    p <- p + scale_colour_manual(values = colour_map)
  }
  
  n_rows <- ceiling(length(unique(preds$topic)) / ncols)
  suppressWarnings(
    ggsave(output_path, p,
           width  = ncols * 3.2,
           height = n_rows * 2.4,
           units  = "in", dpi = 200)
  )
  message("Spline plot saved: ", basename(output_path),
          if (nchar(group_name) > 0) paste0(" [", group_name, "]") else "")
}

# ── Mixed-corpus version ──────────────────────────────────────────────────────
render_spline_plot_mixed <- function(preds, ordered_labels, colour_map,
                                     output_path, ncols = 4,
                                     year_range, group_name = "") {
  preds <- preds %>%
    mutate(label = factor(topic, levels = ordered_labels))
  
  p <- ggplot(preds, aes(x = year, group = topic, colour = corpus)) +
    geom_line(aes(y = ci_lower), linewidth = 0.4, linetype = "dashed") +
    geom_line(aes(y = ci_upper), linewidth = 0.4, linetype = "dashed") +
    geom_line(aes(y = estimate), linewidth = 0.7) +
    facet_wrap(~ label, ncol = ncols, scales = "free_y") +
    scale_colour_manual(values = colour_map) +
    scale_x_continuous(breaks = seq(year_range[1], year_range[2], by = 4)) +
    scale_y_continuous(labels = label_number(accuracy = 0.01),
                       expand = expansion(mult = c(0.02, 0.05))) +
    coord_cartesian(ylim = c(-0.02, NA)) +
    labs(x = "Year", y = "Expected Topic Proportion", colour = "Corpus") +
    theme_bw(base_family = "serif", base_size = 10) +
    theme(
      strip.background = element_rect(fill = "#f0f0f0", colour = "grey70"),
      strip.text       = element_text(size = 8, face = "bold", family = "serif"),
      panel.grid.minor = element_blank(),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 7),
      axis.text.y      = element_text(size = 7),
      axis.title       = element_text(size = 9),
      legend.position  = "none"
    )
  
  n_rows <- ceiling(length(unique(preds$topic)) / ncols)
  suppressWarnings(
    ggsave(output_path, p,
           width  = ncols * 3.2,
           height = n_rows * 2.4,
           units  = "in", dpi = 200)
  )
  message("Spline plot saved: ", basename(output_path),
          if (nchar(group_name) > 0) paste0(" [", group_name, "]") else "")
}

# ── Label builder helpers ─────────────────────────────────────────────────────
build_labels_single <- function(topics, labels_vec, corpus_prefix = NULL) {
  base <- paste0(corpus_prefix, sprintf("%02d", topics))
  paste0(base, ": ", labels_vec[base])
}

# ── Run: default or grouped ───────────────────────────────────────────────────

colour_map <- c("US" = col_us, "EU" = col_eu)
year_range <- range(c(if (analyze_us) out_us$meta$year,
                      if (analyze_eu) out_eu$meta$year))

# ── Always produce default corpus plots ──────────────────────────────────────
if (analyze_us) {
  us_sub  <- setdiff(1:k_us, us_genre_topics)
  preds   <- extract_spline_preds(effects_us_spline, us_sub)
  ol      <- build_labels_single(us_sub, us_topic_labels, "US")
  preds$topic <- build_labels_single(
    as.integer(sub(".*T", "", preds$topic)), us_topic_labels, "US")
  render_spline_plot(
    preds, ol, c(single = col_us),
    file.path(output_dir_us, "us_spline_gg.png"),
    ncols = 4, year_range = range(out_us$meta$year)
  )
}

if (analyze_eu) {
  eu_sub  <- setdiff(1:k_eu, eu_genre_topics)
  preds   <- extract_spline_preds(effects_eu_spline, eu_sub)
  ol      <- build_labels_single(eu_sub, eu_topic_labels, "EU")
  preds$topic <- build_labels_single(
    as.integer(sub(".*T", "", preds$topic)), eu_topic_labels, "EU")
  render_spline_plot(
    preds, ol, c(single = col_eu),
    file.path(output_dir_eu, "eu_spline_gg.png"),
    ncols = 4, year_range = range(out_eu$meta$year)
  )
}

# ── Grouped plots (if defined) ────────────────────────────────────────────────
if (!is.null(register_groups)) {
  dir.create(file.path(output_dir, "Grouped_Splines"),
             showWarnings = FALSE, recursive = TRUE)
  
  for (grp_name in names(register_groups)) {
    grp      <- register_groups[[grp_name]]
    has_us   <- !is.null(grp$us) && length(grp$us) > 0
    has_eu   <- !is.null(grp$eu) && length(grp$eu) > 0
    mixed    <- has_us && has_eu
    
    all_preds <- list()
    
    if (has_us) {
      p_us <- extract_spline_preds(
        effects_us_spline, grp$us,
        corpus_prefix = if (mixed) "US" else NULL)
      # Always relabel row-by-row to avoid recycling over 200-point spline data
      for (i in seq_along(grp$us)) {
        old_label <- if (mixed) paste0("US T", grp$us[i]) else paste0("T", grp$us[i])
        new_label <- paste0("US", sprintf("%02d", grp$us[i]), ": ",
                            us_topic_labels[paste0("US", sprintf("%02d", grp$us[i]))])
        p_us$topic[p_us$topic == old_label] <- new_label
      }
      all_preds[["us"]] <- p_us
    }
    
    if (has_eu) {
      p_eu <- extract_spline_preds(
        effects_eu_spline, grp$eu,
        corpus_prefix = if (mixed) "EU" else NULL)
      # Always relabel row-by-row to avoid recycling over 200-point spline data
      for (i in seq_along(grp$eu)) {
        old_label <- if (mixed) paste0("EU T", grp$eu[i]) else paste0("T", grp$eu[i])
        new_label <- paste0("EU", sprintf("%02d", grp$eu[i]), ": ",
                            eu_topic_labels[paste0("EU", sprintf("%02d", grp$eu[i]))])
        p_eu$topic[p_eu$topic == old_label] <- new_label
      }
      all_preds[["eu"]] <- p_eu
    }
    
    preds <- bind_rows(all_preds)
    
    ol <- c(
      if (has_us) {
        if (mixed) paste0("US", sprintf("%02d", grp$us), ": ",
                          us_topic_labels[paste0("US", sprintf("%02d", grp$us))])
        else build_labels_single(grp$us, us_topic_labels, "US")
      },
      if (has_eu) {
        if (mixed) paste0("EU", sprintf("%02d", grp$eu), ": ",
                          eu_topic_labels[paste0("EU", sprintf("%02d", grp$eu))])
        else build_labels_single(grp$eu, eu_topic_labels, "EU")
      }
    )
    
    safe_name <- gsub("[^A-Za-z0-9_-]", "_", grp_name)
    out_path  <- file.path(output_dir, "Grouped_Splines",
                           paste0(safe_name, "_spline.png"))
    n_topics  <- length(unique(preds$topic))
    ncols     <- min(4, n_topics)
    
    if (mixed) {
      render_spline_plot_mixed(preds, ol, colour_map,
                               out_path, ncols = ncols,
                               year_range = year_range,
                               group_name = grp_name)
    } else {
      corp_col <- if (has_us) c(single = col_us) else c(single = col_eu)
      render_spline_plot(preds, ol, corp_col,
                         out_path, ncols = ncols,
                         year_range = year_range,
                         group_name = grp_name)
    }
  }
}

# =============================================================================
# 9. AGGREGATE REGISTER & TOPIC OVERLAY PLOTS
# =============================================================================

message("\n=== OVERLAY PLOTS ===")

overlay_dir <- file.path(output_dir, "Overlay_Plots")
dir.create(overlay_dir, showWarnings = FALSE, recursive = TRUE)

# ── Clean register names for legend ──────────────────────────────────────────
clean_register_name <- function(x) {
  case_when(
    x == "Liberal_Integration"  ~ "Liberal Integration",
    x == "Bilateral_Management" ~ "Bilateral Management",
    x == "Economic_Security"    ~ "Economic Security",
    x == "Self_Conception"      ~ "Self-Conception",
    TRUE                        ~ x
  )
}

# =============================================================================
# HELPER: compute register-level yearly aggregates
# =============================================================================

compute_register_yearly <- function(model, meta, k, groups, corpus_side,
                                    corpus_prefix, register_subset = NULL,
                                    genre_topics = NULL) {
  # Uses raw theta (no genre rescaling). Register prevalence is reported
  # as the share of total corpus topical content, consistent with the
  # formal definition theta_{R,Y} = sum_{k in T_R} theta_bar_{k,Y} where
  # theta_{d,k} satisfies sum_k theta_{d,k} = 1 over all K topics.
  # Genre topics are excluded from registers (via spline_groups membership)
  # but retain their corpus weight; the four registers therefore sum to
  # (1 - genre share) within each year, not to 1.
  # The genre_topics argument is retained for interface compatibility but
  # is no longer used to rescale theta.
  theta <- as.data.frame(model$theta)
  colnames(theta) <- paste0(corpus_prefix, sprintf("%02d", 1:k))
  theta$year <- meta$year
  
  regs_to_use <- if (!is.null(register_subset)) register_subset else names(groups)
  
  map_dfr(regs_to_use, function(rname) {
    if (!rname %in% names(groups)) return(NULL)
    topic_nums <- groups[[rname]][[corpus_side]]
    if (is.null(topic_nums) || length(topic_nums) == 0) return(NULL)
    topic_ids <- paste0(corpus_prefix, sprintf("%02d", topic_nums))
    valid_ids <- intersect(topic_ids, colnames(theta))
    theta %>%
      group_by(year) %>%
      summarise(
        mean_prop = mean(rowSums(across(all_of(valid_ids)))),
        .groups   = "drop"
      ) %>%
      mutate(series = paste0(corpus_prefix, ": ", clean_register_name(rname)))
  })
}

# =============================================================================
# HELPER: build topic-level spline data for overlay (single-axis, no facets)
# =============================================================================

build_topic_overlay_data <- function(spec) {
  parts <- list()
  
  if (spec$corpus %in% c("us", "both") && !is.null(spec$us) && analyze_us) {
    df <- map_dfr(spec$us, function(t) {
      pdf(nullfile())
      pred <- plot(effects_us_spline, covariate = "year", topics = t,
                   method = "continuous", npoints = 200, ci.level = 0.95)
      dev.off()
      tid <- paste0("US", sprintf("%02d", t))
      data.frame(
        year     = pred$x,
        estimate = pred$means[[1]],
        ci_lower = pred$ci[[1]]["2.5%",  ],
        ci_upper = pred$ci[[1]]["97.5%", ],
        series   = paste0(tid, ": ", us_topic_labels[tid]),
        corpus   = "US"
      )
    })
    parts[["us"]] <- df
  }
  
  if (spec$corpus %in% c("eu", "both") && !is.null(spec$eu) && analyze_eu) {
    df <- map_dfr(spec$eu, function(t) {
      pdf(nullfile())
      pred <- plot(effects_eu_spline, covariate = "year", topics = t,
                   method = "continuous", npoints = 200, ci.level = 0.95)
      dev.off()
      tid <- paste0("EU", sprintf("%02d", t))
      data.frame(
        year     = pred$x,
        estimate = pred$means[[1]],
        ci_lower = pred$ci[[1]]["2.5%",  ],
        ci_upper = pred$ci[[1]]["97.5%", ],
        series   = paste0(tid, ": ", eu_topic_labels[tid]),
        corpus   = "EU"
      )
    })
    parts[["eu"]] <- df
  }
  
  bind_rows(parts)
}

# =============================================================================
# CORE RENDER FUNCTION — handles both register and topic overlays
# =============================================================================

render_overlay <- function(plot_data, spec_name, spec,
                           reg_cols   = register_colours,
                           yr_range   = year_range,
                           loess_span = 0.65,
                           output_path) {
  
  is_register <- spec$type == "register"
  
  # Check if this is truly cross-corpus (has both US and EU series)
  series_names    <- unique(plot_data$series)
  has_us_series   <- any(grepl("^US:", series_names))
  has_eu_series   <- any(grepl("^EU:", series_names))
  is_cross_corpus <- has_us_series && has_eu_series
  
  if (is_register) {
    if (is_cross_corpus) {
      # For cross-corpus: color by corpus, keep full names
      series_levels <- series_names
      corpus_cols   <- c("US" = "#B25751", "EU" = "#1F497D")
      colour_vals   <- setNames(
        ifelse(grepl("^US:", series_levels), corpus_cols["US"], corpus_cols["EU"]),
        series_levels
      )
    } else {
      # For single-corpus: color by register, strip prefix for matching
      clean_series  <- gsub("^(US|EU): ", "", series_names)
      matched       <- clean_series[clean_series %in% names(reg_cols)]
      # Map colors back to original names with prefix
      colour_vals   <- setNames(
        reg_cols[matched],
        series_names[clean_series %in% names(reg_cols)]
      )
      series_levels <- names(colour_vals)
    }
  } else {
    series_levels <- unique(plot_data$series)
    n_series      <- length(series_levels)
    base_palette  <- c(
      "#4A7FB5", "#A63228", "#C4956A", "#5A8A5A",
      "#7B5EA7", "#4A9A8A", "#B25751", "#1F497D"
    )
    colour_vals   <- setNames(rep_len(base_palette, n_series), series_levels)
  }
  
  plot_data <- plot_data %>%
    mutate(series = factor(series, levels = series_levels))
  
  p <- ggplot(plot_data, aes(x = year, colour = series,
                             fill = series, group = series))
  
  if (is_register) {
    p <- p +
      geom_smooth(aes(y = mean_prop),
                  method = "loess", formula = y ~ x,
                  span = loess_span, se = FALSE,
                  linewidth = 0.9)
    y_label <- "Mean Aggregate Topic Proportion"
  } else {
    # Topic-level overlay
    if (show_topic_overlay_ci) {
      p <- p +
        geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                    alpha = 0.10, colour = NA) +
        geom_line(aes(y = estimate), linewidth = 0.85)
    } else {
      p <- p +
        geom_line(aes(y = estimate), linewidth = 0.85)
    }
    y_label <- "Expected Topic Proportion"
  }
  
  p <- p +
    scale_colour_manual(values = colour_vals) +
    scale_fill_manual(values   = colour_vals) +
    scale_x_continuous(breaks  = seq(yr_range[1], yr_range[2], by = 4)) +
    scale_y_continuous(
      labels = label_number(accuracy = 0.01),
      expand = expansion(mult = c(0.02, 0.10))
    ) +
    coord_cartesian(ylim = c(0, NA)) +
    guides(colour = guide_legend(title = NULL,
                                 nrow  = if (is_register) 1 else 2),
           fill   = guide_legend(title = NULL,
                                 nrow  = if (is_register) 1 else 2)) +
    labs(x = "Year", y = y_label) +
    theme_bw(base_family = "serif", base_size = 11) +
    theme(
      panel.grid.minor  = element_blank(),
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y       = element_text(size = 8),
      axis.title        = element_text(size = 10),
      legend.position   = "bottom",
      legend.text       = element_text(size = 8),
      legend.key.width  = unit(1.5, "cm"),
      legend.margin     = margin(t = 6, unit = "pt"),
      legend.box.margin = margin(t = 2, unit = "pt")
    )
  
  plot_width  <- if (is_register) 8 else 10
  plot_height <- if (is_register) 5 else 5.5
  ggsave(output_path, p, width = plot_width, height = plot_height,
         units = "in", dpi = 300)
  message("Overlay saved: ", basename(output_path))
  invisible(plot_data)
}

# =============================================================================
# RUN ALL OVERLAY SPECS
# =============================================================================

for (spec_name in names(overlay_specs)) {
  spec      <- overlay_specs[[spec_name]]
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", spec_name)
  out_path  <- file.path(overlay_dir, paste0(safe_name, "_overlay.png"))
  
  if (spec$type == "register") {
    
    parts <- list()
    if (spec$corpus %in% c("us", "both") && analyze_us) {
      parts[["us"]] <- compute_register_yearly(
        stm_us, out_us$meta, k_us,
        groups          = register_groups,
        corpus_side     = "us",
        corpus_prefix   = "US",
        register_subset = spec$registers,
        genre_topics    = us_genre_topics
      )
    }
    if (spec$corpus %in% c("eu", "both") && analyze_eu) {
      parts[["eu"]] <- compute_register_yearly(
        stm_eu, out_eu$meta, k_eu,
        groups          = register_groups,
        corpus_side     = "eu",
        corpus_prefix   = "EU",
        register_subset = spec$registers,
        genre_topics    = eu_genre_topics
      )
    }
    plot_data <- bind_rows(parts)
    span_val  <- if (!is.null(spec$loess_span)) spec$loess_span else 0.65
    render_overlay(plot_data, spec_name, spec,
                   loess_span  = span_val,
                   output_path = out_path)
    write_csv(plot_data,
              file.path(overlay_dir, paste0(safe_name, "_data.csv")))
    
  } else if (spec$type == "topic") {
    
    plot_data <- build_topic_overlay_data(spec)
    render_overlay(plot_data, spec_name, spec,
                   output_path = out_path)
    
  } else {
    warning("Unknown overlay type for spec: ", spec_name)
  }
}

# =============================================================================
# 10. PREVALENCE METRICS
# =============================================================================
# Produces four prevalence quantities:
#   theta_bar_k       Topic mean prevalence:     (1/|D|) * sum_d theta_{d,k}
#   theta_bar_{k,Y}   Topic yearly prevalence:   (1/|D_Y|) * sum_{d in D_Y} theta_{d,k}
#   theta_R           Register mean prevalence:  sum_{k in T_R} theta_bar_k
#   theta_{R,Y}       Register yearly prevalence: sum_{k in T_R} theta_bar_{k,Y}
#
# Computed on raw theta, so corpus-wide topic means
# sum to 1 across all K topics. Genre topics are excluded from registers
# but retain their corpus weight, so the four registers sum to
# (1 - genre share) within each year. This convention matches the formal
# definitions and the (now-unrescaled) register overlay plots in Section 9.

message("\n=== FORMAL PREVALENCE METRICS ===")

compute_prevalence_metrics <- function(model, meta, k, groups, corpus_side,
                                       corpus_prefix, genre_topics) {
  
  theta <- as.data.frame(model$theta)
  topic_ids <- paste0(corpus_prefix, sprintf("%02d", 1:k))
  colnames(theta) <- topic_ids
  theta$year <- meta$year
  
  genre_ids <- if (length(genre_topics) > 0) {
    paste0(corpus_prefix, sprintf("%02d", genre_topics))
  } else character(0)
  substantive_ids <- setdiff(topic_ids, genre_ids)
  
  # ── 1. Topic mean prevalence: theta_bar_k ────────────────────────────────
  topic_mean <- data.frame(
    topic       = topic_ids,
    theta_bar_k = round(colMeans(theta[topic_ids]), 5),
    is_genre    = topic_ids %in% genre_ids,
    row.names   = NULL
  )
  
  # ── 2. Topic yearly prevalence: theta_bar_{k,Y} (long format) ────────────
  topic_yearly <- theta %>%
    select(year, all_of(topic_ids)) %>%
    group_by(year) %>%
    summarise(across(all_of(topic_ids), mean), .groups = "drop") %>%
    pivot_longer(-year, names_to = "topic", values_to = "theta_bar_kY") %>%
    mutate(theta_bar_kY = round(theta_bar_kY, 5))
  
  # ── 3. Register mean prevalence: theta_R = sum_{k in T_R} theta_bar_k ────
  register_mean <- map_dfr(names(groups), function(rname) {
    topic_nums <- groups[[rname]][[corpus_side]]
    if (is.null(topic_nums) || length(topic_nums) == 0) return(NULL)
    rids <- paste0(corpus_prefix, sprintf("%02d", topic_nums))
    rids <- intersect(rids, substantive_ids)
    data.frame(
      register  = clean_register_name(rname),
      theta_R   = round(sum(topic_mean$theta_bar_k[topic_mean$topic %in% rids]), 5),
      n_topics  = length(rids),
      topic_ids = paste(rids, collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  
  # ── 4. Register yearly prevalence: theta_{R,Y} = sum_{k in T_R} theta_bar_{k,Y}
  register_yearly <- map_dfr(names(groups), function(rname) {
    topic_nums <- groups[[rname]][[corpus_side]]
    if (is.null(topic_nums) || length(topic_nums) == 0) return(NULL)
    rids <- paste0(corpus_prefix, sprintf("%02d", topic_nums))
    rids <- intersect(rids, substantive_ids)
    topic_yearly %>%
      filter(topic %in% rids) %>%
      group_by(year) %>%
      summarise(theta_RY = round(sum(theta_bar_kY), 5), .groups = "drop") %>%
      mutate(register = clean_register_name(rname)) %>%
      select(register, year, theta_RY)
  })
  
  # ── 5. Register yearly prevalence in WIDE format ─────────────────────────
  # One column per register; each row sums to (1 - genre share) for that year.
  register_yearly_wide <- register_yearly %>%
    pivot_wider(names_from = register, values_from = theta_RY) %>%
    arrange(year)
  
  # ── 6. Genre share (diagnostic) ──────────────────────────────────────────
  # The share of corpus topical content not assigned to any substantive
  # register. Useful for interpreting the (1 - genre share) sum property.
  genre_share <- if (length(genre_ids) > 0) {
    theta %>%
      group_by(year) %>%
      summarise(
        genre_share = round(mean(rowSums(across(all_of(genre_ids)))), 5),
        .groups = "drop"
      )
  } else {
    data.frame(year = sort(unique(theta$year)), genre_share = 0)
  }
  
  list(
    topic_mean           = topic_mean,
    topic_yearly         = topic_yearly,
    register_mean        = register_mean,
    register_yearly      = register_yearly,
    register_yearly_wide = register_yearly_wide,
    genre_share          = genre_share
  )
}

if (analyze_us) {
  us_metrics <- compute_prevalence_metrics(
    stm_us, out_us$meta, k_us,
    groups        = register_groups,
    corpus_side   = "us",
    corpus_prefix = "US",
    genre_topics  = us_genre_topics
  )
  write_csv(us_metrics$topic_mean,
            file.path(output_dir_us, "us_topic_mean_prevalence.csv"))
  write_csv(us_metrics$topic_yearly,
            file.path(output_dir_us, "us_topic_yearly_prevalence.csv"))
  write_csv(us_metrics$register_mean,
            file.path(output_dir_us, "us_register_mean_prevalence.csv"))
  write_csv(us_metrics$register_yearly,
            file.path(output_dir_us, "us_register_yearly_prevalence.csv"))
  write_csv(us_metrics$register_yearly_wide,
            file.path(output_dir_us, "us_register_yearly_wide.csv"))
  write_csv(us_metrics$genre_share,
            file.path(output_dir_us, "us_genre_share_yearly.csv"))
  message("\nUS prevalence metrics saved.")
  message("US register mean prevalence (theta_R):")
  print(us_metrics$register_mean)
  message("US topic-mean sanity check (should equal 1.000): ",
          round(sum(us_metrics$topic_mean$theta_bar_k), 4))
}

if (analyze_eu) {
  eu_metrics <- compute_prevalence_metrics(
    stm_eu, out_eu$meta, k_eu,
    groups        = register_groups,
    corpus_side   = "eu",
    corpus_prefix = "EU",
    genre_topics  = eu_genre_topics
  )
  write_csv(eu_metrics$topic_mean,
            file.path(output_dir_eu, "eu_topic_mean_prevalence.csv"))
  write_csv(eu_metrics$topic_yearly,
            file.path(output_dir_eu, "eu_topic_yearly_prevalence.csv"))
  write_csv(eu_metrics$register_mean,
            file.path(output_dir_eu, "eu_register_mean_prevalence.csv"))
  write_csv(eu_metrics$register_yearly,
            file.path(output_dir_eu, "eu_register_yearly_prevalence.csv"))
  write_csv(eu_metrics$register_yearly_wide,
            file.path(output_dir_eu, "eu_register_yearly_wide.csv"))
  write_csv(eu_metrics$genre_share,
            file.path(output_dir_eu, "eu_genre_share_yearly.csv"))
  message("\nEU prevalence metrics saved.")
  message("EU register mean prevalence (theta_R):")
  print(eu_metrics$register_mean)
  message("EU topic-mean sanity check (should equal 1.000): ",
          round(sum(eu_metrics$topic_mean$theta_bar_k), 4))
}

# =============================================================================
# COMPLETE
# =============================================================================

message("\n=== OUTPUTS COMPLETE ===")
message("Files saved to: ", output_dir)
message("\nOutput structure:")
message("  04_STM_Analysis/")
message("    ├── US/")
message("    │   ├── us_correlation_matrix.csv")
message("    │   ├── us_notable_correlations.csv")
message("    │   ├── us_correlations_per_topic.csv")
message("    │   ├── us_topic_correlations_network.png")
message("    │   ├── us_year_trends_spline_all.png")
message("    │   ├── us_spline_gg.png  [all substantive topics]")
message("    │   ├── us_representative_docs.csv")
message("    │   ├── us_yearly_topic_proportions.csv")
message("    │   ├── us_trajectory_analysis.csv")
message("    │   ├── us_topic_cv.csv")
message("    │   ├── us_topic_mean_prevalence.csv      [theta_bar_k]")
message("    │   ├── us_topic_yearly_prevalence.csv    [theta_bar_kY, long]")
message("    │   ├── us_register_mean_prevalence.csv   [theta_R]")
message("    │   ├── us_register_yearly_prevalence.csv [theta_RY, long]")
message("    │   ├── us_register_yearly_wide.csv       [theta_RY, wide]")
message("    │   └── us_genre_share_yearly.csv         [diagnostic]")
message("    ├── EU/")
message("    │   ├── eu_spline_gg.png  [all substantive topics]")
message("    │   ├── eu_topic_mean_prevalence.csv      [theta_bar_k]")
message("    │   ├── eu_topic_yearly_prevalence.csv    [theta_bar_kY, long]")
message("    │   ├── eu_register_mean_prevalence.csv   [theta_R]")
message("    │   ├── eu_register_yearly_prevalence.csv [theta_RY, long]")
message("    │   ├── eu_register_yearly_wide.csv       [theta_RY, wide]")
message("    │   ├── eu_genre_share_yearly.csv         [diagnostic]")
message("    │   └── ...")
message("    ├── Grouped_Splines/")
message("    │   ├── Liberal_Integration_spline.png")
message("    │   ├── Bilateral_Management_spline.png")
message("    │   ├── Economic_Security_spline.png")
message("    │   └── Self_Conception_spline.png")
message("    ├── Overlay_Plots/")
message("    │   ├── [Register overlays — now use raw theta:]")
message("    │   │   ├── EU_Registers_All_overlay.png")
message("    │   │   ├── US_Registers_All_overlay.png")
message("    │   │   ├── EU_Registers_Transformation_overlay.png")
message("    │   │   ├── US_Registers_Transformation_overlay.png")
message("    │   │   ├── Cross_Corpus_Transformation_overlay.png")
message("    │   │   └── [corresponding _data.csv files]")
message("    │   ├── [Cross-corpus register overlays:]")
message("    │       ├── Cross_Corpus_Transformation_overlay.png")
message("    │       ├── Liberal_Integration_Cross_Corpus_overlay.png")
message("    │       ├── Economic_Security_Cross_Corpus_overlay.png")
message("    │       └── Bilateral_Management_Cross_Corpus_overlay.png")
message("    └── corpus_summary_stats.csv")
message("\nSpline objects also saved to 03_STM_Outputs/[US|EU]/.")
message("\nGenre topics excluded from gg plots: US06, US15 (US) | EU01, EU12 (EU)")
message("\nTopic overlay CIs: ", ifelse(show_topic_overlay_ci, "SHOWN", "OMITTED"))
message("\nPrevalence convention: raw theta (no genre rescaling).")
message("Register values across the four registers sum to (1 - genre share),")
message("not to 1, within each year. See us/eu_genre_share_yearly.csv.")
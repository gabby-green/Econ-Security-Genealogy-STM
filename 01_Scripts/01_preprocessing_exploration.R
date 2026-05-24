# =============================================================================
# STM Preprocessing Exploration: N-gram Analysis, Vocab Check & SearchK
# =============================================================================

# Load shared configuration
library(here)
source(here("01_Scripts", "00_configuration.R"))

# Load required packages
library(quanteda.textstats)

run_ngrams <- TRUE
run_bigram_comparison <- TRUE
run_vocab_check <- TRUE
run_searchk <- TRUE
run_searchk_us <- TRUE   # Set FALSE to skip US searchK
run_searchk_eu <- TRUE   # Set FALSE to skip EU searchK

# =============================================================================
# OUTPUT DIRECTORY SETUP
# =============================================================================

output_dir    <- here("02_Preprocessing_Exploration")
output_dir_us <- here("02_Preprocessing_Exploration", "US")
output_dir_eu <- here("02_Preprocessing_Exploration", "EU")

dir.create(output_dir,    showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_us, showWarnings = FALSE, recursive = TRUE)
dir.create(output_dir_eu, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# LOAD DATA (using shared function)
# =============================================================================

corpus_all <- load_corpus()
corpus_us <- corpus_all %>% filter(institution == "US")
corpus_eu <- corpus_all %>% filter(institution == "EU")

# =============================================================================
# N-GRAM ANALYSIS
# =============================================================================

analyze_ngrams <- function(corpus_data, label, out_dir,
                           n_unigrams = 100, n_bigrams = 200, n_trigrams = 200) {
  
  message("\n=== N-GRAM ANALYSIS: ", label, " ===")
  
  corpus_data <- corpus_data %>%
    mutate(
      text = str_replace_all(text, "={2,}", " "),
      text = str_replace_all(text, "\u2018|\u2019", "'"),
      text = str_replace_all(text, "\u201C|\u201D", '"'),
      text = str_replace_all(text, "\u2014|\u2013", "-"),
      text = str_replace_all(text, "'s\\b", ""),
      text = str_replace_all(text, "s'\\b", "s")
    )
  
  qcorpus <- corpus(corpus_data, text_field = "text")
  
  toks <- tokens(qcorpus, remove_punct = TRUE, remove_numbers = TRUE) %>%
    tokens_tolower() %>%
    tokens_remove(stopwords("en"))
  
  unigrams <- tokens_ngrams(toks, n = 1) %>% dfm() %>% topfeatures(n_unigrams)  
  bigrams  <- tokens_ngrams(toks, n = 2) %>% dfm() %>% topfeatures(n_bigrams)
  trigrams <- tokens_ngrams(toks, n = 3) %>% dfm() %>% topfeatures(n_trigrams)
  
  write_csv(tibble(term = names(unigrams), freq = unigrams),
            file.path(out_dir, paste0(label, "_unigrams.csv")))
  write_csv(tibble(term = names(bigrams),  freq = bigrams),
            file.path(out_dir, paste0(label, "_bigrams.csv")))
  write_csv(tibble(term = names(trigrams), freq = trigrams),
            file.path(out_dir, paste0(label, "_trigrams.csv")))
  
  message("\nTop 30 Bigrams:")
  print(head(bigrams, 30))
  
  return(list(bigrams = bigrams, trigrams = trigrams))
}

if (run_ngrams) {
  ngrams_us <- analyze_ngrams(corpus_us, "us", output_dir_us)
  ngrams_eu <- analyze_ngrams(corpus_eu, "eu", output_dir_eu)
  
  us_bi <- tibble(term = names(ngrams_us$bigrams), us_freq = as.numeric(ngrams_us$bigrams))
  eu_bi <- tibble(term = names(ngrams_eu$bigrams), eu_freq = as.numeric(ngrams_eu$bigrams))
  
  bigram_comparison <- full_join(us_bi, eu_bi, by = "term") %>%
    replace_na(list(us_freq = 0, eu_freq = 0)) %>%
    mutate(
      total    = us_freq + eu_freq,
      us_share = us_freq / total,
      corpus   = case_when(
        us_share > 0.8 ~ "US-dominant",
        us_share < 0.2 ~ "EU-dominant",
        TRUE           ~ "Shared"
      )
    ) %>%
    arrange(desc(total))
  
  # Shared output stays in parent directory
  write_csv(bigram_comparison, file.path(output_dir, "bigram_comparison.csv"))
}

# =============================================================================
# VOCABULARY CHECK
# =============================================================================

if (run_vocab_check) {
  vocab_results <- c()
  vocab_results <- c(vocab_results, "=== US VOCABULARY SIZE CHECK ===")
  
  for (threshold in c(7)) {
    out_temp <- preprocess_corpus(corpus_us, phrases_us_full, min_docfreq = threshold)
    vocab_results <- c(vocab_results, paste0("min_docfreq = ", threshold, ": ", length(out_temp$vocab), " features"))
  }
  
  vocab_results <- c(vocab_results, "", "=== EU VOCABULARY SIZE CHECK ===")
  
  for (threshold in c(5)) {
    out_temp <- preprocess_corpus(corpus_eu, phrases_eu_full, min_docfreq = threshold)
    vocab_results <- c(vocab_results, paste0("min_docfreq = ", threshold, ": ", length(out_temp$vocab), " features"))
  }
  
  # Shared output stays in parent directory
  writeLines(vocab_results, file.path(output_dir, "vocab_check.txt"))
  message("Vocabulary check saved to vocab_check.txt")
}

# =============================================================================
# SEARCHK (multi-seed averaged)
# =============================================================================
# searchK uses random holdout partitions, producing unstable results on small
# corpora (see searchK documentation). I average across multiple seeds to
# obtain stable diagnostic estimates.
# =============================================================================

if (run_searchk) {
  
  message("\n=== RUNNING SEARCHK (multi-seed) ===")
  
  # Only preprocess corpora that will be used
  if (run_searchk_us) out_us <- preprocess_corpus(corpus_us, phrases_us_full, min_docfreq = 7)
  if (run_searchk_eu) out_eu <- preprocess_corpus(corpus_eu, phrases_eu_full, min_docfreq = 5)
  
  # --- Configuration ---
  n_seeds <- 20
  set.seed(2025)
  seeds   <- sample.int(10000, n_seeds)
  k_range <- c(10, 15, 20, 25, 30)
  
  message("Using ", n_seeds, " random seeds: ", paste(seeds, collapse = ", "))
  
  # --- Multi-seed searchK function ---
  
  run_multiseed_searchk <- function(out, seeds, k_range, label) {
    
    results_list <- list()
    
    for(i in seq_along(seeds)) {
      message("  ", label, " seed ", i, "/", length(seeds))
      set.seed(seeds[i])
      sk <- searchK(
        documents  = out$documents,
        vocab      = out$vocab,
        K          = k_range,
        prevalence = ~ year,
        data       = out$meta,
        init.type  = "Spectral",
        verbose    = FALSE
      )
      
      results_list[[i]] <- data.frame(
        seed      = seeds[i],
        K         = unlist(sk$results$K),
        heldout   = unlist(sk$results$heldout),
        residuals = unlist(sk$results$residual),
        semcoh    = unlist(sk$results$semcoh),
        exclus    = unlist(sk$results$exclus)
      )
    }
    
    all_runs <- do.call(rbind, results_list)
    
    avg              <- aggregate(. ~ K, data = all_runs[, -1], FUN = mean)
    sds              <- aggregate(. ~ K, data = all_runs[, -1], FUN = sd)
    names(sds)[-1]   <- paste0(names(sds)[-1], "_sd")
    
    summary <- merge(avg, sds, by = "K")
    
    return(list(all_runs = all_runs, summary = summary))
  }
  
  # --- Run ---
  
  if (run_searchk_us) {
    message("\nUS corpus:")
    sk_us <- run_multiseed_searchk(out_us, seeds, k_range, "US")
    saveRDS(sk_us,          file.path(output_dir_us, "searchk_us_multiseed.rds"))
    write_csv(sk_us$summary, file.path(output_dir_us, "searchk_us_averaged.csv"))
    message("US searchK complete and saved.")
  }
  
  if (run_searchk_eu) {
    message("\nEU corpus:")
    sk_eu <- run_multiseed_searchk(out_eu, seeds, k_range, "EU")
    saveRDS(sk_eu,          file.path(output_dir_eu, "searchk_eu_multiseed.rds"))
    write_csv(sk_eu$summary, file.path(output_dir_eu, "searchk_eu_averaged.csv"))
    message("EU searchK complete and saved.")
  }
  
  # Shared output stays in parent directory
  write_csv(data.frame(seed = seeds), file.path(output_dir, "searchk_seeds.csv"))
  
  # --- Print summary ---
  
  message("\n=== AVERAGED SEARCHK RESULTS ===\n")
  
  for(label in c("US", "EU")) {
    if (label == "US" && !run_searchk_us) next
    if (label == "EU" && !run_searchk_eu) next
    
    sk    <- if(label == "US") sk_us else sk_eu
    sel_k <- if(label == "US") 20 else 15
    
    message(label, " corpus (", n_seeds, " seeds):")
    message("  K | Held-out        | Residuals       | Sem.Coh         | Exclusivity")
    for(i in 1:nrow(sk$summary)) {
      s      <- sk$summary[i, ]
      marker <- if(s$K == sel_k) " <--" else ""
      message(sprintf("  %2d| %7.4f (\u00b1%.4f) | %.4f (\u00b1%.4f) | %6.1f (\u00b1%.1f) | %.4f (\u00b1%.4f)%s",
                      s$K, s$heldout, s$heldout_sd, s$residuals, s$residuals_sd,
                      s$semcoh, s$semcoh_sd, s$exclus, s$exclus_sd, marker))
    }
    
    message(sprintf("\n  K=%d averaged ranks: heldout=%d/5, residuals=%d/5, semcoh=%d/5, exclus=%d/5\n",
                    sel_k,
                    rank(-sk$summary$heldout)[sk$summary$K == sel_k],
                    rank(abs(sk$summary$residuals - 1))[sk$summary$K == sel_k],
                    rank(-sk$summary$semcoh)[sk$summary$K == sel_k],
                    rank(-sk$summary$exclus)[sk$summary$K == sel_k]))
  }
  
  # ==========================================================================
  # PLOTS
  # ==========================================================================
  
  message("=== CREATING PLOTS ===")
  
  make_four_panel <- function(results, label, out_dir) {
    png(file.path(out_dir, paste0("searchk_", tolower(label), ".png")),
        width = 1000, height = 800)
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
    
    plot(results$K, results$Held_Out_Likelihood, type = "b", pch = 19,
         xlab = "Number of Topics (K)", ylab = "Held-Out Likelihood",
         main = "Held-Out Likelihood")
    plot(results$K, results$Residuals, type = "b", pch = 19,
         xlab = "Number of Topics (K)", ylab = "Residuals",
         main = "Residuals")
    plot(results$K, results$Semantic_Coherence, type = "b", pch = 19,
         xlab = "Number of Topics (K)", ylab = "Semantic Coherence",
         main = "Semantic Coherence")
    plot(results$K, results$Exclusivity, type = "b", pch = 19,
         xlab = "Number of Topics (K)", ylab = "Exclusivity",
         main = "Exclusivity")
    
    mtext(paste0("Diagnostic Values by Number of Topics (", label,
                 ", averaged over ", n_seeds, " holdout partitions)"),
          outer = TRUE, cex = 1.1)
    dev.off()
  }
  
  make_coh_exc_plot <- function(results, label, filename) {
    p <- ggplot(results, aes(x = Semantic_Coherence, y = Exclusivity, label = K)) +
      geom_errorbar(aes(xmin = Semantic_Coherence - SC_sd,
                        xmax = Semantic_Coherence + SC_sd),
                    width = 0, alpha = 0.3, linewidth = 0.5, color = "steelblue") +
      geom_errorbar(aes(ymin = Exclusivity - Exc_sd,
                        ymax = Exclusivity + Exc_sd),
                    width = 0, alpha = 0.3, linewidth = 0.5, color = "steelblue") +
      geom_point(color = "steelblue", size = 3, alpha = 0.8) +
      geom_text(vjust = -1, size = 3.5, color = "black") +
      geom_path(aes(group = 1), color = "gray50", linetype = "dashed",
                arrow = arrow(length = unit(0.15, "cm")), linewidth = 0.5) +
      labs(
        title    = paste("Model Selection:", label, "Corpus"),
        subtitle = paste0("Semantic Coherence vs. Exclusivity (K=10-30, averaged over ",
                          n_seeds, " holdout partitions)"),
        x        = "Mean Semantic Coherence (higher = better)",
        y        = "Mean Exclusivity (higher = better)",
        caption  = paste0("Error bars show \u00b11 SD across holdout partitions")
      ) +
      theme_minimal() +
      theme(
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10)
      )
    
    ggsave(filename, p, width = 8, height = 6, dpi = 300)
  }
  
  if (run_searchk_us) {
    us_results <- data.frame(
      Corpus             = "US", K = sk_us$summary$K,
      Semantic_Coherence = sk_us$summary$semcoh,
      Exclusivity        = sk_us$summary$exclus,
      Held_Out_Likelihood = sk_us$summary$heldout,
      Residuals          = sk_us$summary$residuals,
      SC_sd              = sk_us$summary$semcoh_sd,
      Exc_sd             = sk_us$summary$exclus_sd
    )
    make_four_panel(us_results, "US", output_dir_us)
    make_coh_exc_plot(us_results, "US",
                      file.path(output_dir_us, "searchk_coherence_exclusivity_us.png"))
    message("  Saved: searchk_us.png, searchk_coherence_exclusivity_us.png")
  }
  
  if (run_searchk_eu) {
    eu_results <- data.frame(
      Corpus             = "EU", K = sk_eu$summary$K,
      Semantic_Coherence = sk_eu$summary$semcoh,
      Exclusivity        = sk_eu$summary$exclus,
      Held_Out_Likelihood = sk_eu$summary$heldout,
      Residuals          = sk_eu$summary$residuals,
      SC_sd              = sk_eu$summary$semcoh_sd,
      Exc_sd             = sk_eu$summary$exclus_sd
    )
    make_four_panel(eu_results, "EU", output_dir_eu)
    make_coh_exc_plot(eu_results, "EU",
                      file.path(output_dir_eu, "searchk_coherence_exclusivity_eu.png"))
    message("  Saved: searchk_eu.png, searchk_coherence_exclusivity_eu.png")
  }
  
  # Shared output stays in parent directory
  if (run_searchk_us && run_searchk_eu) {
    write_csv(rbind(us_results, eu_results),
              file.path(output_dir, "searchk_comparison_table.csv"))
    message("  Saved: searchk_comparison_table.csv")
  }
  
  message("\n=== SEARCHK ANALYSIS COMPLETE ===")
  message("Seeds used: ", paste(seeds, collapse = ", "))
  message("Output saved to: ", output_dir)
}

message("\n=== PRE-PROCESSING EXPLORATION COMPLETE ===")
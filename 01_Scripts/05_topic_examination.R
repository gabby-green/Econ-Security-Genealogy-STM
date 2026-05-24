# =============================================================================
# Topic Exploration Script
# =============================================================================

library(here)
source(here("01_Scripts", "00_configuration.R"))
library(dplyr)
library(stm)
library(quanteda)

# =============================================================================
# CONFIGURATION - CHANGE THESE
# =============================================================================

corpus    <- "US"   # "EU" or "US"
topic_num <- 7      # topic to examine in single-topic mode
n_docs    <- 20     # how many documents to pull for findThoughts
doc_threshold <- 0.5   # theta threshold for document listing

# KWIC Configuration
run_kwic           <- TRUE   # set FALSE to skip KWIC in single-topic mode
kwic_doc_threshold <- 0.5    # theta threshold for KWIC document selection
kwic_window        <- 25     # words either side of keyword

# Batch Configuration
# Each topic gets its own .txt file in 04_STM_Analysis/findthoughts_and_kwic_analysis/
# Each file contains: topic words, top documents, text excerpts, KWIC results
run_batch          <- TRUE   # set FALSE to skip batch output
batch_n_docs       <- 10     # docs per topic in batch output
batch_excerpt_chars <- 800   # characters per excerpt in batch output
batch_kwic_threshold <- 0.5  # theta threshold for KWIC in batch output

# =============================================================================
# TOPIC-SPECIFIC KWIC TERMS
# "china" is always added automatically.
# Multi-word terms (containing a space) are matched as exact phrases.
# Single-word terms use a trailing wildcard (e.g. "energ*" catches energy/energies).
# Stemmed FREX terms should be entered as their raw-text root
# (e.g. FREX "energi" -> enter "energ" to catch energy/energies/energized).
# =============================================================================

genre_topics_US <- c(6, 15)
genre_topics_EU <- c(1, 12)

# ---- EU per-topic KWIC terms ------------------------------------------------
kwic_terms_EU <- list(
  "2"  = c("metal", "steel", "aluminium", "aluminum", "decarboni", "recycle",
           "circular", "critical raw material", "carbon", "suppl"),
  "3"  = c("transatlantic", "rules-based", "democracy", "mexico", "allies",
           "united states", "world trade organ", "order", "friend"),
  "4"  = c("summit", "cooper", "sign", "agreement", "partnership",
           "commission", "minister"),
  "5"  = c("doha development agenda", "doha", "world trade organ",
           "commit", "import", "relationship", "hope"),
  "6"  = c("intellectual property", "market access", "regulatory",
           "reciproc", "deficit", "investor", "service"),
  "7"  = c("ukraine", "de-risk", "war", "russia", "geopolit", "rare earth",
           "decouple", "diversif", "climate change"),
  "8"  = c("dumping", "anti-dumping", "duti", "shoe", "textile", "leather",
           "unfair", "trade defence", "investigat", "competit"),
  "9"  = c("trade policy", "free trade agreement", "labour", "labor",
           "sustainable development", "regulatory", "service", "digital",
           "value chain", "negoti"),
  "10" = c("globali", "open", "centur", "currenc", "consumpt", "boom",
           "Mandelson", "argument"),
  "11" = c("economic security", "risk", "resili", "foreign subsidies",
           "dual use", "infrastructure", "technolog", "control", "critic"),
  "13" = c("cooperation", "programme", "dialogue", "human rights",
           "rule of law", "development", "migrat"),
  "14" = c("level playing field", "reciproc",
           "comprehensive agreement on investment",
           "engag", "princip", "law", "secur"),
  "15" = c("asia", "asean", "asem", "terror", "histor", "region",
           "popul", "east")
)

# ---- US per-topic KWIC terms ------------------------------------------------
kwic_terms_US <- list(
  "1"  = c("currency", "exchange rate", "flexibl", "deficit", "surplus",
           "imbalanc", "capital", "g-7", "g7"),
  "2"  = c("intellectual property", "infringe", "industrial policy",
           "telecom", "transpar", "copyright", "trips",
           "world trade organ", "trade distort"),
  "3"  = c("strategic economic dialogue", "financial", "household",
           "exchange rate", "consumpt", "pledg", "reform", "demand",
           "recovery"),
  "4"  = c("trump administr", "digital", "appellate body", "unfair",
           "farmer", "renegoti", "american worker", "tariff",
           "world trade organ"),
  "5"  = c("counterfeit", "intellectual property", "enforc", "piracy",
           "bilateral trade", "dialogue", "govern", "protect"),
  "7"  = c("china accession", "safeguard", "textil", "zoellick",
           "agricultur", "world trade organ", "quota", "sanitary",
           "commit", "implement"),
  "8"  = c("communist party", "freedom", "peace", "hong kong",
           "democracy", "democr", "american", "propaganda",
           "nuclear", "influenc", "nixon"),
  "9"  = c("steel", "aluminium", "aluminum", "excess capac",
           "countervail", "national security", "tariff", "injur",
           "invest", "appeal"),
  "10" = c("biden administr", "forced labor", "resili", "worker",
           "supply chain", "climate", "union", "environ", "commit",
           "underserv"),
  "11" = c("state-owned enterpr", "film", "government procur",
           "information communications technolog", "licens",
           "anti-monopol", "world trade organ", "regulatory",
           "export", "restraint"),
  "12" = c("forced technology transfer", "mercantilist", "excess capac",
           "non-market", "phase one agreement", "made in china 2025",
           "industrial policy", "tariff", "market"),
  "13" = c("non-market approach", "predator", "state-owned",
           "technology transfer", "excess capac", "world trade organ",
           "practic", "import", "harm"),
  "14" = c("subsidies", "government procur", "dispute",
           "level playing field", "raw material", "auto",
           "world trade organ", "enforc", "intellectual property"),
  "16" = c("doha", "pacific", "asia", "america", "hope", "people",
           "benefit", "agreement", "open"),
  "17" = c("human rights", "allie", "climate",
           "committee on foreign investment",
           "debt", "risk", "conflict", "indo-pacific",
           "national security", "technolog"),
  "18" = c("semiconductor", "chip", "supply chain", "infrastructure",
           "energ", "battery", "technolog", "manufactur", "resili",
           "president biden"),
  "19" = c("trans-pacific partnership", "tpp", "job", "trade agreement",
           "asia-pacific", "environment", "labor", "american",
           "export", "service"),
  "20" = c("free trade agreement", "free trade", "developing countr",
           "doha", "negoti", "integr", "bush administr",
           "world trade organ", "market", "opportun")
)

# =============================================================================
# LOAD MODELS AND READ ALL TEXTS
# =============================================================================

if (corpus == "US") {
  stm_model    <- readRDS(here("03_STM_Outputs", "US", "us_stm_model_k20.rds"))
  out          <- readRDS(here("03_STM_Outputs", "US", "us_stm_preprocessed.rds"))
  k            <- 20
  kwic_lookup  <- kwic_terms_US
  genre_topics <- genre_topics_US
} else {
  stm_model    <- readRDS(here("03_STM_Outputs", "EU", "eu_stm_model_k15.rds"))
  out          <- readRDS(here("03_STM_Outputs", "EU", "eu_stm_preprocessed.rds"))
  k            <- 15
  kwic_lookup  <- kwic_terms_EU
  genre_topics <- genre_topics_EU
}

cat("Reading document texts...\n")
doc_texts <- sapply(out$meta$file_name, function(f) {
  filepath <- here("00_Corpus", paste0(f, ".txt"))
  if (file.exists(filepath)) {
    text <- readLines(filepath, warn = FALSE) %>% paste(collapse = " ")
    gsub("\\s+", " ", text)
  } else {
    NA_character_
  }
})

# =============================================================================
# HELPERS
# =============================================================================

# Build KWIC term list for a topic (always prepends "china")
get_kwic_terms <- function(topic_n, lookup) {
  key         <- as.character(topic_n)
  topic_terms <- if (!is.null(lookup[[key]])) lookup[[key]] else character(0)
  unique(c("china", topic_terms))
}

# Build quanteda tokens object from high-loading documents for a topic
build_tokens <- function(topic_n, threshold) {
  idx <- which(stm_model$theta[, topic_n] > threshold)
  if (length(idx) == 0) return(NULL)
  texts <- sapply(out$meta$file_name[idx], function(f) {
    fp <- here("00_Corpus", paste0(f, ".txt"))
    if (file.exists(fp)) paste(readLines(fp, warn = FALSE), collapse = " ") else NA
  })
  texts <- texts[!is.na(texts)]
  if (length(texts) == 0) return(NULL)
  tokens(corpus(texts), remove_punct = TRUE) %>% tokens_tolower()
}

# Run KWIC for a set of terms on a tokens object; write output via cat()
write_kwic <- function(toks, terms, window) {
  for (term in terms) {
    cat("\n>>> KWIC:", term, "<<<\n\n")
    pattern_arg <- if (grepl(" ", term)) phrase(term) else paste0(term, "*")
    res <- kwic(toks, pattern = pattern_arg, window = window)
    if (nrow(res) > 0) {
      for (j in 1:nrow(res)) {
        cat("[", res$docname[j], "]\n", sep = "")
        cat("  ...", res$pre[j], " [", res$keyword[j], "] ", res$post[j], "...\n\n", sep = "")
      }
      cat("Total occurrences:", nrow(res), "\n")
    } else {
      cat("No occurrences found.\n")
    }
  }
}

# =============================================================================
# SINGLE-TOPIC EXAMINATION (console output)
# =============================================================================

cat("\n========== ", corpus, " TOPIC ", topic_num, " ==========\n\n", sep = "")

labels <- labelTopics(stm_model, topics = topic_num, n = 15)
cat("PROB:  ", paste(labels$prob[topic_num,], collapse = ", "), "\n\n")
cat("FREX:  ", paste(labels$frex[topic_num,], collapse = ", "), "\n\n")
cat("LIFT:  ", paste(labels$lift[topic_num,], collapse = ", "), "\n\n")
cat("SCORE: ", paste(labels$score[topic_num,], collapse = ", "), "\n\n")

cat("---------- TOP DOCUMENTS ----------\n\n")
thoughts    <- findThoughts(stm_model, texts = doc_texts, n = n_docs, topics = topic_num)
doc_indices <- thoughts$index[[1]]

doc_summary <- data.frame(
  Rank       = 1:length(doc_indices),
  File       = out$meta$file_name[doc_indices],
  Year       = out$meta$year[doc_indices],
  Proportion = round(stm_model$theta[doc_indices, topic_num], 3),
  Title      = out$meta$title[doc_indices]
)
for (i in 1:nrow(doc_summary)) {
  cat(sprintf("[%2d] %s (%d) - %.3f\n     %s\n\n",
              doc_summary$Rank[i], doc_summary$File[i],
              doc_summary$Year[i], doc_summary$Proportion[i],
              doc_summary$Title[i]))
}

cat("\n---------- TEXT EXCERPTS ----------\n")
thought_texts <- thoughts$docs[[1]]
for (i in 1:min(5, length(thought_texts))) {
  excerpt <- substr(thought_texts[i], 1, 600)
  cat("\n[", i, "] ", out$meta$file_name[doc_indices[i]],
      " (", out$meta$year[doc_indices[i]], ")\n", sep = "")
  cat("    ", out$meta$title[doc_indices[i]], "\n", sep = "")
  cat(strwrap(excerpt, width = 80), sep = "\n")
  cat("...\n")
}

threshold  <- quantile(stm_model$theta[, topic_num], 0.9)
high_docs  <- which(stm_model$theta[, topic_num] > threshold)

cat("\n---------- YEAR DISTRIBUTION ----------\n\n")
print(table(out$meta$year[high_docs]))

cat("\n---------- DOCUMENT TYPE DISTRIBUTION ----------\n\n")
if ("document_type" %in% names(out$meta)) {
  print(table(out$meta$document_type[high_docs]))
} else { cat("document_type not found in metadata\n") }

cat("\n---------- SUB-INSTITUTION DISTRIBUTION ----------\n\n")
if ("sub_institution" %in% names(out$meta)) {
  print(table(out$meta$sub_institution[high_docs]))
} else { cat("sub_institution not found in metadata\n") }

if (corpus == "US") {
  theta_admin <- stm_model$theta[, topic_num]
  years <- out$meta$year
  admin <- ifelse(years <= 2008, "Bush",
                  ifelse(years <= 2016, "Obama",
                         ifelse(years <= 2020, "Trump1",
                                ifelse(years <= 2024, "Biden",
                                       ifelse(years == 2025, "Trump2", NA)))))
  cat("\n---------- ADMIN MEAN PREVALENCE ----------\n\n")
  print(round(tapply(theta_admin, admin, mean), 3))
}

cat("\n---------- ALL DOCUMENTS LOADING ON TOPIC ----------\n\n")
high_idx <- which(stm_model$theta[, topic_num] > doc_threshold)
if (length(high_idx) > 0) {
  topic_docs <- data.frame(
    file       = out$meta$file_name[high_idx],
    year       = out$meta$year[high_idx],
    proportion = round(stm_model$theta[high_idx, topic_num], 3),
    title      = out$meta$title[high_idx]
  )
  topic_docs <- topic_docs[order(-topic_docs$proportion), ]
  cat("Documents with proportion >", doc_threshold, ":", nrow(topic_docs), "\n\n")
  for (i in 1:nrow(topic_docs)) {
    cat(sprintf("[%2d] %s (%d) - %.3f\n     %s\n\n",
                i, topic_docs$file[i], topic_docs$year[i],
                topic_docs$proportion[i], topic_docs$title[i]))
  }
} else {
  cat("No documents above threshold", doc_threshold, "\n")
}

if (run_kwic) {
  cat("\n---------- KEYWORD-IN-CONTEXT ----------\n\n")
  toks <- build_tokens(topic_num, kwic_doc_threshold)
  if (!is.null(toks)) {
    active_terms <- get_kwic_terms(topic_num, kwic_lookup)
    cat("Search terms:", paste(active_terms, collapse = " | "), "\n")
    write_kwic(toks, active_terms, kwic_window)
  } else {
    cat("No documents found above threshold.\n")
  }
}

message("\n=== SINGLE-TOPIC EXAMINATION COMPLETE ===\n")

# =============================================================================
# BATCH OUTPUT
# One .txt file per substantive topic saved to:
#   04_STM_Analysis/findthoughts_and_kwic_analysis/
# Each file contains:
#   - PROB and FREX terms
#   - Top documents list
#   - Text excerpts (batch_excerpt_chars characters each)
#   - KWIC results for topic-specific terms + "china"
# =============================================================================

if (run_batch) {
  
  substantive_tops <- setdiff(1:k, genre_topics)
  
  out_dir <- here("04_STM_Analysis", "findthoughts_and_kwic_analysis")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  cat("Writing batch files to:", out_dir, "\n\n")
  
  for (t in substantive_tops) {
    
    out_file <- file.path(out_dir,
                          sprintf("%s_topic%02d_findthoughts.txt", tolower(corpus), t))
    
    sink(out_file)
    
    cat("=============================================================\n")
    cat(corpus, "Corpus -- Topic", t, "\n")
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
    cat("=============================================================\n\n")
    
    # Topic words
    lbl <- labelTopics(stm_model, topics = t, n = 15)
    cat("PROB: ", paste(lbl$prob[t, ], collapse = ", "), "\n")
    cat("FREX: ", paste(lbl$frex[t, ], collapse = ", "), "\n\n")
    
    # Top documents
    ft  <- findThoughts(stm_model, texts = doc_texts, n = batch_n_docs, topics = t)
    idx <- ft$index[[1]]
    
    cat("-------------------------------------------------------------\n")
    cat("TOP DOCUMENTS\n")
    cat("-------------------------------------------------------------\n\n")
    for (i in seq_along(idx)) {
      cat(sprintf("[%2d] %s (%s) -- theta=%.3f\n     %s\n\n",
                  i,
                  out$meta$file_name[idx[i]],
                  out$meta$year[idx[i]],
                  round(stm_model$theta[idx[i], t], 3),
                  out$meta$title[idx[i]]))
    }
    
    # Text excerpts
    cat("-------------------------------------------------------------\n")
    cat("TEXT EXCERPTS\n")
    cat("-------------------------------------------------------------\n\n")
    excerpts <- ft$docs[[1]]
    for (i in seq_along(excerpts)) {
      excerpt <- substr(excerpts[i], 1, batch_excerpt_chars)
      cat(sprintf("[%d] %s (%s)\n",
                  i, out$meta$file_name[idx[i]], out$meta$year[idx[i]]))
      cat(strwrap(excerpt, width = 100), sep = "\n")
      cat("...\n\n")
    }
    
    # KWIC
    cat("-------------------------------------------------------------\n")
    cat("KEYWORD-IN-CONTEXT\n")
    active_terms <- get_kwic_terms(t, kwic_lookup)
    cat("Search terms:", paste(active_terms, collapse = " | "), "\n")
    cat("-------------------------------------------------------------\n")
    
    toks <- build_tokens(t, batch_kwic_threshold)
    if (!is.null(toks)) {
      write_kwic(toks, active_terms, kwic_window)
    } else {
      cat("\nNo documents above KWIC threshold.\n")
    }
    
    sink()
    cat(sprintf("  Topic %02d -> %s\n", t, basename(out_file)))
  }
  
  cat("\nDone. Files saved to:", out_dir, "\n")
}

message("=== SCRIPT COMPLETE ===")

# =============================================================================
# NOTE: to pull a specific file for further inspection, run in console:
# file.edit(here("00_Corpus", "USTR_SP_1021.txt"))
# =============================================================================
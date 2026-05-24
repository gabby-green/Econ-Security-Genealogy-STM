# =============================================================================
# Generate topic_summary.xlsx
# =============================================================================

library(here)
library(tidyverse)
library(openxlsx)

# =============================================================================
# FILE PATHS
# =============================================================================

stm_dir_us   <- here("03_STM_Outputs", "US")
stm_dir_eu   <- here("03_STM_Outputs", "EU")
anal_dir_us  <- here("04_STM_Analysis", "US")
anal_dir_eu  <- here("04_STM_Analysis", "EU")
rob_dir_us   <- here("05_Robustness_Checks", "US")
rob_dir_eu   <- here("05_Robustness_Checks", "EU")

output_path  <- here("04_STM_Analysis", "topic_summary.xlsx")

# =============================================================================
# HELPER: Parse topic words from labelTopics text output
# =============================================================================

parse_topic_words <- function(filepath) {
  lines <- readLines(filepath)
  
  topics <- list()
  current_topic <- NULL
  
  for (line in lines) {
    # Match "Topic N Top Words:"
    topic_match <- regmatches(line, regexpr("Topic (\\d+)", line))
    if (grepl("Top Words:", line) && length(topic_match) > 0) {
      current_topic <- as.integer(gsub("Topic ", "", topic_match))
      topics[[current_topic]] <- list()
      next
    }
    
    if (!is.null(current_topic)) {
      line <- trimws(line)
      if (grepl("^Highest Prob:", line)) {
        topics[[current_topic]]$Highest_Prob <- trimws(sub("Highest Prob:", "", line))
      } else if (grepl("^FREX:", line)) {
        topics[[current_topic]]$FREX_Terms <- trimws(sub("FREX:", "", line))
      } else if (grepl("^Lift:", line)) {
        topics[[current_topic]]$Lift <- trimws(sub("Lift:", "", line))
      }
    }
  }
  
  map_dfr(seq_along(topics), function(i) {
    tibble(
      Topic        = i,
      FREX_Terms   = topics[[i]]$FREX_Terms %||% "",
      Highest_Prob = topics[[i]]$Highest_Prob %||% "",
      Lift         = topics[[i]]$Lift %||% ""
    )
  })
}

# =============================================================================
# HELPER: Split correlations into positive/negative columns
# =============================================================================

split_correlations <- function(corr_df) {
  corr_df %>%
    mutate(
      Topic = as.integer(gsub("T", "", Topic)),
      `Positive Correlations` = map_chr(Correlations, function(x) {
        if (x == "None >0.10") return("None")
        parts <- str_split(x, "; ")[[1]]
        pos <- parts[grepl("\\+", parts)]
        # Reformat from T2 (+0.19) to T2 (0.19)
        pos <- gsub("\\+", "", pos)
        if (length(pos) > 0) paste(pos, collapse = ", ") else "None"
      }),
      `Negative Correlations` = map_chr(Correlations, function(x) {
        if (x == "None >0.10") return("None")
        parts <- str_split(x, "; ")[[1]]
        neg <- parts[grepl("\\-", parts)]
        if (length(neg) > 0) paste(neg, collapse = ", ") else "None"
      })
    ) %>%
    select(Topic, `Positive Correlations`, `Negative Correlations`)
}

# =============================================================================
# ASSEMBLY FUNCTION
# =============================================================================

build_topic_sheet <- function(stm_dir, anal_dir, rob_dir, prefix, k) {
  
  message(sprintf("Assembling %s (K=%d)...", toupper(prefix), k))
  
  # 1. Topic words
  words <- parse_topic_words(file.path(stm_dir, paste0(prefix, "_topic_words.txt")))
  
  # 2. Year effects
  year_fx <- read_csv(file.path(stm_dir, paste0(prefix, "_year_effects_summary.csv")),
                      show_col_types = FALSE) %>%
    select(Topic, Direction, Year_Estimate = Estimate, P_Value = P_value)
  
  # 3. Topic quality
  quality <- read_csv(file.path(rob_dir, paste0(prefix, "_topic_quality.csv")), show_col_types = FALSE) %>%
    mutate(
      Quadrant = case_when(
        Quadrant == "High Quality (Upper-Right)" ~ "High Quality (High Coherence, High Exclusivity)",
        Quadrant == "Low Quality (Lower-Left)"   ~ "Low Quality (Low Coherence, Low Exclusivity)",
        TRUE ~ Quadrant
      )
    ) %>%
    select(Topic, Coherence, Exclusivity, Quadrant)
  
  # 4. Correlations
  corrs <- read_csv(file.path(anal_dir, paste0(prefix, "_correlations_per_topic.csv")),
                    show_col_types = FALSE) %>%
    split_correlations()
  
  # 5. Trajectories
  trajs <- read_csv(file.path(anal_dir, paste0(prefix, "_trajectory_analysis.csv")),
                    show_col_types = FALSE) %>%
    mutate(Topic = as.integer(gsub("T", "", Topic))) %>%
    select(Topic, Trajectory_Shape, Inflection_Points)
  
  # 6. Coefficient of variation
  # CV = SD / mean of each topic's proportion across documents (theta).
  # Low CV = topic appears at a consistent rate across documents (hegemonic/pervasive).
  # High CV = topic is concentrated in a small number of documents (episodic/conjunctural).
  # CV_Rank: 1 = most pervasive (lowest CV). Source: Jacobs & Tschötschel (2019).
  cv_data <- read_csv(file.path(anal_dir, paste0(prefix, "_topic_cv.csv")),
                      show_col_types = FALSE) %>%
    mutate(Topic = as.integer(gsub("T", "", Topic))) %>%
    select(Topic, CV, CV_Rank = Rank)
  
  # Combine
  sheet <- tibble(Topic = 1:k) %>%
    left_join(words, by = "Topic") %>%
    left_join(year_fx, by = "Topic") %>%
    left_join(trajs, by = "Topic") %>%
    left_join(quality, by = "Topic") %>%
    left_join(cv_data, by = "Topic") %>%
    left_join(corrs, by = "Topic")
  
  # Add blank human-input columns
  sheet <- sheet %>%
    mutate(Label = "", Key_Docs = "", Notes = "") %>%
    select(
      Topic, Label,
      FREX_Terms, Highest_Prob, Lift,
      Direction, Year_Estimate, P_Value,
      Trajectory_Shape, Inflection_Points,
      Coherence, Exclusivity, CV, CV_Rank, Quadrant,
      `Positive Correlations`, `Negative Correlations`,
      Key_Docs, Notes
    )
  
  message(sprintf("  %d topics assembled.", k))
  return(sheet)
}

# =============================================================================
# BUILD SHEETS
# =============================================================================

us_sheet <- build_topic_sheet(stm_dir_us, anal_dir_us, rob_dir_us, "us", k = 20)
eu_sheet <- build_topic_sheet(stm_dir_eu, anal_dir_eu, rob_dir_eu, "eu", k = 15)

# =============================================================================
# WRITE EXCEL
# =============================================================================

message("\nWriting Excel file...")

wb <- createWorkbook()

number_style <- createStyle(numFmt = "0.00000")
pval_style   <- createStyle(numFmt = "0.0000")
score_style  <- createStyle(numFmt = "0.00")
cv_style     <- createStyle(numFmt = "0.00")

write_formatted_sheet <- function(wb, sheet_name, data, corpus = "US") {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
  
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  col_names <- names(data)
  
  # --- Header row: corpus-specific colour, bold white text ---
  header_colour <- if (corpus == "EU") "#4F81BD" else "#C0504D"
  header_style <- createStyle(
    textDecoration = "bold", fontColour = "white",
    fgFill = header_colour, halign = "center",
    wrapText = TRUE, border = "Bottom", borderStyle = "thin"
  )
  addStyle(wb, sheet_name, style = header_style,
           rows = 1, cols = 1:n_cols, gridExpand = TRUE)
  
  # --- First column (Topic numbers): size 26, bold, fill #EEECE1 ---
  topic_col_style <- createStyle(
    fontSize = 26, textDecoration = "bold",
    fgFill = "#EEECE1", halign = "center", valign = "top",
    wrapText = TRUE
  )
  addStyle(wb, sheet_name, style = topic_col_style,
           rows = 2:(n_rows + 1), cols = 1, gridExpand = TRUE)
  
  # --- Quadrant conditional formatting ---
  quad_col <- which(col_names == "Quadrant")
  
  high_quality_style <- createStyle(fgFill = "#9BBB59", wrapText = TRUE)
  low_quality_style  <- createStyle(fgFill = "#F79646", wrapText = TRUE)
  
  for (r in 2:(n_rows + 1)) {
    val <- data$Quadrant[r - 1]
    if (grepl("^High Quality", val)) {
      addStyle(wb, sheet_name, style = high_quality_style, rows = r, cols = quad_col, stack = TRUE)
    } else if (grepl("^Low Quality", val)) {
      addStyle(wb, sheet_name, style = low_quality_style, rows = r, cols = quad_col, stack = TRUE)
    }
  }
  
  # --- Column widths ---
  for (i in seq_along(col_names)) {
    cn <- col_names[i]
    width <- dplyr::case_when(
      cn == "Topic"                                          ~ 7,
      cn %in% c("Label")                                    ~ 25,
      cn %in% c("FREX_Terms", "Highest_Prob", "Lift")       ~ 55,
      cn == "Direction"                                      ~ 14,
      cn == "Quadrant"                                       ~ 30,
      cn %in% c("Year_Estimate", "P_Value",
                "Coherence", "Exclusivity")                  ~ 14,
      cn %in% c("CV", "CV_Rank")                            ~ 10,
      cn == "Trajectory_Shape"                               ~ 20,
      cn == "Inflection_Points"                              ~ 22,
      cn %in% c("Positive Correlations",
                "Negative Correlations")                      ~ 40,
      cn %in% c("Key_Docs", "Notes")                         ~ 30,
      TRUE                                                    ~ 15
    )
    setColWidths(wb, sheet_name, cols = i, widths = width)
  }
  
  # --- Number formatting ---
  est_col <- which(col_names == "Year_Estimate")
  pv_col  <- which(col_names == "P_Value")
  coh_col <- which(col_names == "Coherence")
  exc_col <- which(col_names == "Exclusivity")
  cv_col  <- which(col_names == "CV")
  
  addStyle(wb, sheet_name, style = number_style,
           rows = 2:(n_rows + 1), cols = est_col, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, style = pval_style,
           rows = 2:(n_rows + 1), cols = pv_col, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, style = score_style,
           rows = 2:(n_rows + 1), cols = coh_col, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, style = score_style,
           rows = 2:(n_rows + 1), cols = exc_col, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet_name, style = cv_style,
           rows = 2:(n_rows + 1), cols = cv_col, gridExpand = TRUE, stack = TRUE)
  
  # --- Wrap text in all data cells ---
  wrap_style <- createStyle(wrapText = TRUE, valign = "top")
  addStyle(wb, sheet_name, style = wrap_style,
           rows = 2:(n_rows + 1), cols = 1:n_cols,
           gridExpand = TRUE, stack = TRUE)
  
  freezePane(wb, sheet_name, firstRow = TRUE, firstCol = TRUE)
}

write_formatted_sheet(wb, "US Topics (K=20)", us_sheet, corpus = "US")
write_formatted_sheet(wb, "EU Topics (K=15)", eu_sheet, corpus = "EU")

saveWorkbook(wb, output_path, overwrite = TRUE)

message(sprintf("\nDone. Saved to: %s", output_path))
message("Fill in Label, Label_Short, Key_Docs, Confidence, and Notes columns manually.")
# =============================================================================
# Shared Configuration for STM Analysis
# =============================================================================

library(here)
library(stm)
library(tidyverse)
library(readxl)
library(quanteda)

# Paths (relative to project root)
metadata_path <- here("metadata.xlsx")
texts_folder <- here("00_Corpus")
output_base <- here()

# Output directories
preprocessing_dir <- here("02_Preprocessing Exploration")
stm_outputs_dir <- here("03_STM_Outputs")
stm_analysis_dir <- here("04_STM_Analysis")
robustness_dir <- here("05_Robustness_Checks")

# =============================================================================
# PHRASE LISTS
# =============================================================================
## SHARED ##

phrases_shared <- c(
  
  # --- Institutions ---
  "european union",                # US=134, EU=228
  "chinese communist party",       # US=72; ABBREV (CCP)
  
  # --- Trade ---
  "trade policy",                  # US=763, EU=203
  "market access",                 # US=560, EU=377
  "trade agreement",               # 
  "trade agreements",              # US=418, EU=126
  "free trade agreement",          # ABBREV (FTA)
  "free trade agreements",         # US=91, EU=35; ABBREV (FTAs)
  "bilateral trade",               # US=132, EU=66
  "global trade",                  # US=140, EU=66
  "world trade",                   # US=175, EU=93
  "free trade",                    # US=358, EU=83
  "trading system",                # US=397, EU=86
  "trading partners",              # US=665, EU=63
  "global trading system",         # US=127, EU=22
  "multilateral trading system",   # US=82, EU=20
  "trans pacific partnership",     # ABBREV (TPP)
  "transatlantic trade investment partnership",  # ABBREV (TTIP)
  "regional comprehensive economic partnership", # ABBREV (RCEP)
  "asian infrastructure investment bank",        # ABBREV (AIIB)
  
  # --- Economy ---
  "intellectual property",         # US=896, EU=151
  "intellectual property rights",  # US=393, EU=91; ABBREV (IPR)
  "supply chain",                  # US=132, EU=57
  "supply chains",                 # US=240, EU=121
  "global economy",                # US=244, EU=114
  "economic growth",               # US=337, EU=70
  "developing countries",          # US=182, EU=74
  "level playing field",           # US=208, EU=114
  "foreign direct investment",     # EU=46; ABBREV (FDI)
  "government procurement",        # US=176, EU=59
  "state owned enterprises",       # ABBREV (SOE)
  "most favored nation",           # ABBREV (MFN)
  
  # --- WTO ---
  "world trade organization",      # US=143, EU=24
  "dispute settlement",            # US=394, EU=88
  "wto dispute settlement",        # US=212, EU=38
  
  # --- Geographic ---
  "hong kong",                     # US=88, EU=33
  
  # --- Technology ---
  "critical technologies",
  "critical technology",           # low ngram freq; genuine compound term
  
  # --- Figures ---
  "von der leyen",                 # EU=18
  "ursula von der leyen",          
  "charles michel",
  "valdis dombrovskis",
  "jean-claude juncker",
  "jose manuel barroso",
  "peter mandelson",
  "cecilia malmstrom",
  "jyrki katainen",
  "donald tusk",
  "wen jiabao",
  "zhu rongji",
  "li keqiang",
  "hu jintao",
  "bo xilai",
  # Note: xi jinping and li keqiang also handled in pre-tokenisation
  
  # --- Misc ---
  "human rights",                  # US=111, EU=220
  "rule of law"                    # US=153, EU=63
)

## US-SPECIFIC ##

phrases_us <- c(
  
  # --- Institutions ---
  "united states",                 # US=5418
  "chinese government",            # US=454
  "chinese companies",             # US=195
  "united states code",            # ABBREV (USC) 
  "committee on foreign investment", # ABBREV (CFIUS)
  
  # --- Political ---
  "president trump",               # US=215
  "president biden",               # US=125
  "president obama",               # US=139
  "president bush",                # US=101
  "biden administration",          # US=177
  "trump administration",          # US=100
  "obama administration",          # US=118
  "bush administration",           # low ngram freq; kept for consistency
  "trade policy agenda",           # US=144
  "national security",             # US=367
  
  # --- WTO ---
  "wto members",                   # US=744
  "wto rules",                     # US=271
  "wto dispute",                   # US=243
  "wto membership",                # US=222
  "wto commitments",               # US=177
  "wto member",                    # US=155
  "wto accession",                 # US=204
  "accession wto",                 # US=175
  "china accession",               # US=147
  "appellate body",                # US=138
  
  # --- Trade ---
  "unfair trade",                  # US=133
  "trade practices",               # US=139
  "unfair trade practices",        # US=96
  "trade representative",          # US=173
  "international trading system",  # US=90
  "information communications technology",  # ABBREV (ICT)
  "permanent normal trade relations",       # ABBREV (PNTR)
  "normal trade relations",                 # ABBREV (NTR)
  "dispute settlement body",                # ABBREV (DSB)
  "asia pacific economic cooperation",      # ABBREV (APEC)
  "trade adjustment assistance",            # ABBREV (TAA)
  "generalized system of preferences",      # ABBREV (GSP)
  
  # --- Economy ---
  "non-market policies",           # US=133
  "non-market approach",           # US=77
  "non-market economy",            # US=87
  "non-market economic",           # US=87
  "excess capacity",               # US=205
  "industrial policies",           # US=186
  "industrial policy",             # US=119
  "exchange rate",                 # US=155
  "technology transfer",           # US=265
  "forced technology transfer",    # US=57
  "forced labor",                  # US=145
  
  # --- Dialogue ---
  "strategic economic",            # US=132
  "economic dialogue",             # US=134
  "strategic economic dialogue",   # US=121; ABBREV (S&ED/SED)
  "phase one",                     # US=131
  "phase one agreement",           # US=115
  "joint commission commerce trade", # ABBREV (JCCT)
  
  # --- Domestic & Sectoral ---
  "american workers",              # US=249
  "agricultural products"          # US=141
)

## EU-SPECIFIC ##

phrases_eu <- c(
  
  # --- Institutions ---
  "european commission",           # EU=113
  "european parliament",           # EU=61
  "member states",                 # EU=431
  "eu member",                     # EU=99
  
  # --- Trade ---
  "trade defence",                 # EU=96
  "trade defence instruments",     # EU=47; ABBREV (TDI)
  "trade defence measures",        # EU=19
  "trade barriers",                # EU=113
  "trade flows",                   # EU=100
  "investment barriers",           # EU=96
  "market access barriers",        # EU=46
  "public procurement",            # EU=89
  "international procurement instrument", # EU=12; ABBREV (IPI)
  "third countries",               # EU=209
  "third country",                 # EU=49
  "single market",                 # EU=83
  "foreign subsidies",             # EU=47
  "export controls",               # EU=39
  
  # --- Economy ---
  "economic security",             # EU=221
  "raw materials",                 # EU=198
  "critical raw",                  # EU=66
  "critical raw materials",        # EU=63
  "rare earth",
  "rare earths",
  "economic coercion",             # EU=32
  "value chains",                  # EU=60
  "market economy",                
  "market economy status",         # EU=13; ABBREV (MES)
  
  # --- Bilateral ---
  "eu-china summit",               # EU=67
  "eu-china relations",            # EU=69
  "comprehensive agreement on investment", # ABBREV (CAI)
  "human rights dialogue",         # EU=25
  
  # --- Policy ---
  "sustainable development",       # EU=126
  "climate change",                # EU=157
  "open strategic autonomy",       # EU=13
  
  # --- WTO ---
  "doha development",              # EU=37
  "doha development agenda"        # EU=29; ABBREV (DDA)
)

## COMBINE ##

phrases_us_full <- c(phrases_shared, phrases_us)
phrases_eu_full <- c(phrases_shared, phrases_eu)

# =============================================================================
# STOPWORDS
# =============================================================================

custom_stops <- c(
  # Common but uninformative conversational/informal terms (with all conjugations to prevent post-stemming artifacts)
  "re", "ve", "ll", "don", "got", "know",
  "want", "wants", "wanted", "wanting",
  "think", "thinks", "thinking", "thought", "thinker", "thinkers", "rethink", "rethinking",
  "said", "just", "get", "see", "say",
  "really", "going", "thing", "things", "lot",
  "yeah", "okay", "well", "like", "weve", "were",
  "its", "dont", "im", "ive", "thats",
  "theres", "isnt", "wasnt", "werent", "hasnt",
  "lets", "youre", "youve", "theyre", "theyve",
  "yes", "course", "sure", "right",
  "also", "can", "will", "one", "two", "new",
  "make", "made", "take", "first", "last",
  "always",
  "thank", "thanks", "thanked", "thanking", "thankful", "thankfully",
  
  # Common speech genre markers
  "however", "many", "include", "includes", "including", "included",
  "cant", "cannot",
  "let", "lets",
  "perhaps",
  "please", "pleases", "pleased", "pleasing", "pleasant", "pleasantly", "pleasure", "pleasures",
  "believe", "believes", "believed", "believing",
  "argue", "argues", "argued", "arguing",
  "something", "frank", "frankly", 
  "rather",
  "talk", "talks", "talking", "talked",
  "kind", "kinds",
  "live", "lives", "living", "lived",
  "much", "give", "gives", "giving", "gave", "given",
  "done", "hard", "harder",
  "sense", "senses",
  "seen", "see", "seeing",
  "ago",
  "still",
  "even",
  "next",
  "come", "comes", "came", "coming",
  "put", "puts", "putting",
  "set", "sets", "setting",
  "shape", "shapes", "shaped", "shaping",
  "way", "ways",
  "back",
  "remember", "remembers", "remembered", "remembering",
  "told", "tell", "tells", "telling",
  "start", "starts", "started", "starting", "stop", "stops", "stopped", "stopping",
  "wait", "waiting", "waited",
  "remind", "reminds", "reminded", "reminding",
  "quit", "quits", "quitting", "quite",
  "write", "writes", "writing", "wrote", "written",
  "surprise", "surprises", "surprised", "surprising",
  "proud", "proudly",
  "anyone", "anybody", "someone", "somebody",
  "sit", "sits", "sitting", "sat",
  "big", "bigger", "biggest",
  "lecture", "lectures", "lectured", "lecturing",
  "happen", "happens", "happened", "happening",
  "point", "points", "pointed", "pointing",
  "shape", "shapes", "shaped", "shaping",
  "full", "fully",
  "long", "longest", "longer",
  
  # Common official testimony genre markers
  "mr", "mrs", "ms", "dr",
  "chairman", "chairwoman", "chairperson",
  "questions", "question", "member", "members", "house",
  "committee", "committees",
  "testify", "testifies", "testified", "testifying", "testimony",
  "hearing", "hearings",
  "look", "looks", "looking", "looked",
  "forward",
  "annex", "annexes",
  "ladies", "gentlemen",
  "staff", "working", "worked", "works",
  "document", "documents",
  "interested", "parties", "party",
  "high", "higher", "highest",
  "level", "levels",
  "took", "take", "takes", "taking", "taken",
  "place", "places",
  "progress",
  "achieve", "achieved", "achieves", "achieving",
  "conclude", "concluded", "concludes", "concluding",
  "initiate", "initiated", "initiates", "initiating",
  "impose", "imposed", "imposes", "imposing",
  "journal", "journals",
  "department", "departments",
  
  # Temporal terms
  
  # Months
  "january", "february", "march", "april", "may", "june",
  "july", "august", "september", "october", "november", "december",
  "jan", "feb", "mar", "apr", "jun", "jul", "aug", "sep", "sept", "oct", "nov", "dec",
  
  # Days
  "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday",
  "mon", "tue", "tues", "wed", "thu", "thur", "thurs", "fri", "sat", "sun",
  
  # Dates
  "week", "weeks", "month", "months", "year", "years", 
  "today", "yesterday", "tomorrow",
  "hour", "hours", "minute", "minutes",
  "date", "now", "then", "before", "after",
  "morning", "afternoon", "evening", 
  "breakfast", "lunch", "dinner",
  "beginning", "begin", "end", "ending",
  
  # Single characters (list markers, abbreviations, artifacts)
  "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
  "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
  
  # Roman numerals
  "ii", "iii", "iv", "vi", "vii", "viii", "ix", "xi", "xii",
  
  # Numbers
  "1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th", "11th", 
  "12th", "13th", "14th", "15th", "16th", "17th", "18th", "19th", "20th", "21st", 
  "22nd", "23rd", "24th", "25th", "26th", "27th", "28th", "29th", "30th", "31st", 
  "32nd", "33rd", "34th", "35th", "36th", "37th", "38th", "39th", "40th", "41st", 
  "42nd", "43rd", "44th", "45th", "46th", "47th", "48th", "49th", "50th", "51st", 
  "52nd", "53rd", "54th", "55th", "56th", "57th", "58th", "59th", "60th", "61st", 
  "62nd", "63rd", "64th", "65th", "66th", "67th", "68th", "69th", "70th", "71st", 
  "72nd", "73rd", "74th", "75th", "76th", "77th", "78th", "79th", "80th", "81st", 
  "82nd", "83rd", "84th", "85th", "86th", "87th", "88th", "89th", "90th", "91st", 
  "92nd", "93rd", "94th", "95th", "96th", "97th", "98th", "99th", "100th",
  
  # Short tokens and artifacts
  "ed", "et", "be", "eg"
)

# =============================================================================
# SHARED FUNCTIONS
# =============================================================================

load_corpus <- function() {
  # Read metadata spreadsheet and remove rows without file names
  metadata <- read_excel(metadata_path) %>%
    filter(!is.na(file_name))
  
  # For each row, construct file path and read text content
  corpus_all <- metadata %>%
    mutate(
      text_path = file.path(texts_folder, paste0(file_name, ".txt")),
      text = map_chr(text_path, ~ {
        if (file.exists(.x)) {
          readLines(.x, warn = FALSE, encoding = "UTF-8") %>%
            paste(collapse = " ")
        } else {
          NA_character_
        }
      })
    ) %>%
    filter(!is.na(text)) # Remove rows where text file was not found
  
  # Report corpus composition
  message("Total documents loaded: ", nrow(corpus_all))
  message("US: ", sum(corpus_all$institution == "US"))
  message("EU: ", sum(corpus_all$institution == "EU"))
  
  return(corpus_all)
}

# Clean text
# corpus argument: "US", "EU", or "shared"
preprocess_corpus <- function(corpus_data, phrases, min_docfreq = 5,
                              min_nchar = 3, corpus = "shared") {
  
  # ---- Text normalisation (pre-tokenisation) ----
  corpus_data <- corpus_data %>%
    mutate(
      # Remove URLs
      text = str_replace_all(text, "https?://[^\\s]+", ""),
      text = str_replace_all(text, "www\\.[^\\s]+", ""),
      
      # Remove separator lines (e.g., "======")
      text = str_replace_all(text, "={2,}", " "),
      
      # =================================================================
      # SHARED ABBREVIATIONS — applied to all corpora
      # =================================================================
      
      # Entity abbreviations to single tokens
      text = str_replace_all(text, "(?i)\\bU\\.?S\\.?C\\.?\\b", "united_states_code"),
      text = str_replace_all(text, "(?i)\\bU\\.?\\s?S\\.?\\b", "united_states"),
      text = str_replace_all(text, "(?i)\\bE\\.?U\\.?\\b", "european_union"),
      text = str_replace_all(text, "(?i)\\bW\\.?T\\.?O\\.?\\b", "world_trade_organization"),
      text = str_replace_all(text, "(?i)\\bP\\.?R\\.?C\\.?\\b", "china"),
      text = str_replace_all(text, "(?i)\\bI\\.?C\\.?T\\.?\\b", "information_communications_technology"),
      
      # Trade abbreviations shared across corpora
      text = str_replace_all(text, "(?i)\\bIPR\\b", "intellectual_property_rights"),
      text = str_replace_all(text, "(?i)\\bFDI\\b", "foreign_direct_investment"),
      text = str_replace_all(text, "(?i)\\bFTAs?\\b", "free_trade_agreements"),
      text = str_replace_all(text, "(?i)\\bMES\\b", "market_economy_status"),
      text = str_replace_all(text, "(?i)\\bDDA\\b", "doha_development_agenda"),
      
      # Hyphenated compounds shared across corpora
      text = str_replace_all(text, "(?i)\\bindo-pacific\\b", "indo_pacific"),
      text = str_replace_all(text, "(?i)\\basia-pacific\\b", "asia_pacific"),
      text = str_replace_all(text, "(?i)\\bstate[- ]owned\\s+enterprises?\\b", "state_owned_enterprises"),
      
      # Handle names
      text = str_replace_all(text, "(?i)\\bxi\\s+jinping\\b", "xijinping"),
      text = str_replace_all(text, "(?i)\\bli\\s+keqiang\\b", "likeqiang"),
      
      # Number-containing terms
      text = str_replace_all(text, "(?i)\\bsection\\s+301\\b", "section_301"),
      text = str_replace_all(text, "(?i)\\bmade\\s+in\\s+china\\s+2025\\b", "made_in_china_2025")
    )
  
  # =================================================================
  # US-SPECIFIC ABBREVIATIONS
  # =================================================================
  if (corpus == "US") {
    corpus_data <- corpus_data %>%
      mutate(
        text = str_replace_all(text, "(?i)\\bJCCT\\b", "joint_commission_commerce_trade"),
        text = str_replace_all(text, "(?i)\\bS&ED\\b", "strategic_economic_dialogue"),
        text = str_replace_all(text, "(?i)\\bSED\\b", "strategic_economic_dialogue"),
        text = str_replace_all(text, "(?i)\\bTTC\\b", "trade_and_technology_council"),
        text = str_replace_all(text, "(?i)\\bCCP\\b", "chinese_communist_party"),
        text = str_replace_all(text, "(?i)\\bSOEs?\\b", "state_owned_enterprises"),
        text = str_replace_all(text, "(?i)\\bCFIUS\\b", "committee_on_foreign_investment"),
        text = str_replace_all(text, "(?i)\\bPNTR\\b", "permanent_normal_trade_relations"),
        text = str_replace_all(text, "(?i)\\bNTR\\b", "normal_trade_relations"),
        text = str_replace_all(text, "(?i)\\bDSB\\b", "dispute_settlement_body"),
        text = str_replace_all(text, "(?i)\\bMFN\\b", "most_favored_nation"),
        text = str_replace_all(text, "(?i)\\bGSP\\b", "generalized_system_of_preferences"),
        text = str_replace_all(text, "(?i)\\bTAA\\b", "trade_adjustment_assistance"),
        text = str_replace_all(text, "(?i)\\bTPP\\b", "trans_pacific_partnership"),
        text = str_replace_all(text, "(?i)\\btrans-pacific\\s+partnership\\b", "trans_pacific_partnership"),
        text = str_replace_all(text, "(?i)\\bT-?TIP\\b", "transatlantic_trade_investment_partnership"),
        text = str_replace_all(text, "(?i)\\btransatlantic\\s+trade\\s+(?:and\\s+)?investment\\s+partnership\\b", "transatlantic_trade_investment_partnership"),
        text = str_replace_all(text, "(?i)\\bRCEP\\b", "regional_comprehensive_economic_partnership"),
        text = str_replace_all(text, "(?i)\\bAIIB\\b", "asian_infrastructure_investment_bank")
      )
  }
  
  # =================================================================
  # EU-SPECIFIC ABBREVIATIONS
  # =================================================================
  if (corpus == "EU") {
    corpus_data <- corpus_data %>%
      mutate(
        text = str_replace_all(text, "(?i)\\bCAI\\b", "comprehensive_agreement_on_investment"),
        text = str_replace_all(text, "(?i)\\bTDI\\b", "trade_defence_instruments"),
        text = str_replace_all(text, "(?i)\\bIPI\\b", "international_procurement_instrument"),
        text = str_replace_all(text, "(?i)\\bCCP\\b", "chinese_communist_party"),
        text = str_replace_all(text, "(?i)\\bSOEs?\\b", "state_owned_enterprises"),
        text = str_replace_all(text, "(?i)\\bMFN\\b", "most_favored_nation"),
        text = str_replace_all(text, "(?i)\\bRCEP\\b", "regional_comprehensive_economic_partnership"),
        text = str_replace_all(text, "(?i)\\bTPP\\b", "trans_pacific_partnership"),
        text = str_replace_all(text, "(?i)\\btrans-pacific\\s+partnership\\b", "trans_pacific_partnership"),
        text = str_replace_all(text, "(?i)\\bAIIB\\b", "asian_infrastructure_investment_bank")
      )
  }
  
  # =================================================================
  # SHARED POST-PROCESSING (encoding, punctuation — all corpora)
  # =================================================================
  corpus_data <- corpus_data %>%
    mutate(
      # Normalise curly quotes and dashes to standard ASCII
      text = str_replace_all(text, "\u2018|\u2019", "'"),
      text = str_replace_all(text, "\u201C|\u201D", '"'),
      text = str_replace_all(text, "\u2014|\u2013", "-"),
      
      # Fix common encoding issues
      text = str_replace_all(text, "ö", "o"),
      text = str_replace_all(text, "Ã¶", "o"),
      text = str_replace_all(text, "ï", "i"),
      text = str_replace_all(text, "Ã¯", "i"),
      text = str_replace_all(text, "naïve", "naive"),
      
      # Handle possessives before tokenisation
      text = str_replace_all(text, "'s\\b", ""),
      text = str_replace_all(text, "s'\\b", "s"),
      text = str_replace_all(text, "'\\b", ""),
      
      # Remove stray punctuation artifacts
      text = str_replace_all(text, "`", ""),
      text = str_replace_all(text, "'{2,}", ""),
      text = str_replace_all(text, "\\.{2,}", " "),
      
      # Normalise e.g. and i.e.
      text = str_replace_all(text, "(?i)\\be\\.?g\\.?\\b", ""),
      text = str_replace_all(text, "(?i)\\bi\\.?e\\.?\\b", "")
    )
  
  # ---- Tokenisation ----
  qcorpus <- corpus(corpus_data, text_field = "text")
  
  toks <- tokens(qcorpus, remove_punct = TRUE, remove_numbers = TRUE,
                 remove_symbols = TRUE, remove_url = TRUE) %>%
    tokens_tolower() %>%
    # Compound multi-word phrases before removing stopwords
    tokens_compound(pattern = phrase(phrases), concatenator = "_") %>%
    tokens_remove(stopwords("en")) %>%
    tokens_remove(custom_stops) %>%
    tokens_wordstem() %>%
    # Remove short tokens (single/double characters)
    tokens_select(min_nchar = min_nchar)
  
  # ---- Convert to document-feature matrix ----
  dfm_corpus <- dfm(toks) %>%
    # Remove rare terms appearing in fewer than min_docfreq documents
    dfm_trim(min_docfreq = min_docfreq)
  
  # ---- Convert to STM format ----
  out <- convert(dfm_corpus, to = "stm")
  out$meta <- corpus_data
  
  # Report final corpus dimensions
  message("Documents: ", length(out$documents))
  message("Vocabulary: ", length(out$vocab))
  
  return(out)
}

message("Configuration loaded successfully.")
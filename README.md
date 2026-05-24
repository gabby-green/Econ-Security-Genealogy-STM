# From Integration to 'De-Coupling': A Genealogy of Economic Security in Western Trade Relations with China

Computational analysis accompanying a Master's thesis examining the evolution of Western 
economic security discourse toward China (2001–2025), using Structural Topic Modeling (STM) 
applied to separate US and EU official trade policy corpora.

---

## Repository Structure
```
├── 00_Corpus/                     # Corpus metadata and document sources
├── 01_Scripts/                    # Core analysis scripts (see below)
├── 02_Preprocessing_Exploration/  # preText diagnostics and preprocessing outputs
├── 03_STM_Outputs/                # Fitted model objects and topic proportion outputs
├── 04_STM_Analysis/               # Post-estimation analysis: temporal effects, KWIC, correlations
├── 05_Robustness_Checks/          # searchK results, seed averaging, model diagnostics
├── 06_preText_Analysis/           # Preprocessing robustness outputs (Denny & Spirling, 2018)
├── metadata.xlsx                  # Document-level metadata for US and EU corpora
└── STM.Rproj                      # R project file
```

## Scripts

| Script | Description |
|--------|-------------|
| `00_configuration.R` | Shared configuration, phrase lists, abbreviation mappings, helper functions |
| `01_preprocessing_exploration.R` | Corpus exploration and preprocessing specification |
| `02_STM.R` | STM model fitting for US (K=20) and EU (K=15) corpora |
| `03_STM_analysis.R` | Post-estimation analysis: temporal trajectories, inter-topic correlations, KWIC |
| `04_robustness_checks.R` | searchK with multi-seed averaging; model diagnostics |
| `05_topic_examination.R` | Representative document retrieval; topic-level deep dives |
| `06_topic_summary.R` | Summary outputs for topic labelling and classification |
| `07_pretext_analysis.R` | preText robustness analysis following Denny & Spirling (2018) |

## Corpora

Two separate corpora of official trade policy discourse, spanning 2001–2025:

- **US corpus** (204 documents): USTR (n=86, 42%), White House (n=45, 22%), Department of 
  Commerce (n=32, 16%), Department of Treasury (n=24, 12%), and Department of State (n=17, 8%). 
  Yields a vocabulary of 2,863 unique terms after preprocessing. Corpus comprises 11 document 
  types; speeches, congressional testimony, strategic documents, and reports form the bulk.
- **EU corpus** (143 documents): European Commission. Yields a vocabulary of 2,565 unique terms 
  after preprocessing. Corpus comprises 10 document types; speeches are the predominant form 
  (n=74, 51.7%).

Both corpora span 2001–2025, with continuous annual coverage across all 25 years. Document 
selection follows criteria established in poststructuralist discourse analysis (Hansen, 2013): 
formal authority over trade policy, clear articulation of Western economic positions vis-à-vis 
China, and wide circulation or prominence. Transatlantic-level documents (G7/8, MSP) are 
excluded from Stage 1 to maintain corpus comparability and are reserved for Stage 2 analysis.

## Dependencies
```r
install.packages(c("stm", "quanteda", "tidyverse", "here", 
                   "openxlsx", "writexl", "igraph"))

# preText from GitHub
devtools::install_github("matthewjdenny/preText")
```

All scripts use relative paths via the `here` package. Open `STM.Rproj` before running.

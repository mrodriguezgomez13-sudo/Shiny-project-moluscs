# Interactive Visualization of Mediterranean Bivalve Mollusks (OBIS)

**Authors:** Mònica Rodríguez Gómez & Celia Vinagre Izquierdo  
**Course:** Anàlisi de dades òmiques — UOC  
**Date:** July 2026  
**Data source:** [OBIS](https://obis.org/) (Ocean Biodiversity Information System)

---

# Mediterranean bivalve mollusk visualization

This project provides an interactive Shiny application for exploring occurrence records of three bivalve mollusk species in the Mediterranean Sea:

- ***Mytilus galloprovincialis*** (Mediterranean mussel)
- ***Ruditapes decussatus*** (Grooved carpet shell)  
- ***Pecten jacobaeus*** (Mediterranean scallop)

The data is **not included** in this repository due to file size and dynamic nature. It is downloaded directly from OBIS using the `robis` package when running the application or analysis scripts.

The project includes:

- **Interactive Shiny app** with dynamic filters by species, year range, and depth
- **Exploratory data analysis** (descriptive statistics, boxplots, histograms, spatial maps)
- **Inferential analysis** (ANOVA, Tukey HSD post-hoc tests, probability distributions)
- **Machine learning** (PCA and k-means clustering to identify ecological niches)
- **Simulations** (normal distribution modeling and Monte Carlo simulation)
- **Complete RMarkdown report** with all analyses and interpretations

## Repository contents

This repository contains **two main RMarkdown documents**:

### 1. `PEC4_shiny.Rmd` — Shiny application code

This file contains the **interactive Shiny application** code. It includes:

- The complete Shiny UI and server logic
- Data download from OBIS using `robis`
- Real-time filtering by species, year range, and depth
- Interactive map of occurrences
- Interactive depth histogram
- Dynamic updates based on user input

**How to use:** Open this file in RStudio and click **"Run Document"** to launch the interactive app.

### 2. `PEC4_mo.Rmd` — Complete analysis report

This file contains the **full research report** with all statistical analyses, including:

- **Context and objectives** of the study
- **Data description** and preprocessing steps
- **Exploratory data analysis** with descriptive statistics and visualizations
- **Inferential statistics**:
  - ANOVA tests for depth, SST, and SSS differences between species
  - Tukey HSD post-hoc comparisons
  - Probability calculations (binomial, normal, Poisson distributions)
- **Simulations** of SST using Monte Carlo methods
- **Machine learning**:
  - Principal Component Analysis (PCA)
  - K-means clustering for ecological niche identification
- **Conclusions** and ecological interpretation of results
- **Interactive visualization** section with Shiny app screenshots

**How to use:** Open this file in RStudio and knit to HTML to generate the complete report.

## Main results

- Clear differences in depth distribution between species (*P. jacobaeus* > *M. galloprovincialis* > *R. decussatus*)
- Significant differences in SST and SSS between species (ANOVA p < 0.001)
- *Mytilus galloprovincialis* shows the widest geographic distribution
- *Pecten jacobaeus* forms a distinct ecological cluster, indicating a more specialized niche
- PCA and clustering reveal that *Ruditapes decussatus* behaves as a generalist species

## Requirements

Open R and run:

```r
# Install CRAN packages
install.packages(c(
  "shiny",
  "dplyr",
  "ggplot2",
  "purrr",
  "tidyr",
  "knitr",
  "GGally",
  "factoextra",
  "prettydoc"
))

# Install BiocManager if needed
if (!require("BiocManager")) install.packages("BiocManager")

# Install Bioconductor packages
BiocManager::install(c(
  "robis",
  "FactoMineR"
))

# =============================================================================
# PEC4_mo.Rmd - Complete Analysis Report for Bivalve Mollusk Study
# Author: Monica Rodriguez Gomez
# Date: January 2026
# =============================================================================

# This RMarkdown document contains the complete analysis of bivalve mollusk
# occurrence records from the Mediterranean Sea.
# =============================================================================


# =============================================================================
# LIBRARIES
# =============================================================================

# Load required libraries for data management, analysis, visualization,
# and document formatting.

library(robis)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(knitr)
library(prettydoc)


# =============================================================================
# 1. CONTEXT AND STUDY OBJECTIVES. DATA
# =============================================================================

## 1.1 Study Context
# -----------------------------------------------------------------------------
# Mediterranean marine ecosystems are characterized by high biodiversity and
# strong environmental heterogeneity, where factors such as depth, distance
# from coast, and oceanographic conditions decisively influence species
# distribution. Within these ecosystems, bivalve mollusks play an essential
# ecological role, participating in trophic cycles, structuring benthic
# bottoms, and acting as bioindicators of environmental conditions
# (Koulouri et al., 2006).
#
# Analysis of marine species occurrence data allows exploration of spatial and
# ecological distribution patterns, evaluation of environmental variables,
# and application of statistical techniques to extract relevant conclusions
# for marine ecology and conservation.
#
# This report applies the concepts and techniques of data analysis covered
# throughout the course through the study of occurrence records of marine
# mollusks in the Mediterranean.

## 1.2 Origin and Source of Data
# -----------------------------------------------------------------------------
# The data used in this report were obtained from a public and freely
# accessible source: Ocean Biodiversity Information System (OBIS), accessed
# using the robis package in R (Ocean Biodiversity Information System Client,
# version 2.11.3), installed from CRAN, to download and manage marine species
# occurrence data directly from the database (Provoost et al., 2022).
# OBIS is an international platform that collects information on the
# distribution and occurrence of marine organisms from multiple scientific
# and institutional sources. Records include taxonomic, geographic, and
# ecological information, such as geographic coordinates, year of observation,
# and associated environmental variables.

## 1.3 Dataset Selection and Justification
# -----------------------------------------------------------------------------
# For the development of this report, we selected three representative
# bivalve mollusk species from the Mediterranean Sea (Voultsiadou et al., 2009):
#
# - Mytilus galloprovincialis (Mediterranean mussel)
# - Ruditapes decussatus (Grooved carpet shell)
# - Pecten jacobaeus (Mediterranean scallop)
#
# The choice of these species is based on their ecological relevance and
# representativeness in the Mediterranean ecosystem, as well as data
# availability in OBIS.

## 1.4 Study Objectives
# -----------------------------------------------------------------------------
# The main objective of this work is to apply biostatistics and bioinformatics
# techniques to analyze occurrence data of Mediterranean marine mollusks.
# Specifically, the following objectives are proposed:
#
# - Explore the spatial distribution of the three selected species from their
#   georeferenced records
# - Compare depth occurrence patterns among the studied species
# - Analyze the ecological variability of species based on available
#   environmental variables
# - Explore the spatial distribution of three bivalve mollusk species in the
#   Mediterranean Sea

# References:
# - Koulouri, P., Dounas, C., Arvanitidis, C., Eleftheriou, A., & Koutsoubas, D.
#   (2006). Molluscan diversity along a Mediterranean soft bottom sublittoral
#   ecotone. Scientia Marina, 70(4), 573–584.
#   https://doi.org/10.3989/scimar.2006.70n4573
# - Provoost, P., Bosch, S., Appeltans, W., & OBIS. (2022). robis: Ocean
#   Biodiversity Information System (OBIS) Client. R package version 2.11.3.
#   https://CRAN.R-project.org/package=robis
# - Voultsiadou, E., Koutsoubas, D., & Achparaki, M. (2009). Bivalve mollusc
#   exploitation in Mediterranean coastal communities: an historical approach.


# =============================================================================
# 2. DATA PROSPECTION AND PREPARATION
# =============================================================================

## 2.1 Data Description
# -----------------------------------------------------------------------------
# Import data for the species to study

# List of selected species
species <- c("Mytilus galloprovincialis", "Ruditapes decussatus", "Pecten jacobaeus")

# Download data from OBIS for each selected species
moluscos_data <- map_dfr(species, occurrence)

# View original data
table(moluscos_data$scientificName)
summary(moluscos_data)

# Check which columns have NA values
colSums(is.na(moluscos_data))

# Latitude and longitude have no NA values.
# Depth does have NA values.
summary(moluscos_data$depth)

# Filter marine records
# Remove records with NA in coordinate columns and depth.
# Also filter values to minimum 0 depth, because in the original values
# we observed that some have min -12m.
moluscos_filtered <- moluscos_data %>%
  filter(marine == TRUE, !is.na(depth), depth >= 0)

# View filtered data
table(moluscos_filtered$scientificName)
summary(moluscos_filtered$depth)

# The downloaded dataset is an R data frame containing a total of 7903 records,
# of which 6773 belong to Mytilus galloprovincialis, 303 to Pecten jacobaeus,
# and 827 records belong to Ruditapes decussatus. Each record includes
# taxonomic, geographic, temporal, and environmental information.
#
# The dataset contains different types of variables, among which those used
# for this report are:
#
# - Categorical variables: variables that describe qualitative characteristics.
#   For example, scientificName indicates the species of the record.
#
# - Numeric variables: quantitative variables that allow statistical analysis
#   and comparisons. Among them we use decimalLatitude and decimalLongitude
#   (geographic coordinates), depth (record depth in meters), sst (sea surface
#   temperature in °C), and sss (sea surface salinity in PSU).
#
# - Logical variables: represent truth values (TRUE/FALSE), such as the marine
#   variable, which indicates whether the record corresponds to a marine
#   environment.
#
# After downloading the data from OBIS, it was checked that some columns had
# null values (NA). For this reason, rows that had NA values in the depth
# variable were removed (4667), as it is the main variable of interest for the
# report. Subsequently, to avoid losing data, the sst, sss, and year variables
# were managed specifically, filtering only in those analyses where these
# variables were necessary.
#
# For depth values, inconsistent values were also observed, with a range from
# -12m to 1125.50m, so records with depth values smaller than 0 were also
# filtered out. It is observed that there is an imbalance in the number of
# records per species, with Mytilus galloprovincialis being the most
# represented (2203), followed by Pecten jacobaeus (207) and Ruditapes
# decussatus the least (74). This could influence statistical analyses,
# especially in comparisons between species, and should be considered when
# interpreting results, as an imbalance in the number of records can bias
# conclusions and reduce the statistical power of applied tests.

## 2.2 Objective Questions
# -----------------------------------------------------------------------------
# - What is the depth distribution of each species, and how are their records
#   related to environmental variables of sea surface temperature (SST) and
#   sea surface salinity (SSS)? Are there significant differences in these
#   variables among the three selected species?
#
# - Does the variability of depth, SST, and SSS differ among species,
#   indicating different environmental tolerances? Which species shows the
#   greatest environmental variability?
#
# - What is the density of records by geographic area (latitude/longitude)?
#
# - Are there significant differences in mean depth and spatial distribution
#   of records among the three species?
#
# - Is there a correlation between record depth and latitude/longitude for
#   each species?


# =============================================================================
# 3. EXPLORATORY DATA ANALYSIS
# =============================================================================

## 3.1 Descriptive and Graphical Analysis
# -----------------------------------------------------------------------------
# Perform a summary of variable information.

# Descriptive summary for depth variable
moluscos_filtered %>%
  group_by(scientificName) %>%
  summarise(
    n = n(),
    depth_min = min(depth),
    depth_max = max(depth),
    depth_mean = mean(depth),
    depth_median = median(depth),
    depth_sd = sd(depth)
  )

# In terms of depth, Mytilus galloprovincialis presents a mean of 26.48m and
# median of 25m, with extreme records from 0m to 1125.5m and a standard
# deviation of 42.07m, reflecting some dispersion and the presence of very
# high values. Pecten jacobaeus has mean 55.92m and median 49.4m, with a
# range from 3m to 131.5m and standard deviation 33.77m, indicating deep
# records that raise the mean. Ruditapes decussatus shows mean 10.36m and
# median 3.5m, with a minimum of 0m and maximum 167.86m, and standard
# deviation 26.21m, indicating that most records are in shallow waters but
# there are some very high values that influence the mean.

# Descriptive summary for environmental variable sst (Sea Surface Temperature)
# Filter the sst variable to remove NA's
moluscos_filtered %>%
  group_by(scientificName) %>%
  summarise(
    n = n(),
    sst_min = min(sst, na.rm = TRUE),
    sst_max = max(sst, na.rm = TRUE),
    sst_mean = mean(sst, na.rm = TRUE),
    sst_median = median(sst, na.rm = TRUE),
    sst_sd = sd(sst, na.rm = TRUE)
  )

# Regarding sea surface temperature (SST), Mytilus galloprovincialis presents
# mean 15.03°C and median 15.27°C, with standard deviation 1.45°C, indicating
# little dispersion and absence of significant extreme values. Pecten jacobaeus
# has mean 17.94°C and median 17.51°C, standard deviation 1.71°C, showing
# moderate variability. Ruditapes decussatus presents mean 16.71°C and median
# 17.43°C, with standard deviation 3.84°C, evidencing greater dispersion and
# some colder values that reduce the mean.

# Descriptive summary for environmental variable sss (Sea Surface Salinity)
# Filter the sss variable to remove NA's
moluscos_filtered %>%
  group_by(scientificName) %>%
  summarise(
    n = n(),
    sss_min = min(sss, na.rm = TRUE),
    sss_max = max(sss, na.rm = TRUE),
    sss_mean = mean(sss, na.rm = TRUE),
    sss_median = median(sss, na.rm = TRUE),
    sss_sd = sd(sss, na.rm = TRUE)
  )

# Regarding sea surface salinity (SSS), Mytilus galloprovincialis has mean
# 21.32PSU and median 17.71PSU, with standard deviation 7.89PSU, reflecting
# great dispersion and the presence of records with high salinities that raise
# the mean. Pecten jacobaeus presents mean 36.02PSU and median 37.07PSU,
# standard deviation 2.31PSU, with less dispersion and some low salinity
# records that reduce the mean. Ruditapes decussatus shows mean 35.86PSU and
# median 35.25PSU, with standard deviation 1.36PSU, indicating that the species
# is found in a relatively uniform salinity range.

### Correlation plot between environmental variables (depth, SST, SSS) by species

# Install GGally if not installed
if (!require(GGally)) install.packages("GGally")
library(GGally)

# Select only complete records for the three variables
corr_data <- moluscos_filtered %>%
  select(scientificName, depth, sst, sss) %>%
  filter(complete.cases(.))

# Define colors manually as in other plots
species_colors <- c(
  "Mytilus galloprovincialis" = "#E377C2",
  "Ruditapes decussatus" = "#1F77B4",
  "Pecten jacobaeus" = "#9467BD"
)

# Pair plot by species with custom colors
g <- ggpairs(
  corr_data,
  columns = 2:4,
  mapping = ggplot2::aes(color = scientificName),
  title = "Correlation between depth, SST, and SSS by species"
)

# Apply colors and legend as in other plots
for(i in 1:g$nrow) {
  for(j in 1:g$ncol) {
    g[i, j] <- g[i, j] + 
      ggplot2::scale_color_manual(values = species_colors, name = "Species") +
      ggplot2::scale_fill_manual(values = species_colors, name = "Species")
  }
}
g <- g + theme(
  legend.position = "bottom",
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10)
)
print(g)

# It is observed that Mytilus galloprovincialis has a moderate positive
# correlation with temperature and a negative one with salinity, while
# Pecten jacobaeus presents a strong positive correlation with salinity,
# and Ruditapes decussatus shows a significant negative correlation with
# temperature, indicating different environmental preferences among species.

### ANOVA

# Convert scientificName column to factor
moluscos_filtered$scientificName <- as.factor(moluscos_filtered$scientificName)

# One-way ANOVA: depth ~ species
anova_depth <- aov(depth ~ scientificName, data = moluscos_filtered)

# Summary of results
summary(anova_depth)

# Post-hoc comparisons
TukeyHSD(anova_depth)

# The analysis of variance (ANOVA) shows that there are significant
# differences in mean depth among the analyzed mollusk species (p < 0.001).
# This indicates that at least one of the species is at a significantly
# different mean depth compared to the others.
#
# Tukey's multiple comparisons show significant differences between species pairs:
#
# - Pecten jacobaeus vs. Mytilus galloprovincialis: difference of 29.44 depth
#   units (p < 0.001)
# - Ruditapes decussatus vs. Mytilus galloprovincialis: difference of -16.11
#   units (p = 0.0026)
# - Ruditapes decussatus vs. Pecten jacobaeus: difference of -45.55 units
#   (p < 0.001)
#
# All comparisons are statistically significant, indicating that the three
# species are at significantly different mean depths.

# ANOVA for sea surface temperature
anova_sst <- aov(sst ~ scientificName, data = moluscos_filtered)
summary(anova_sst)
TukeyHSD(anova_sst)

# Regarding sea surface temperature (SST), it also shows significant
# differences between species (p < 0.001), indicating that at least one
# species is in waters with a significantly different mean temperature.
# Tukey's comparisons reveal:
#
# - Pecten jacobaeus vs. Mytilus galloprovincialis: difference of 2.92°C
#   (p < 0.001)
# - Ruditapes decussatus vs. Mytilus galloprovincialis: difference of 1.68°C
#   (p < 0.001)
# - Ruditapes decussatus vs. Pecten jacobaeus: difference of -1.23°C
#   (p < 0.001)
#
# These results indicate that each species is found in significantly
# different temperature ranges, with Mytilus being the species that inhabits
# more temperate waters, Pecten in warmer waters, and Ruditapes with
# intermediate values.

# ANOVA for sea surface salinity
anova_sss <- aov(sss ~ scientificName, data = moluscos_filtered)
summary(anova_sss)
TukeyHSD(anova_sss)

# Regarding sea surface salinity (SSS), it also reveals significant
# differences between species (p < 0.001). Tukey's comparisons show:
#
# - Pecten jacobaeus vs. Mytilus galloprovincialis: difference of 14.70
#   salinity units (p < 0.001)
# - Ruditapes decussatus vs. Mytilus galloprovincialis: difference of 14.55
#   units (p < 0.001)
# - Ruditapes decussatus vs. Pecten jacobaeus: difference of -0.15 units
#   (p = 0.99)
#
# This indicates that Pecten and Ruditapes present significantly higher
# salinity values than Mytilus, while between Pecten and Ruditapes no
# significant differences are detected. Together, the results confirm that
# the three species present distinct ecological preferences in terms of
# depth, temperature, and salinity, reflecting their specialization in
# different environmental niches.

#### Geographic and descriptive visualization

# Global occurrence map
ggplot(moluscos_filtered,
       aes(x = decimalLongitude, y = decimalLatitude,
           color = scientificName)) +
  borders("world", colour = "grey70", fill = "grey95") +
  geom_point(alpha = 0.5, size = 0.7) +
  scale_color_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  coord_quickmap() +
  labs(
    title = "Occurrences of bivalve mollusks in OBIS",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )

# The species Mytilus galloprovincialis shows a wider distribution compared
# to the other two species, which appear to be more concentrated in certain
# areas of the Mediterranean.

# Map restricted to the Mediterranean
med_data <- moluscos_filtered %>%
  filter(decimalLongitude > -10, decimalLongitude < 40,
         decimalLatitude > 30, decimalLatitude < 46)

ggplot(med_data,
       aes(x = decimalLongitude, y = decimalLatitude,
           color = scientificName)) +
  borders("world",
          xlim = c(-10, 40), ylim = c(30, 46),
          colour = "grey70", fill = "grey95") +
  geom_point(alpha = 0.6, size = 1.5) +
  scale_color_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  coord_quickmap(xlim = c(-10, 40), ylim = c(30, 46)) +
  labs(
    title = "Occurrences in the Mediterranean Sea",
    x = "Longitude",
    y = "Latitude",
    color = "Species"
  )

# In this case Mytilus galloprovincialis is not as concentrated in
# Mediterranean areas, but rather in the Black Sea. Pecten jacobaeus has a
# more localized presence, concentrating mainly on the Italian and French
# northern coast. And Ruditapes decussatus shows a more dispersed
# distribution with lower density of records in the Mediterranean.

### Depth boxplot by species

moluscos_filtered %>%
  ggplot(aes(x = scientificName, y = depth, fill = scientificName)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 20, hjust = 1, size = 10)) +
  labs(
    title = "Depth distribution by species",
    x = "Species",
    y = "Depth (m)"
  )

# The species that on average is in deeper places is Pecten jacobaeus.
# There are several outliers in Mytilus galloprovincialis, which has been
# found at great depths, which may be sampling errors.

### Sea surface temperature (SST) boxplot by species

moluscos_filtered %>%
  ggplot(aes(x = scientificName, y = sst, fill = scientificName)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 20, hjust = 1, size = 10)) +
  labs(
    title = "Sea surface temperature (SST) distribution by species",
    x = "Species",
    y = "Surface temperature (°C)"
  )

# It is observed that Mytilus galloprovincialis inhabits more temperate
# waters, Pecten jacobaeus warmer waters, and Ruditapes decussatus
# intermediate values, in agreement with the ANOVA.

### Sea surface salinity (SSS) boxplot by species

moluscos_filtered %>%
  ggplot(aes(x = scientificName, y = sss, fill = scientificName)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(angle = 20, hjust = 1, size = 10)) +
  labs(
    title = "Sea surface salinity (SSS) distribution by species",
    x = "Species",
    y = "Surface salinity (PSU)"
  )

# It is appreciated that Pecten jacobaeus and Ruditapes decussatus present
# higher salinity values than Mytilus galloprovincialis, in line with the
# ANOVA results.

### Evolution of records by year and species (robust version)

moluscos_filtered$year <- as.factor(moluscos_filtered$year)
reg_year <- moluscos_filtered %>%
  filter(!is.na(year)) %>%
  count(scientificName, year, name = "n_records")

ggplot(reg_year,
       aes(x = as.numeric(as.character(year)), y = n_records,
           color = scientificName, group = scientificName)) +
  geom_line(aes(size = 1)) +
  scale_color_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  scale_x_continuous(breaks = seq(min(as.numeric(as.character(reg_year$year))),
                                  max(as.numeric(as.character(reg_year$year))),
                                  by = 10)) +
  theme_classic(base_size = 10) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  labs(
    title = "Number of records by year and species",
    x = "Year",
    y = "Number of records",
    color = "Species"
  )

ggplot(moluscos_filtered, aes(x = depth, fill = scientificName)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  labs(title = "Histogram of depths by species",
       x = "Depth (m)", y = "Frequency")

# The depth frequencies by species are similar; it is clear that there are
# many more records of Mytilus galloprovincialis than of the other two
# species.


## 3.2 Inference and Simulation Exercises
# -----------------------------------------------------------------------------

### a) From the OBIS dataset with records of the three selected species,
###    classify by each species the probability that a record falls within
###    different depth intervals: depth less than or equal to 10 meters,
###    between 10 and 30 meters, and greater than 30 meters.
###
###    Additionally, for each species, calculate the probability that a
###    record is located in each interval, defined by the proportion of
###    records that fall within the interval relative to the total records
###    of the species.

# Create an empty data.frame to store results
iterative_summary <- data.frame(
  Species = character(),
  MinDepth = numeric(),
  MeanDepth = numeric(),
  MaxDepth = numeric(),
  Category = character(),
  stringsAsFactors = FALSE
)

# Loop through all species
for(sp in species) {
  sp_data <- moluscos_filtered %>% filter(scientificName == sp)
  
  min_depth <- min(sp_data$depth)
  mean_depth <- mean(sp_data$depth)
  max_depth <- max(sp_data$depth)
  
  # Classification by mean depth
  if(mean_depth < 10) {
    category <- "Littoral"
  } else if(mean_depth <= 200) {
    category <- "Neritic"
  } else {
    category <- "Bathyal"
  }
  
  # Add results to data.frame
  iterative_summary <- rbind(iterative_summary, data.frame(
    Species = sp,
    MinDepth = min_depth,
    MeanDepth = mean_depth,
    MaxDepth = max_depth,
    Category = category,
    stringsAsFactors = FALSE
  ))
}

# Show results
print(iterative_summary)

# The table above summarizes the minimum, mean, and maximum depth for each
# species, along with an ecological category based on mean depth:
#
# | Species                   | MinDepth | MeanDepth | MaxDepth | Category |
# |---------------------------|----------|-----------|----------|-----------|
# | Mytilus galloprovincialis | 0        | 26.48     | 1125.50  | Neritic  |
# | Ruditapes decussatus      | 0        | 10.36     | 167.86   | Neritic  |
# | Pecten jacobaeus          | 3        | 55.92     | 131.50   | Neritic  |
#
# **Biological interpretation:**
#
# The three species are mainly found in mid-depth waters (10-200m), although
# Mytilus galloprovincialis and Ruditapes decussatus are close to the
# shallow water threshold.
#
# This classification helps to understand the ecological preferences and
# possible habitat segregation among the studied species.

prob_classified <- function(data, species) {
  
  # Filter data by species
  species_data <- data[data$scientificName == species, ]
  
  # Total number of records
  n_total <- nrow(species_data)
  
  if (n_total == 0) {
    stop("No records for the selected species")
  }
  
  # Define depth intervals
  intervals <- list(
    "<= 10 m"   = species_data$depth <= 10,
    "10-200 m"  = species_data$depth > 10 & species_data$depth <= 200,
    "> 200 m"   = species_data$depth > 200
  )
  
  # Vector for empirical probabilities
  probabilities <- numeric(length(intervals))
  
  # Iteration: calculation of empirical probabilities
  i <- 1
  for (condition in intervals) {
    probabilities[i] <- sum(condition) / n_total
    i <- i + 1
  }
  
  # Output data frame
  result <- data.frame(
    species = species,
    depth_interval = names(intervals),
    empirical_probability = probabilities
  )
  
  return(result)
}

# For "Mytilus galloprovincialis"
prob_classified(
  data = moluscos_filtered,
  species = "Mytilus galloprovincialis"
)

# For Mytilus galloprovincialis, most records are concentrated in depths
# between 10 and 200m (71.27%), with less presence in shallow waters
# (<= 10m, 28.46%) and practically none above 200m (0.27%), but some
# records have been found. This indicates that Mytilus prefers moderate
# depth habitats, although it can also be found in shallow waters.

# For "Ruditapes decussatus"
prob_classified(
  data = moluscos_filtered,
  species = "Ruditapes decussatus"
)

# In Ruditapes decussatus, a clear preference for shallow waters is
# observed, with 81.08% of records at depths <= 10m. Only 18.92% of
# records are found between 10 and 200m and none above 200m. This
# reflects that Ruditapes is strongly associated with shallow bottoms.

# For "Pecten jacobaeus"
prob_classified(
  data = moluscos_filtered,
  species = "Pecten jacobaeus"
)

# Pecten jacobaeus shows a pattern inverse to that of Ruditapes,
# concentrating mainly at depths between 10 and 200m (92.27%), with
# scarce presence in shallow waters (7.73%) and none above 200m. This
# indicates that Pecten prefers moderate depths.


### b) Probability questions

### Exercise 1:

# Probability of being Mytilus galloprovincialis and at depth > 30m
p_myt_depth <- mean(moluscos_filtered$scientificName == "Mytilus galloprovincialis" &
                      moluscos_filtered$depth > 30)
p_myt_depth

# From the OBIS dataset, it has been observed that 34% of records of the
# species Mytilus galloprovincialis are found at depths greater than 30 meters.
#
# If 50 observations of this species are randomly selected, calculate:
#
# a) Probability that exactly 18 records correspond to depths greater than
#    30 meters.

dbinom(18, size = 50, prob = 0.34)

# The probability that exactly 18 records correspond to depths greater than
# 30m is approximately 0.112. This value indicates that, although it is not
# the most probable result, it does represent a reasonably plausible scenario
# within the expected variability when sampling 50 observations of the
# species Mytilus galloprovincialis.

# b) Probability that at least 10 records are found at depths greater than
#    30 meters.

1 - pbinom(9, size = 50, prob = 0.34)

# The probability that at least 10 records are found at depths greater than
# 30m is very high, approximately 0.99. This is consistent with the
# relatively high proportion of deep records observed in the original data.

# c) Probability that at most 25 records are found at depths greater than
#    30 meters.

pbinom(25, size = 50, prob = 0.34)

# The probability that at most 25 records are found at depths greater than
# 30m is also very high, with an approximate value of 0.993. This result
# suggests that it is very unlikely that more than half of the sample is
# below 30m, which reinforces the idea that, although Mytilus galloprovincialis
# has a considerable presence at greater depths, most records remain
# concentrated at shallower depths.


### Exercise 2:

# Sea surface temperature (SST) associated with records of Ruditapes decussatus
# can be approximated by a normal distribution with mean μ = 16.71 °C and
# standard deviation σ = 3.84 °C.

# a) Calculate the probability that a randomly selected record has an SST
#    greater than 20 °C.

1 - pnorm(20, mean = 16.71, sd = 3.84)

# The probability that a randomly selected record presents an SST greater
# than 20°C is 19.6%, suggesting that, although the species tolerates these
# temperatures, they do not constitute the most common range of its thermal
# distribution.

# b) Calculate the probability that the SST is between 16 °C and 22 °C.

pnorm(22, mean = 16.71, sd = 3.84) - pnorm(16, mean = 16.71, sd = 3.84)

# The probability that the SST is between 16°C and 22°C is approximately
# 0.819. This implies that more than 80% of the species records are
# concentrated within this temperature interval, reflecting a clear
# preference for temperate waters and confirming that this thermal range
# represents the most frequent conditions for Ruditapes decussatus.

# c) Graphically represent the density function of this normal distribution
#    and visually indicate the areas corresponding to the previous sections.

# Define the range of values
x <- seq(10, 26, length.out = 1000)

# Density function
y <- dnorm(x, mean = 16.71, sd = 3.84)

# Density plot
plot(x, y, type = "l", lwd = 2,
     main = "Normal distribution of SST",
     xlab = "Sea surface temperature (°C)",
     ylab = "Density")

# Area P(X > 20)
x1 <- seq(20, 26, length.out = 500)
y1 <- dnorm(x1, mean = 16.71, sd = 3.84)
polygon(c(x1, rev(x1)), c(y1, rep(0, length(y1))),
        col = rgb(1, 0, 0, 0.4), border = NA)

# Area P(16 <= X <= 22)
x2 <- seq(16, 22, length.out = 500)
y2 <- dnorm(x2, mean = 16.71, sd = 3.84)
polygon(c(x2, rev(x2)), c(y2, rep(0, length(y2))),
        col = rgb(0, 0, 1, 0.4), border = NA)

# Legend
legend("topright",
       legend = c("P(X > 20)", "P(16 <= X <= 22)"),
       fill = c(rgb(1, 0, 0, 0.4), rgb(0, 0, 1, 0.4)),
       border = NA)

# The figure shows the density function of the normal distribution that
# approximates the sea surface temperature (SST) associated with records of
# Ruditapes decussatus, with mean 16.71°C and standard deviation 3.84°C.
# The curve represents the relative probability of observing different SST
# values for this species.
#
# The blue shaded area corresponds to the interval between 16°C and 22°C.
# Visually, this interval covers most of the density curve, which agrees
# with the result obtained in section b), where it was calculated that
# approximately 81.9% of records fall within this range. This indicates
# that most observations of Ruditapes decussatus are concentrated at
# moderate temperatures close to the distribution mean
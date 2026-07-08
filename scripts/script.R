# =============================================================================
# PEC4_analysis.R - Complete Analysis Script for Bivalve Mollusk Study
# Authors: Monica Rodriguez Gomez and Celia Vinagre Izquierdo
# Date: July 2026
# =============================================================================


# =============================================================================
# 1. LIBRARIES
# =============================================================================

library(robis)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(knitr)
library(prettydoc)
library(GGally)
library(FactoMineR)
library(factoextra)


# =============================================================================
# 2. DATA DOWNLOAD AND PREPARATION
# =============================================================================

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
# Remove records with NA in coordinate columns and in depth.
# Also filter values to minimum 0 depth, because in the original values
# we observed that some have min -12m.
moluscos_filtered <- moluscos_data %>%
  filter(marine == TRUE, !is.na(depth), depth >= 0)

# View filtered data
table(moluscos_filtered$scientificName)
summary(moluscos_filtered$depth)


# =============================================================================
# 3. EXPLORATORY DATA ANALYSIS
# =============================================================================

## 3.1 Descriptive Analysis

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

# Descriptive summary for environmental variable sst (Sea Surface Temperature)
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

# Descriptive summary for environmental variable sss (Sea Surface Salinity)
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

## 3.2 Correlation Plot

# Select only complete records for the three variables
corr_data <- moluscos_filtered %>%
  select(scientificName, depth, sst, sss) %>%
  filter(complete.cases(.))

# Define colors manually
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

# Apply colors and legend
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

## 3.3 ANOVA Analysis

# Convert scientificName column to factor
moluscos_filtered$scientificName <- as.factor(moluscos_filtered$scientificName)

# One-way ANOVA: depth ~ species
anova_depth <- aov(depth ~ scientificName, data = moluscos_filtered)
summary(anova_depth)
TukeyHSD(anova_depth)

# ANOVA for sea surface temperature
anova_sst <- aov(sst ~ scientificName, data = moluscos_filtered)
summary(anova_sst)
TukeyHSD(anova_sst)

# ANOVA for sea surface salinity
anova_sss <- aov(sss ~ scientificName, data = moluscos_filtered)
summary(anova_sss)
TukeyHSD(anova_sss)

## 3.4 Geographic Visualization

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

## 3.5 Boxplots

# Depth boxplot by species
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

# Sea surface temperature (SST) boxplot by species
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

# Sea surface salinity (SSS) boxplot by species
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

## 3.6 Evolution of Records by Year

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

# Histogram of depths by species
ggplot(moluscos_filtered, aes(x = depth, fill = scientificName)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  scale_fill_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  theme_classic(base_size = 10) +
  theme(legend.position = "bottom", legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)) +
  labs(title = "Histogram of depths by species",
       x = "Depth (m)", y = "Frequency")


# =============================================================================
# 4. PROBABILITY AND SIMULATION EXERCISES
# =============================================================================

## 4.1 Depth Classification

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

print(iterative_summary)

# Function for probability classification
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

# Probability classification for each species
prob_classified(moluscos_filtered, "Mytilus galloprovincialis")
prob_classified(moluscos_filtered, "Ruditapes decussatus")
prob_classified(moluscos_filtered, "Pecten jacobaeus")

## 4.2 Binomial Distribution - Exercise 1

# Probability of being Mytilus galloprovincialis and at depth > 30m
p_myt_depth <- mean(moluscos_filtered$scientificName == "Mytilus galloprovincialis" &
                      moluscos_filtered$depth > 30)
p_myt_depth

# a) Probability that exactly 18 records correspond to depths > 30m
dbinom(18, size = 50, prob = 0.34)

# b) Probability that at least 10 records are at depths > 30m
1 - pbinom(9, size = 50, prob = 0.34)

# c) Probability that at most 25 records are at depths > 30m
pbinom(25, size = 50, prob = 0.34)

## 4.3 Normal Distribution - Exercise 2

# a) Probability of SST > 20°C for Ruditapes decussatus
1 - pnorm(20, mean = 16.71, sd = 3.84)

# b) Probability of SST between 16°C and 22°C
pnorm(22, mean = 16.71, sd = 3.84) - pnorm(16, mean = 16.71, sd = 3.84)

# c) Density plot
x <- seq(10, 26, length.out = 1000)
y <- dnorm(x, mean = 16.71, sd = 3.84)

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

legend("topright",
       legend = c("P(X > 20)", "P(16 <= X <= 22)"),
       fill = c(rgb(1, 0, 0, 0.4), rgb(0, 0, 1, 0.4)),
       border = NA)

## 4.4 Poisson Distribution - Exercise 3

# a) Probability of zero records in a cell
dpois(0, lambda = 2)

# b) Probability of at most 3 records in a cell
ppois(3, lambda = 2)

# c) Probability of exactly 5 records in two independent cells
dpois(5, lambda = 4)

## 4.5 SST Simulation

# Basic SST statistics
sst_data <- moluscos_filtered %>%
  select(scientificName, sst)

sst_stats <- sst_data %>%
  group_by(scientificName) %>%
  summarise(
    media = mean(sst, na.rm = TRUE),
    sd = sd(sst, na.rm = TRUE),
    n = n()
  )

# Probability of SST < 12°C
sst_stats <- sst_stats %>%
  mutate(prob_less_12 = pnorm(12, mean = media, sd = sd))

# Probability of SST between 15 and 18°C
sst_stats <- sst_stats %>%
  mutate(prob_15_18 = pnorm(18, mean = media, sd = sd) - pnorm(15, mean = media, sd = sd))

print(sst_stats)

# Simulation for Mytilus galloprovincialis
set.seed(123)

subdata_sst <- moluscos_filtered %>%
  filter(scientificName == "Mytilus galloprovincialis")

mean_sst <- mean(subdata_sst$sst, na.rm = TRUE)
sd_sst <- sd(subdata_sst$sst, na.rm = TRUE)

n_sim <- 1000
sim_sst <- rnorm(n_sim, mean = mean_sst, sd = sd_sst)
sim_sst[sim_sst < 0] <- 0

summary(sim_sst)
mean(sim_sst)
sd(sim_sst)


# =============================================================================
# 5. MACHINE LEARNING - PCA AND CLUSTERING
# =============================================================================

# Prepare numeric variables for PCA and clustering
vars_num <- moluscos_filtered %>%
  select(depth, sst, sss)
vars_num <- na.omit(vars_num)
pca_data <- moluscos_filtered[complete.cases(moluscos_filtered[,c("depth","sst","sss")]),]

# PCA with scaled variables
res_pca <- PCA(vars_num, scale.unit = TRUE, graph = FALSE)

# PCA plot - individuals colored by species
fviz_pca_ind(
  res_pca,
  geom = "point",
  habillage = pca_data$scientificName,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  palette = c("#E377C2", "#1F77B4", "#9467BD"),
  legend = "bottom"
) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    text = element_text(size = 10)
  )

# PCA with axis limits
fviz_pca_ind(
  res_pca,
  geom = "point",
  habillage = pca_data$scientificName,
  addEllipses = TRUE,
  ellipse.level = 0.95,
  palette = c("#E377C2", "#1F77B4", "#9467BD"),
  legend = "bottom"
) +
  xlim(-5, 5) +
  ylim(-4, 4) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    text = element_text(size = 10)
  )

# K-means clustering
set.seed(123)

k <- 3
km_res <- kmeans(scale(vars_num), centers = k, nstart = 25)

# Add cluster to data frame
pca_data$cluster <- factor(km_res$cluster)

# Contingency table: species vs cluster
table(pca_data$scientificName, pca_data$cluster)

# K-means visualization in depth-SST space
ggplot(pca_data,
       aes(x = depth, y = sst,
           color = cluster, shape = scientificName)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    text = element_text(size = 10)
  ) +
  labs(
    title = "Environmental clusters (k-means) in depth-SST space",
    x = "Depth (m)",
    y = "Sea surface temperature (°C)",
    color = "Cluster",
    shape = "Species"
  )

# K-means with x-axis limit (0-300m)
ggplot(pca_data,
       aes(x = depth, y = sst,
           color = cluster, shape = scientificName)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("#E377C2", "#1F77B4", "#9467BD")) +
  xlim(0, 300) +
  theme_classic(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    text = element_text(size = 10)
  ) +
  labs(
    title = "Environmental clusters (k-means) in depth-SST space",
    x = "Depth (m)",
    y = "Sea surface temperature (°C)",
    color = "Cluster",
    shape = "Species"
  )
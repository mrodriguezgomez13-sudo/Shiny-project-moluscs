# Interactive Visualization of Mediterranean Bivalve Mollusks

[![Shiny](https://img.shields.io/badge/Shiny-1.8.0-blue.svg)](https://shiny.rstudio.com/)
[![R](https://img.shields.io/badge/R-4.3.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![OBIS](https://img.shields.io/badge/Data-OBIS-green.svg)](https://obis.org/)

## 📋 Description

Interactive Shiny application for exploring occurrence records of three bivalve mollusk species in the Mediterranean Sea:

- 🐚 ***Mytilus galloprovincialis*** (Mediterranean mussel)
- 🐚 ***Ruditapes decussatus*** (Grooved carpet shell)  
- 🐚 ***Pecten jacobaeus*** (Mediterranean scallop)

Data comes from **OBIS** (Ocean Biodiversity Information System) and was downloaded using the `robis` package.

## Study Objectives

- Explore the spatial distribution of the three selected species
- Compare depth patterns between species
- Analyze ecological variability based on environmental variables
- Interactively visualize georeferenced data

## App Features

- ** Interactive Map**: Visualize the geographic distribution of records
- ** Depth Histogram**: Analyze bathymetric distribution
- ** Dynamic Filters**: Select by species, year range, and depth
- ** Data Table**: View filtered records (if added)

## Installation and Usage

### Prerequisites
- R (>= 4.0.0)
- RStudio (recommended)

### Installation

```bash
# Clone the repository
git clone https://github.com/your-username/moluscos-shiny.git
cd moluscos-shiny

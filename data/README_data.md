# Data Directory

This directory contains the data files used in the Mediterranean bivalve mollusk visualization project.

## Data Source

All data were obtained from **OBIS** (Ocean Biodiversity Information System) using the `robis` R package.

- **Source:** [OBIS](https://obis.org/)
- **Download date:** January 2026
- **Species:** Mytilus galloprovincialis, Ruditapes decussatus, Pecten jacobaeus

## Files

### `moluscos_data.RData`
Processed and cleaned dataset containing occurrence records for the three selected species.

**Contents:**
- `moluscos_filtered`: Data frame with filtered records
- Variables: scientificName, year, depth, decimalLongitude, decimalLatitude, sst, sss
- Records after filtering: 2,484
- Filtering criteria:
  - Marine environment only
  - No NA values in depth
  - Depth >= 0
  - No NA values in coordinates

### `raw_data.RData` 
Raw data downloaded from OBIS before any filtering.

**Contents:**
- `moluscos_data`: Original data frame with all columns from OBIS
- Total records: 7,903

## Loading Data

To load the processed data in R:

```r
load("data/moluscos_data.RData")
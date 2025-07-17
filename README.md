# The land cover paradox: Characteristics of blue- and green spaces within and beyond high-risk suicide clusters

This repository contains the code, data, and documentation for a comprehensive spatial epidemiology study examining the relationship between urban greenspace characteristics, environmental and sociodemographic factors, and suicide risk in Chicago census block groups (CBGs) from 2023–2024. The project combines descriptive epidemiology, spatial statistics, Bayesian modeling, and geospatial data science to identify and interpret neighborhood-level suicide risk patterns.
[Full text available via PubMed Central](https://pmc.ncbi.nlm.nih.gov/articles/PMC12169782/)

## Project Overview

**Objective**: To model and map spatial variation in suicide incidence across Chicago using land cover diversity, park access, tree equity, social vulnerability, and other built-environment indicators.

**Methods**:
- Descriptive epidemiology and visual analytics of Medical Examiner death data
- Spatial Poisson and BYM models (INLA and CARBayes)
- Interaction testing and exceedance probability mapping
- Geovisualizations of high-risk clusters and built environment characteristics
- Partial dependence plots (PDPs) to evaluate marginal and moderated effects

## Data Sources

- Cook County Medical Examiner Suicide Deaths (2023–2024)
- NLCD Land Cover and Land Cover Diversity
- Urban Tree Canopy and Tree Equity Index
- Park Priority Index and park access buffers
- SafeGraph Travel Times to Amenities
- Area Deprivation Index (ADI), American Community Survey (ACS)
- Custom shapefiles for environmental exposures

## Key Features

- **Classification of Suicide Mechanisms**: Categorized deaths by means (e.g., gunshot, hanging, overdose) using regex-based NLP on narrative fields.
- **Land Cover Diversity Index**: Simpson’s Diversity Index calculated across 7 NLCD categories and undeveloped land.
- **Built Environment Indicators**: Tree canopy, park acreage within 15 minutes, tree and park priority indices.
- **Environmental Stressors**: Heat anomalies, temperature, unemployment, linguistic isolation, and health burden.
- **Modeling Framework**:
  - Poisson GLMs for baseline associations
  - Spatial autocorrelation (Moran’s I, LISA clusters)
  - Spatial BYM models using INLA
  - Sensitivity-tested spatial models using CARBayes
  - Exceedance probability maps for identifying high-risk CBGs
- 📊 **Interaction Effects**: Examined how land cover diversity moderates open space, tree equity, blue space, poverty, and racial composition effects.

## Output Tables and Figures

- `tableone.docx`: Descriptive summary of environmental variables.
- `DescriptiveSuicides.docx`: Stratified summary of suicide cases by year.
- `tabletwo.docx`: Environmental and demographic comparison of high-risk vs non-risk areas.
- `tablethree1.docx`: Descriptive table for unjoined environmental data.
- `inla_interaction_results.docx`: Summary table of all INLA interaction terms.
- `ind_comparison_with_highrisk.docx`: Tract-level comparison by risk category.

## Visualizations

- Spatial maps of:
  - Suicide case points
  - Land cover and tree canopy
  - Exceedance probabilities at thresholds 1.0, 1.25, and 1.5
  - Local Moran’s I and high/low risk clusters
- PDPs showing:
  - Tree priority effect across tree equity quartiles
  - Land cover diversity effect by open space and tree canopy quartiles
- Network and correlation matrix visualizations of covariate structure

## Modeling Details

- Offset: Log of population * 2
- Models: BYM spatial models with and without interaction terms
- Fitted using:
  - `INLA` for structured spatial random effects
  - `CARBayes` for comparison using MCMC
- Diagnostics: VIF, multicollinearity checks, posterior distributions

## Dependencies

The following R packages were used:

```r
library(dplyr)
library(tidyr)
library(ggpubr)
library(RColorBrewer)
library(sf)
library(tmap)
library(purrr)
library(ggplot2)
library(sociome)
library(INLA)
library(spdep)
library(patchwork)
library(car)
library(flextable)
library(gtsummary)
library(stringr)
library(Hmisc)
library(brinla)
library(CARBayes)
library(igraph)
library(ggraph)


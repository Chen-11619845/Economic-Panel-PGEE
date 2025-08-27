# Economic-Panel-PGEE

This repository contains the R code and scripts for the empirical analysis of high-dimensional economic panel data using **Generalized Estimating Equations (GEE)** and **Penalized GEE (PGEE)**.  

The project explores both **continuous outcomes (GDP growth rates)** and **binary outcomes (recession indicators)**, with a focus on model performance and variable selection in macroeconomic forecasting.

---

## File Structure

- **1-global_env_data.R** : Load and preprocess global environment data.  
- **2-domestic_data.R** : Preprocess domestic macroeconomic data.  
- **3-continuous_model.R** : Continuous GDP growth analysis with GEE and PGEE.  
- **4-binary_model.R** : Recession forecasting using binary response models.  

---

## Data Sources

The macro-financial panel dataset was compiled from publicly available sources, including:  
- **IMF** – International Financial Statistics (IFS)  
- **BIS** – Bank for International Settlements Statistics Warehouse  
- **ILO** – ILOSTAT Database  

 *Note*: Original raw data files are not included in this repository due to size and license restrictions. Please access them directly from the official databases.

---

## Requirements

The analysis is implemented in **R**. Main packages used include:  

```r
library(tidyverse)
library(geepack)
library(PGEE)
library(kableExtra)
library(pROC)

# Nonagenarian men and women in Switzerland at the end of the 19th century: Inspecting verified outliers at the top end of the historical age distribution

## Paper

submitted

## Data

The data is public available via Zenodo:
<br >
<br >
to add

## Content of this repository

### Structure

```
.
+-- R
+-- data
+-- output

```

### `R` folder 

This folder contains all R scripts.
 
  - `Figure1.R` : code to create Figure 1
  - `Figure2.R` : code to create Figure 2
  - `Figure3.R` : code to create Figure 3
  - `Supplement_Figure6.R`  : code to create Supplement Figure 6
  - `Supplement_Figure7.R`  : code to create Supplement Figure 7
  - `data_obs_exp_sex.R`  : Code to create a table with observed and expected values for sex
  - `data_obs_exp_urbanity.R`  : Code to create a table with observed and expected values for urbanity
  - `data_obs_exp_language.R`  : Code to create a table with observed and expected values for language regions
  - `data_obs_exp_altitude.R`  : Code to create a table with observed and expected values for altitude groups
  - `Poisson_Regression.R` : code of the Poission regression models  (Table 3)
  - `Negativebinomial_Regression.R`  : code of the negative biniomial regression models (Table 4)
  
### `data` folder

  - `data_age90.RData`: spatial data for each person >=90 years old 
  - `Population_sex.xlsx` : contingency table for sex
  - `Population_urbanity.xlsx` : contingency table for urbanity
  - `Population_Altitude.xlsx` : contingency table for altitude
  - `Language_district_1888_1900.xlsx` : language regions for each district
  - `Cofactors.xlsx` : data and explanatory variables for the negative binomial regression models
  - `Hotspot.xlsx` : data for Figure 3 (Spatial Analysis)
  - `Age_distribution.xlsx` : data for Figure 2 (Age distribution)
  - `Age_distribution.xlsx` : data for Figure 2 (Age distribution)
 
### `output` folder

This folder contains figures of the publication.

### `master.R` 

This skript contains information of the used R packages, R scripts, plotting parameters etc..

[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]

[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png

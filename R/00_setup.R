library(tidyverse)
library(openxlsx)
library(raster)
library(sf)
library(sp)
library(viridis)
library(scales)
library(cowplot)
library(rmarkdown)
library(kableExtra)
library(lubridate)
library(robmixglm)
library(MASS)
# library(introdataviz)
library(spdep)
library(tmap)
library(tmaptools)


library(conflicted)


#
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("recode", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarise", "dplyr")



col4magma <- viridis(4, alpha = 1, begin = 1, end = 0, direction = 1, option = "magma")
col3magma <- col4magma[c(4,3,2)]
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
col3grey <- c("#bdbdbd","#f0f0f0", "#737373")



brk_alt <- c(0,300,600,900,1200,1500,1800,2200)
brk_alt_reg <- c(0,600,2200)
brk_alt_poisson <- c(0,600,1000,Inf)
brk_age <- c(90,92,94,96,102)

size_legend <- 30
size_ggtitle <- 35
size_striptext <- 30
legend_size_title <- 1.5
main_size_map <- 2
legend_size_map <- 1.1

# load Skripts
source("R/Figure1.R")
source("R/Figure2.R")
source("R/Figure3.R")
source("R/Supplement_Figure6.R")
source("R/Supplement_Figure7.R")
source("R/data_obs_exp_sex.R")
source("R/data_obs_exp_urbanity.R")
source("R/data_obs_exp_language.R")
source("R/data_obs_exp_altitude.R")
source("R/r01_poisson_regression.R")
source("R/Negativebinomial_Regression.R")

# run functions fpr Poisson regressions

function_poisson2_regression(1888, "Sex")
function_poisson_regression(1888, "Urbanity")
function_poisson_regression(1888, "Language")
function_poisson_regression(1888, "Altitude")

function_poisson_regression(1900, "Sex")
function_poisson_regression(1900, "Urbanity")
function_poisson_regression(1900, "Language")
function_poisson_regression(1900, "Altitude")


# run functions fpr Poisson regressions

suppressWarnings(function_regression_nb(1888))
suppressWarnings(function_regression_nb(1900))
# 




  
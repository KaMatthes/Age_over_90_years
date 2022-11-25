.libPaths(c("H:/Documents/R/win-library/4.1", "C:/Program Files/R/R-4.2.1/library"))

library(tidyverse)
library(dplyr)
library(readxl)
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
library(introdataviz)
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


# library(INLA)
# library(inlabru)


# 
# col7 <- c("#ffffb2","#c7e9b4","#7fcdbb","#41b6c4","#225ea8", "#253494","#081d58")
# col7dark <- c("#b35806",
#   "#e08214",
#   "#fdb863",
#   # "#fee0b6",
#   "#d8daeb",
#   "#b2abd2",
#   "#8073ac",
#   "#542788")
# 
# col7green <- c("#c7e9c0",
#   "#a1d99b",
#   "#74c476",
#   "#41ab5d",
#   "#238b45",
#   "#006d2c",
#   "#00441b")
# 
# col7blue <- c(
# "#c7e9b4",
# "#7fcdbb",
# "#41b6c4",
# "#1d91c0",
# "#225ea8",
# "#253494",
# "#081d58")
# 
# col7pink <- c("#feebe2",
#   "#fcc5c0",
#   "#fa9fb5",
#   "#f768a1",
#   "#dd3497",
#   "#ae017e",
#   "#7a0177")
# 
# col7red <- c("#fee5d9",
#   "#fcbba1",
#   "#fc9272",
#   "#fb6a4a",
#   "#ef3b2c",
#   "#cb181d",
#   "#99000d")
# 
# col7grey <- c("#d9d9d9",
#   "#bdbdbd",
#   "#969696",
#   "#737373",
#   "#525252",
#   "#252525",
#   "#000000")

col3grey <- c("#bdbdbd","#f0f0f0", "#737373")
col7magma <- viridis(7, alpha = 1, begin = 1, end = 0, direction = 1, option = "magma")
col4magma <- viridis(4, alpha = 1, begin = 1, end = 0, direction = 1, option = "magma")
col3magma <- col4magma[c(4,3,2)]
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
# col7viridis <- viridis(7, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
# col7rocket <- viridis(7, alpha = 1, begin = 1, end = 0, direction = 1, option = "rocket")
# col7cividis <- viridis(7, alpha = 1, begin = 1, end = 0, direction = 1, option = "cividis")

brk_alt <- c(0,300,600,900,1200,1500,1800,2200)
brk_alt_reg <- c(0,600,2200)
brk_alt_poisson <- c(0,600,1000,Inf)
# brk_alt_poisson_sen <- c(0,600,900,Inf)
brk_age <- c(90,92,94,96,102)

size_legend <- 15
size_ggtitle <- 35
size_striptext <- 25
legend_size_title <- 1.5
main_size_map <- 2
legend_size_map <- 1.1

# source("R/maps.R")
source("R/maps_age.R")
source("R/Plot_Age.R")
source("R/Sex_diff.R")
source("R/Urbanity_diff.R")
source("R/Language_diff.R")
source("R/Altitude_diff.R")
source("R/Altitude_diff_sen.R")
source("R/Altitude_female_diff.R")
source("R/Altitude_male_diff.R")
source("R/Altitude_sex_diff.R")
source("R/Regression_Districts.R")
source("R/Supplement_Scatter.R")
source("R/Supplement_violin.R")
# source("R/Regression_AgeDead.R")
# source("R/Spacial_Point_Process_Age.R")
  

render(paste0("R/Age_over_90_years.Rmd"), output_file = paste0("../output/",today(),"_Age_over_90_years.html"))
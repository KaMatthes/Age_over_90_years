.libPaths(c("H:/Documents/R/win-library/4.1", "C:/Program Files/R/R-4.1.2/library"))

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
# col7viridis <- viridis(7, alpha = 1, begin = 1, end = 0, direction = 1, option = "viridis")
# col7rocket <- viridis(7, alpha = 1, begin = 1, end = 0, direction = 1, option = "rocket")
# col7cividis <- viridis(7, alpha = 1, begin = 1, end = 0, direction = 1, option = "cividis")

brk_alt <- c(0,300,600,900,1200,1500,1800,2200)
brk_alt_reg <- c(0,600,2200)

size_legend <- 20
size_ggtitle <- 35


source("R/maps.R")
source("R/Language_diff.R")
source("R/Regression_AgeDead.R")
  

render(paste0("R/Age_over_90_years.Rmd"), output_file = paste0("../output/",today(),"_Age_over_90_years.html"))
rm(list=ls())
source("R/00_setup.R")

load("data/data_age90.RData")

dat.pop.t <- read_xlsx(paste0("data/Population_sex.xlsx")) %>%
    mutate(
      Year = as.factor(Year)
      ) %>%
    group_by(Year) %>%
    summarise(Pop_total = sum(Population)) %>%
    ungroup()

dat.pop <- read_xlsx(paste0("data/Population_sex.xlsx")) %>%
    mutate(
      Year = as.factor(Year)
      ) %>%
    full_join(dat.pop.t) %>%
    group_by(Sex, Year) %>%
    mutate(Population = sum(Population)) %>%
    distinct(Year, Sex, .keep_all = TRUE)
  
data.expected_y <- data_age90 %>%
    select(Erhebungsjahr) %>%
    rename(Year = Erhebungsjahr) %>%
    group_by(Year) %>%
    count() %>%
    ungroup() %>%
    rename(Number =n) %>%
    mutate(Year = as.factor(Year))

data.obs.exp.sex <- dat.pop  %>%
    left_join(  data.expected_y ) %>%
    mutate(Perc = (Population/Pop_total)*100,
           Expected = (Number*Perc)/100) %>%
    select(Year, Sex, Observed, Expected, Population, Pop_total, Number, Perc)
  

save( data.obs.exp.sex ,file=paste0("data/data.obs.exp.sex.RData"))
write.xlsx( data.obs.exp.sex ,file=paste0("data/data.obs.exp.sex.xlsx"),rowNames=FALSE, overwrite = TRUE)
  
 

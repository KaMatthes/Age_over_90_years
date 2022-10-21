function_obs_exp_urbanity <- function() {

  
  dat.pop.t <- readxl::read_excel("data_raw/Population_urbanity.xlsx", sheet="Sheet1") %>%
    mutate(Year = as.factor(Year)) %>%
    group_by(Year) %>%
    summarise(Pop_total = sum(Population)) %>%
    ungroup()
  
  dat.pop <- readxl::read_excel("data_raw/Population_urbanity.xlsx", sheet="Sheet1") %>%
    mutate(Year = as.factor(Year)) %>%
    full_join(dat.pop.t) %>%
    group_by(Urbanity, Year) %>%
    mutate(Population = sum(Population)) %>%
    distinct(Year, Urbanity, .keep_all = TRUE)
  
  data.expected_y <- data_age90 %>%
    dplyr::select(Erhebungsjahr) %>%
    rename(Year = Erhebungsjahr) %>%
    group_by(Year) %>%
    count() %>%
    ungroup() %>%
    rename(Number =n) %>%
    mutate(Year = as.factor(Year))
  

  data.obs.exp.urbanity <- dat.pop  %>%
    left_join(  data.expected_y ) %>%
    mutate(Perc = (Population/Pop_total)*100,
           Expected = (Number*Perc)/100) %>%
    dplyr::select(Year, Urbanity, Observed, Expected, Population, Pop_total, Number, Perc)
  

  save(  data.obs.exp.urbanity ,file=paste0("data/data.obs.exp.urbanity.RData"))
  write.xlsx(  data.obs.exp.urbanity ,file=paste0("data/data.obs.exp.urbanity.xlsx"),rowNames=FALSE, overwrite = TRUE)
  
}
  
 

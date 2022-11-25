  load("data/data_age90.RData")
  
  
  dat.pop.t <- readxl::read_excel(paste0("data/Population_sex.xlsx")) %>%
    mutate(Year = as.factor(Year)) %>%
    group_by(Year) %>%
    summarise(Pop_total = sum(Population)) %>%
    ungroup()

  
  dat.pop <- readxl::read_excel(paste0("data/Population_sex.xlsx")) %>%
    mutate(Year = as.factor(Year)) %>%
    full_join(dat.pop.t) %>%
    group_by(Sex, Year) %>%
    mutate(Population = sum(Population)) %>%
    distinct(Year, Sex, .keep_all = TRUE)
  
  data.expected_y <- data_age90 %>%
    dplyr::select(Erhebungsjahr) %>%
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
    dplyr::select(Year, Sex, Observed, Expected, Population, Pop_total, Number, Perc)
  

  save( data.obs.exp.sex ,file=paste0("data/data.obs.exp.sex.RData"))
  write.xlsx( data.obs.exp.sex ,file=paste0("data/data.obs.exp.sex.xlsx"),rowNames=FALSE, overwrite = TRUE)
  
 

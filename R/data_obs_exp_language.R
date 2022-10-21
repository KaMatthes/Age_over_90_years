function_obs_exp_language <- function() {
  load("data/data_age90.RData")
  
  dat.pop <- readxl::read_excel("data_raw/Population_district_1888_1900.xlsx", sheet="Sheet1") %>%
    gather(., Year, population, `1888`:`1900`, factor_key=TRUE) %>%
    group_by(Language, Year) %>%
    summarise(Pop=sum(population)) %>%
    ungroup() %>%
    mutate(Year = as.factor(Year))
  
  dat.pop.t <- readxl::read_excel("data_raw/Population_district_1888_1900.xlsx", sheet="Sheet1") %>%
    gather(., Year, population, `1888`:`1900`, factor_key=TRUE) %>%
    group_by(Year) %>%
    summarise(Pop_total=sum(population)) %>%
    ungroup() %>%
    mutate(Year = as.factor(Year))
  dat.pop <-  dat.pop  %>%
    full_join(dat.pop.t)
  
  dat.pop.diff.t <- readxl::read_excel("data_raw/Population_Age1888.xlsx", sheet="Districts") %>%
    filter(Agegroups=="Total") %>%
    group_by(Language) %>%
    summarise(Pop_UniGe=sum(Total)) %>%
    ungroup() %>%
    mutate(Year=1888,
           Year = as.factor(Year)) %>%
    right_join(dat.pop)
  
  dat.pop.diff.y <- readxl::read_excel("data_raw/Population_Age1888.xlsx", sheet="Districts") %>%
    filter(Agegroups=="Total") %>%
    summarise(Pop_UniGe_t=sum(Total)) %>%
    ungroup() %>%
    mutate(Year=1888,
           Year = as.factor(Year)) %>%
    right_join( dat.pop.diff.t)
  
  
  dat.pop.diff.90.t <- readxl::read_excel("data_raw/Population_Age1888.xlsx", sheet="Districts") %>%
    filter(Agegroups=="90-95" |Agegroups==">=95" ) %>%
    summarise(Number_UniGe=sum(Total)) %>%
    ungroup() %>%
    mutate(Year=1888,
           Year = as.factor(Year))

  dat.pop.diff.90 <- readxl::read_excel("data_raw/Population_Age1888.xlsx", sheet="Districts") %>%
    filter(Agegroups=="90-95" |Agegroups==">=95" ) %>%
    group_by(Language) %>%
    summarise(Observed_UniGe=sum(Total)) %>%
    ungroup() %>%
    mutate(Year=1888,
           Year = as.factor(Year)) %>%
    full_join(dat.pop.diff.90.t)
  
  dat.dist1888 <-  readxl::read_excel("data_raw/Age_distribution_1888_1900.xlsx", sheet="1888")  %>%
    mutate(Year = 1888,
           Number_AgeDist = Frauen + Männer,
           Pop_AgeDist =  2922897) %>%
    dplyr::select(-Frauen, -Männer)
    
  dat.dist1900 <-  readxl::read_excel("data_raw/Age_distribution_1888_1900.xlsx", sheet="1900") %>%
    mutate(Year = 1900,
           Number_AgeDist = Frauen + Männer,
           Pop_AgeDist =  3318985) %>%
    dplyr::select(-Frauen, -Männer)
  
  dat.dist <- rbind(dat.dist1888, dat.dist1900) %>%
    mutate(Year= as.factor(Year)) %>%
    filter(Alter >89) %>%
    group_by(Year, Pop_AgeDist) %>%
    summarise(Number_AgeDist= sum(Number_AgeDist)) %>%
    ungroup() %>%
    full_join(  dat.pop.diff.y) %>%
    full_join(dat.pop.diff.90) 
  # %>%
  #   mutate(Perc =  (Number/Pop)*100,
  #          French_Perc =  (French/Pop)*100,
  #          German_Perc =  (German/Pop)*100,
  #          Italian_Perc = (Italian/Pop)*100,
  #          Year=as.factor(Year)) 
    
  data.expected <- data_age90 %>%
    dplyr::select(Erhebungsjahr) %>%
    group_by(Erhebungsjahr) %>%
    count() %>%
    ungroup() %>%
    rename(Year= Erhebungsjahr,
           Number_Alessia =n) %>%
    mutate(Year = as.factor(Year)) %>%
    full_join(dat.dist) %>%
    mutate(Perc = (Pop/Pop_total)*100,
           Expected = (Number_Alessia*Perc)/100,
           Perc_UniGe = (Pop_UniGe/Pop_UniGe_t)*100,
           Expected_UniGe = (Number_UniGe* Perc_UniGe)/100)
  
  data.obs.exp.language <- data_age90 %>%
    dplyr::select(Wohnkanton, Erhebungsjahr) %>%
    mutate(Language = Wohnkanton,
           Language = as.character(Language),
           Language = recode(Language,
                             "Aargau" = "German",
                             "AppenzellA.-Rh." = "German",
                             "AppenzellI.-Rh." = "German",
                             "Basel-Stadt" = "German",
                             "Basel" = "German",
                             "Basel-Landschaft" = "German",
                             "Bern" = "German",
                             "Freiburg" = "French",
                             "Genf" = "French",
                             "Glarus" = "German",
                             "Graubünden" = "German",
                             "Luzern" = "German",
                             "Neuenburg" = "French",
                             "Nidwalden" = "German",
                             "Obwalden" = "German",
                             "Schaffhausen" ="German",
                             "Schwyz" = "German",
                             "Solothurn" = "German",
                             "St.Gallen" ="German",
                             "Tessin" = "Italian",
                             "Thurgau" = "German",
                             "Uri" = "German",
                             "Waadt" = "French",
                             "Wallis" ="French",
                             "Zug" = "German",
                             "Zürich" = "German"),
           Language = as.character(Language)) %>%
    rename (Year = Erhebungsjahr) %>%
    group_by(Year, Language) %>%
    count() %>%
    ungroup() %>%
    rename(Observed = n) %>%
    mutate(Year = as.factor(Year)) %>%
    full_join(data.expected) %>%
    dplyr::select(Year, Language,Number_Alessia,Pop, Pop_total, Perc, Observed, Expected, Number_UniGe, Pop_UniGe_t, Pop_UniGe,Perc_UniGe, Observed_UniGe, Expected_UniGe,
           Pop_AgeDist, Number_AgeDist)
  
  save(data.obs.exp.language,file=paste0("data/data.obs.exp.language.RData"))
  write.xlsx(data.obs.exp.language,file=paste0("data/data.obs.exp.language.xlsx"),rowNames=FALSE, overwrite = TRUE)
  
}
  
 

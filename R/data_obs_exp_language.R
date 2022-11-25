load("data/data_age90.RData")

  dat.pop <- readxl::read_excel("data/Population_district_1888_1900.xlsx") %>%
    group_by(Language, Year) %>%
    summarize(Population=sum(Population)) %>%
    ungroup() %>%
    mutate(Year = as.factor(Year))
  
  dat.pop.t <-readxl::read_excel("data/Population_district_1888_1900.xlsx") %>%
    group_by(Year) %>%
    summarise(Pop_total=sum(Population)) %>%
    ungroup() %>%
    mutate(Year = as.factor(Year))
  
  dat.pop <-  dat.pop  %>%
    full_join(dat.pop.t)

    
  data.expected <- data_age90 %>%
    dplyr::select(Erhebungsjahr) %>%
    group_by(Erhebungsjahr) %>%
    count() %>%
    ungroup() %>%
    rename(Year= Erhebungsjahr,
           Number =n) %>%
    mutate(Year = as.factor(Year)) %>%
    full_join(dat.pop) %>%
    mutate(Perc = (Population/Pop_total)*100,
           Expected = (Number*Perc)/100)
  
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
    dplyr::select(Year, Language, Observed, Expected, Population, Pop_total, Number, Perc)
  
  save(data.obs.exp.language,file=paste0("data/data.obs.exp.language.RData"))
  write.xlsx(data.obs.exp.language,file=paste0("data/data.obs.exp.language.xlsx"),rowNames=FALSE, overwrite = TRUE)
  
  
 

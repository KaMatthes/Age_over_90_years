  load("data/data_age90.RData")
  
  dat.pop.t <- readxl::read_excel("data/Population_Altitude.xlsx", sheet="Sheet1") %>%
    mutate(Year = as.factor(Year)) %>%
    group_by(Year) %>%
    summarise(Pop_total = sum(Population)) %>%
    ungroup()
  
  dat.pop <- readxl::read_excel("data/Population_Altitude.xlsx", sheet="Sheet1") %>%
    mutate(Year = as.factor(Year)) %>%
    full_join(dat.pop.t) %>%
    mutate(Altitude = ifelse(Altitude=="< 400",  "< 600",Altitude),
           Altitude = ifelse(Altitude=="400-599",  "< 600",Altitude)) %>%
    group_by(Altitude, Year) %>%
    mutate(Population = sum(Population)) %>%
    distinct(Year, Altitude, .keep_all = TRUE)
  
  
  data.expected_y <- data_age90 %>%
    dplyr::select(Erhebungsjahr) %>%
    rename(Year = Erhebungsjahr) %>%
    group_by(Year) %>%
    count() %>%
    ungroup() %>%
    rename(Number =n) %>%
    mutate(Year = as.factor(Year))
  

  data.obs.exp.altitude <- data_age90 %>%
    dplyr::select(Erhebungsjahr,W_Hoehe) %>%
    mutate( W_Hoehe_cat = cut(W_Hoehe,breaks=brk_alt_poisson,include.lowest = TRUE,right = FALSE),
            W_Hoehe_cat = recode( W_Hoehe_cat,
                                  "[0,600)" = "< 600",
                                  "[600,1e+03)" = "600-999",
                                  "[1e+03,Inf]" = ">= 1000")) %>%
    rename(Year = Erhebungsjahr,
           Altitude= W_Hoehe_cat) %>%
    group_by(Year,Altitude) %>%
    count() %>%
    ungroup() %>%
    rename(Observed =n) %>%
    mutate(Year = as.factor(Year)) %>%
    full_join(dat.pop) %>%
    full_join(data.expected_y) %>%
    mutate(Perc = (Population/Pop_total)*100,
           Expected = (Number*Perc)/100) %>%
    dplyr::select(Year, Altitude, Observed, Expected, Population, Pop_total, Number, Perc)
  

  save( data.obs.exp.altitude ,file=paste0("data/data.obs.exp.altitude.RData"))
  write.xlsx( data.obs.exp.altitude ,file=paste0("data/data.obs.exp.altitude.xlsx"),rowNames=FALSE, overwrite = TRUE)
  

  
 

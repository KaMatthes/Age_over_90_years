function_obs_exp_female_altitude <- function() {
  
  load("data/data_age90.RData")
  
  
  dat.pop <- readxl::read_excel("data_raw/Population_Altitude_sex.xlsx") %>%
    filter(Year==1900) %>%
    filter(Sex=="Female")
  
  dat.pop.t <- dat.pop %>%
    filter(Year==1900) %>%
    group_by(Year) %>%
    summarise(Pop_total = sum(Population)) %>%
    ungroup()  
  
  
  dat.pop <- dat.pop  %>%
    full_join(dat.pop.t)
  
  
  data.expected_y <- data_age90 %>%
    dplyr::select(Erhebungsjahr, Geschlecht) %>%
    rename(Year = Erhebungsjahr) %>%
    rename(Sex = Geschlecht) %>%
    mutate(Sex=recode(Sex,
                      "0" = "Female",
                      "1" = "Male" ),
           Sex = factor(Sex)) %>%
    filter(Sex=="Female") %>%
    group_by(Year) %>%
    count() %>%
    ungroup() %>%
    rename(Number =n) %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(Year==1900)
  
  

  
  data.obs.exp.altitude.female <- data_age90 %>%
    dplyr::select(Erhebungsjahr,W_Hoehe, Geschlecht) %>%
    rename(Sex = Geschlecht) %>%
    mutate( W_Hoehe_cat = cut(W_Hoehe,breaks=brk_alt_poisson,include.lowest = TRUE,right = FALSE),
            W_Hoehe_cat = recode( W_Hoehe_cat,
                                  "[0,600)" = "< 600",
                                  "[600,1e+03)" = "600-999",
                                  "[1e+03,Inf]" = ">= 1000"),
            Sex=recode(Sex,
                       "0" = "Female",
                       "1" = "Male" ),
            Sex = factor(Sex)) %>%
    rename(Year = Erhebungsjahr,
           Altitude= W_Hoehe_cat) %>%
    filter(Year==1900) %>%
    filter(Sex=="Female") %>%
    group_by(Year,Altitude) %>%
    count() %>%
    ungroup() %>%
    rename(Observed =n) %>%
    full_join(dat.pop) %>%
    full_join(data.expected_y) %>%
    mutate(Perc = (Population/Pop_total)*100,
           Expected = (Number*Perc)/100) %>%
    dplyr::select(Year, Altitude, Observed, Expected, Population, Pop_total, Number, Perc)
  

  save( data.obs.exp.altitude.female ,file=paste0("data/data.obs.exp.altitude.female.RData"))
  write.xlsx(data.obs.exp.altitude.female ,file=paste0("data/data.obs.exp.altitude.female.xlsx"),rowNames=FALSE, overwrite = TRUE)
  
}
  
 

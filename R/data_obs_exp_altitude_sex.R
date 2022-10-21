function_obs_exp_altitude <- function() {
  load("data/data_age90.RData")
  
  dat.pop.t <- readxl::read_excel("data_raw/Population_Altitude_sex_one.xlsx", sheet="Sheet1") %>%
    group_by(Year) %>%
    summarise(Pop_total = sum(Population)) %>%
    ungroup()
  
  dat.pop <- readxl::read_excel("data_raw/Population_Altitude_sex_one.xlsx", sheet="Sheet1") %>%
    full_join(dat.pop.t)
  
  data.expected_y <- data_age90 %>%
    dplyr::select(Erhebungsjahr) %>%
    rename(Year = Erhebungsjahr) %>%
    filter(Year==1900) %>%
    group_by(Year) %>%
    count() %>%
    ungroup() %>%
    rename(Number =n) 
  

  data.obs.exp.altitude.sex.one <-  data_age90 %>%
    dplyr::select(Erhebungsjahr, Geschlecht,W_Hoehe) %>%
    rename(Year = Erhebungsjahr) %>%
    rename(Sex = Geschlecht) %>%
    filter(Year==1900)    %>%
    mutate( W_Hoehe_cat = cut(W_Hoehe,breaks=brk_alt_poisson,include.lowest = TRUE,right = FALSE),
            W_Hoehe_cat = recode( W_Hoehe_cat,
                                  "[0,600)" = "< 600",
                                  "[600,1e+03)" = "600-999",
                                  "[1e+03,Inf]" = ">= 1000"),
            Sex=recode(Sex,
                       "0" = "Female",
                       "1" = "Male" ),
            Sex = factor(Sex)) %>%
    rename(Altitude= W_Hoehe_cat) %>%
    group_by(Sex, Altitude) %>%
    count() %>%
    ungroup() %>%
    rename(Observed  =n) %>%
    mutate(Altitude_Sex =c("600_f",
                           "600_999_f",
                           "1000_f",
                           "600_m",
                           "600_999_m",
                           "1000_m"),
           Year=1900) %>%
    full_join(  data.expected_y) %>%
    full_join(dat.pop) %>%
    mutate(Perc = (Population/Pop_total)*100,
           Expected = (Number*Perc)/100) %>%
    dplyr::select(Year,Altitude_Sex, Altitude, Observed, Expected, Population, Pop_total, Number, Perc)
  

  save( data.obs.exp.altitude.sex.one ,file=paste0("data/data.obs.exp.altitude.sex.one.RData"))
  write.xlsx(data.obs.exp.altitude.sex.one ,file=paste0("data/data.obs.exp.altitude.sex.one.xlsx"),rowNames=FALSE, overwrite = TRUE)
  
}
  
 

function_obs_exp_sex <- function() {
  
  load("data/data_age90.RData")
  
  
  dat.pop.1888 <- readxl::read_excel(paste0("data_raw/Age_distribution_1888_1900.xlsx"), sheet="1888") %>%
    mutate(Female = sum(Frauen),
           Male = sum(Männer),
           Year = "1888") %>%
    distinct(Year, .keep_all = TRUE) %>%
    select(Year, Female, Male)
  
  
  dat.pop.1900 <- readxl::read_excel(paste0("data_raw/Age_distribution_1888_1900.xlsx"), sheet="1900") %>%
    mutate(Female = sum(Frauen),
           Male = sum(Männer),
           Year = "1900") %>%
    distinct(Year, .keep_all = TRUE) %>%
    select(Year, Female, Male)
  
  dat.pop <- rbind(dat.pop.1888, dat.pop.1900) %>%
    gather(., Sex, Population,Female:Male)


  dat.pop.t <- dat.pop %>%
    group_by(Year) %>%
    summarise(Pop_total = sum(Population)) %>%
    ungroup()  
  
  
  dat.pop <- dat.pop  %>%
    full_join(dat.pop.t)
  
  
  data.expected_y <- data_age90 %>%
    dplyr::select(Erhebungsjahr) %>%
    rename(Year = Erhebungsjahr) %>%
    group_by(Year) %>%
    count() %>%
    ungroup() %>%
    rename(Number =n) %>%
    mutate(Year = as.factor(Year))
  

  data.obs.exp.sex <- data_age90 %>%
    dplyr::select(Erhebungsjahr,Geschlecht) %>%
    rename(Year = Erhebungsjahr) %>%
    rename(Sex  = Geschlecht) %>%
    mutate(Sex=recode(Sex,
                      "0" = "Female",
                      "1" = "Male" ),
           Sex = factor(Sex)) %>%
    group_by(Year,Sex) %>%
    count() %>%
    ungroup() %>%
    rename(Observed =n) %>%
    mutate(Year = as.factor(Year),
           Sex = as.factor(Sex)) %>%
    full_join(dat.pop) %>%
    full_join(data.expected_y) %>%
    mutate(Perc = (Population/Pop_total)*100,
           Expected = (Number*Perc)/100) %>%
    dplyr::select(Year, Sex, Observed, Expected, Population, Pop_total, Number, Perc)
  

  save( data.obs.exp.sex ,file=paste0("data/data.obs.exp.sex.RData"))
  write.xlsx( data.obs.exp.sex ,file=paste0("data/data.obs.exp.sex.xlsx"),rowNames=FALSE, overwrite = TRUE)
  
}
  
 

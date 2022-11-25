function_poisson_regression <- function(YearM, Variable_s) {
  
  load("data/data.obs.exp.sex.RData")
  data.obs.exp.sex <- data.obs.exp.sex %>%
    rename(Faktor = Sex) %>%
    mutate(Variable ="Sex")
  
  load("data/data.obs.exp.urbanity.RData")
  data.obs.exp.urbanity <- data.obs.exp.urbanity %>%
    rename(Faktor = Urbanity) %>%
    mutate(Variable ="Urbanity")
  
  load("data/data.obs.exp.language.RData")
    data.obs.exp.language <- data.obs.exp.language %>%
    rename(Faktor =Language) %>%
      mutate(Variable ="Language")
  
  load("data/data.obs.exp.altitude.RData")
    data.obs.exp.altitude <- data.obs.exp.altitude %>%
    rename(Faktor =Altitude)%>%
      mutate(Variable ="Altitude")
  

  data.model <- rbind(data.obs.exp.sex, data.obs.exp.urbanity, data.obs.exp.language,data.obs.exp.altitude) %>%
      filter(Year == YearM) %>%
    filter(Variable == Variable_s)
  
  if(Variable_s=="Sex") {
    data.model <-   data.model %>%
      mutate(Faktor=factor(Faktor, levels=c("male","female")))
  }
    
  if(Variable_s=="Urbanity") {
    data.model <-   data.model %>%
      mutate(Faktor=factor(Faktor, levels=c("urban","rural")))
  }
  
  if(Variable_s=="Language") {
    data.model <-   data.model %>%
      mutate(Faktor=factor(Faktor, levels=c("German", "French", "Italian")))
  }
  
  if(Variable_s=="Altitude") {
    data.model <-   data.model %>%
      mutate(Faktor=factor(Faktor, levels=c("< 600","600-999",">= 1000")))
  }
  
    
    mod_sum <- summary(Model_Poisson <- glm(Observed ~ Faktor , family="poisson",offset=log(Population), data=data.model ))
    
    OR <- data.frame(exp(mod_sum$coefficients[,1])) %>%
      mutate(OR=format(round(exp.mod_sum.coefficients...1..,2),nsmall=2),
             Faktor= row.names(.)) %>%
      filter(!Faktor=="(Intercept)") %>%
      dplyr::select(-1)
    
    CIl <-  data.frame(exp(mod_sum$coefficients[,1]-1.96*mod_sum$coefficients[,2])) %>%
      mutate(CIl=format(round(exp.mod_sum.coefficients...1....1.96...mod_sum.coefficients...,2),nsmall=2),
             Faktor= row.names(.)) %>%
      filter(!Faktor=="(Intercept)") %>%
      dplyr::select(-1)
    
    CIu <-  data.frame(exp(mod_sum$coefficients[,1]+1.96*mod_sum$coefficients[,2]))  %>%
      mutate(CIu= format(round(exp.mod_sum.coefficients...1....1.96...mod_sum.coefficients...,2),nsmall=2),
             Faktor= row.names(.)) %>%
      filter(!Faktor=="(Intercept)") %>%
      dplyr::select(-1)
    
    mod_p <- data.frame(mod_sum$coefficients[,4])  %>%
      mutate(p_value= format(round(mod_sum.coefficients...4.,5),nsmall=2),
             Faktor= row.names(.)) %>%
      filter(!Faktor=="(Intercept)") %>%
      dplyr::select(-1)
    
    
    res <- OR %>%
      left_join(CIl) %>%
      left_join(CIu) %>%
      left_join(mod_p) %>%
      mutate(CI = paste0(CIl," - ", CIu)) %>%
      dplyr::select(Faktor, OR, CI, p_value)
    
    res %>%
      kbl() %>%
      kable_material(c("striped", "hover"),full_width = F,position = "left")
return(res)
    
}

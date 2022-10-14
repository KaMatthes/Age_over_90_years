function_language_diff <- function(dataset, YearM, Both) {
  
  load("../data/data.obs.exp.language.RData")

  
  if(dataset =="Original" & Both=="no") {
    data.model <- data.obs.exp.language %>%
      filter(Year == YearM) %>%
      mutate(Language=factor(Language, levels=c("German", "French", "Italian")))
    
    mod_sum <- summary(Model_lang <- glm(Observed ~ Language , family="poisson",offset=log(Pop), data=data.model )) 
    OR <- data.frame(exp(mod_sum$coefficients[,1])) %>%
      mutate(OR=format(round(exp.mod_sum.coefficients...1..,2),nsmall=2),
             Language = row.names(.)) %>%
      filter(!Language=="(Intercept)") %>%
      dplyr::select(-1)
    
    CIl <-  data.frame(exp(mod_sum$coefficients[,1]-1.96*mod_sum$coefficients[,2])) %>%
      mutate(CIl=format(round(exp.mod_sum.coefficients...1....1.96...mod_sum.coefficients...,2),nsmall=2),
             Language = row.names(.)) %>%
      filter(!Language=="(Intercept)") %>%
      dplyr::select(-1)
    
    CIu <-  data.frame(exp(mod_sum$coefficients[,1]+1.96*mod_sum$coefficients[,2]))  %>%
      mutate(CIu= format(round(exp.mod_sum.coefficients...1....1.96...mod_sum.coefficients...,2),nsmall=2),
             Language = row.names(.)) %>%
      filter(!Language=="(Intercept)") %>%
      dplyr::select(-1)
    
    mod_p <- data.frame(mod_sum$coefficients[,4])  %>%
      mutate(p_value= format(round(mod_sum.coefficients...4.,5),nsmall=2),
             Language = row.names(.)) %>%
      filter(!Language=="(Intercept)") %>%
      dplyr::select(-1)
    
    
    res <- OR %>%
      left_join(CIl) %>%
      left_join(CIu) %>%
      left_join(mod_p) %>%
      mutate(CI = paste0(CIl," - ", CIu)) %>%
      dplyr::select(Language, OR, CI, p_value)
    
    res %>%
      kbl() %>%
      kable_material(c("striped", "hover"),full_width = F,position = "left")
    

    
  }
  
  
  # else if(dataset =="UniGe" & Both=="no") {
  #   data.model <- data.obs.exp.language %>%
  #     filter(Year == YearM) %>%
  #     mutate(Language=factor(Language, levels=c("German", "French", "Italian")))
  #   
  #   summary(Model_lang <- glm(Observed_UniGe ~ Language , family="poisson",offset=log(Pop_UniGe), data=data.model))
  #   
  # }
  # 
  
  else if(dataset =="Original" & Both=="yes") {
    data.model <- data.obs.exp.language %>%
      distinct(Year,.keep_all = TRUE)
    
    summary(Model_lang <- glm(Number_Alessia ~ Year  , family="poisson",offset=log(Pop_total), data=data.model))
  
    
  }
  
}

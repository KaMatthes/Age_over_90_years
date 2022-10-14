function_sex_diff <- function(YearM) {
  
  load("../data/data.obs.exp.sex.RData")

  data.model <- data.obs.exp.sex %>%
      filter(Year == YearM) %>%
      mutate(Sex=factor(Sex, levels=c("Male","Female")))
    
    mod_sum <- summary(Model_sex <- glm(Observed ~ Sex , family="poisson",offset=log(Population), data=data.model ))
    
    
    OR <- data.frame(exp(mod_sum$coefficients[,1])) %>%
      mutate(OR=format(round(exp.mod_sum.coefficients...1..,2),nsmall=2),
             Sex= row.names(.)) %>%
      filter(!Sex=="(Intercept)") %>%
      dplyr::select(-1)
    
    CIl <-  data.frame(exp(mod_sum$coefficients[,1]-1.96*mod_sum$coefficients[,2])) %>%
      mutate(CIl=format(round(exp.mod_sum.coefficients...1....1.96...mod_sum.coefficients...,2),nsmall=2),
             Sex= row.names(.)) %>%
      filter(!Sex=="(Intercept)") %>%
      dplyr::select(-1)
    
    CIu <-  data.frame(exp(mod_sum$coefficients[,1]+1.96*mod_sum$coefficients[,2]))  %>%
      mutate(CIu= format(round(exp.mod_sum.coefficients...1....1.96...mod_sum.coefficients...,2),nsmall=2),
             Sex= row.names(.)) %>%
      filter(!Sex=="(Intercept)") %>%
      dplyr::select(-1)
    
    mod_p <- data.frame(mod_sum$coefficients[,4])  %>%
      mutate(p_value= format(round(mod_sum.coefficients...4.,5),nsmall=2),
             Sex= row.names(.)) %>%
      filter(!Sex=="(Intercept)") %>%
      dplyr::select(-1)
    
    
    res <- OR %>%
      left_join(CIl) %>%
      left_join(CIu) %>%
      left_join(mod_p) %>%
      mutate(CI = paste0(CIl," - ", CIu)) %>%
      dplyr::select(Sex, OR, CI, p_value)
    
    res %>%
      kbl() %>%
      kable_material(c("striped", "hover"),full_width = F,position = "left")

    
}

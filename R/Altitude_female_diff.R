function_altitude_female_diff <- function() {
  
  load("../data/data.obs.exp.altitude.female.RData")

  data.model <- data.obs.exp.altitude.female %>%
      mutate(Altitude=factor(Altitude, levels=c("< 600","600-999",">= 1000")))
    
    mod_sum <- summary(Model_alt <- glm(Observed ~ Altitude , family="poisson",offset=log(Population), data=data.model ))
    
    
    OR <- data.frame(exp(mod_sum$coefficients[,1])) %>%
      mutate(OR=format(round(exp.mod_sum.coefficients...1..,2),nsmall=2),
             Altitude = row.names(.)) %>%
      filter(!Altitude=="(Intercept)") %>%
      dplyr::select(-1)
    
    CIl <-  data.frame(exp(mod_sum$coefficients[,1]-1.96*mod_sum$coefficients[,2])) %>%
      mutate(CIl=format(round(exp.mod_sum.coefficients...1....1.96...mod_sum.coefficients...,2),nsmall=2),
             Altitude = row.names(.)) %>%
      filter(!Altitude=="(Intercept)") %>%
      dplyr::select(-1)
    
    CIu <-  data.frame(exp(mod_sum$coefficients[,1]+1.96*mod_sum$coefficients[,2]))  %>%
      mutate(CIu= format(round(exp.mod_sum.coefficients...1....1.96...mod_sum.coefficients...,2),nsmall=2),
             Altitude = row.names(.)) %>%
      filter(!Altitude=="(Intercept)") %>%
      dplyr::select(-1)
    
    mod_p <- data.frame(mod_sum$coefficients[,4])  %>%
      mutate(p_value= format(round(mod_sum.coefficients...4.,5),nsmall=2),
             Altitude = row.names(.)) %>%
      filter(!Altitude=="(Intercept)") %>%
      dplyr::select(-1)
    
    
    res <- OR %>%
      left_join(CIl) %>%
      left_join(CIu) %>%
      left_join(mod_p) %>%
      mutate(CI = paste0(CIl," - ", CIu)) %>%
      dplyr::select(Altitude, OR, CI, p_value)
    
    res %>%
      kbl() %>%
      kable_material(c("striped", "hover"),full_width = F,position = "left")

    
}

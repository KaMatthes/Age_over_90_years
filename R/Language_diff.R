function_language_diff <- function(dataset, YearM, Both) {
  
  load("../data/data.obs.exp.language.RData")

  
  if(dataset =="Original" & Both=="no") {
    data.model <- data.obs.exp.language %>%
      filter(Year == YearM) %>%
      mutate(Language=factor(Language, levels=c("German", "French", "Italian")))
    
    summary(Model_lang <- glm(Observed ~ Language , family="poisson",offset=log(Pop), data=data.model ))
  
    
  }
  
  
  else if(dataset =="UniGe" & Both=="no") {
    data.model <- data.obs.exp.language %>%
      filter(Year == YearM) %>%
      mutate(Language=factor(Language, levels=c("German", "French", "Italian")))
    
    summary(Model_lang <- glm(Observed_UniGe ~ Language , family="poisson",offset=log(Pop_UniGe), data=data.model))
    
  }
  
  
  else if(dataset =="Original" & Both=="yes") {
    data.model <- data.obs.exp.language %>%
      mutate(Language=factor(Language, levels=c("German", "French", "Italian")))
    
    m1 <- summary(Model_lang <- glm(Observed ~ Year  , family="poisson",offset=log(Pop), data=data.model))
    m2 <- summary(Model_lang <- glm(Observed ~ Year + Language  , family="poisson",offset=log(Pop), data=data.model))
    
    m_list <- list(m1, m2)
    return(m_list)
    
  }
  
}

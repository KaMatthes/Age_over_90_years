rm(list=ls())
source("R/00_setup.R")

function_regression_nb <- function(YearM) {
  
  
  data.cofactors <- read_excel("data/Cofactors.xlsx") %>%
    mutate(U90_inc=  number/population*10000,  
           Mortality=death/population*10000,
           TB_1888  = as.numeric(TB_1888 ),
           TB_mort = TB_1888/population*10000,
           hosp_group = ifelse( hospitals>0, ">=1",  hospitals),
           hosp_group = factor( hosp_group, levels = c("0", ">=1")),
           Prop80 = as.numeric( Prop80),
           height = as.numeric(height))

  if (YearM == 1888) {
    
    data.cofactors <-  data.cofactors %>%
      filter(Year == 1888)
    
  Mod1 <- coef(summary(glm.nb(number ~ Mortality  + offset(log(population)),data = data.cofactors)))
  Mod2 <- coef(summary(glm.nb(number ~ BIP + offset(log(population)),data = data.cofactors)))
  Mod3 <- coef(summary(glm.nb(number ~ height + offset(log(population)),data = data.cofactors)))
  Mod4 <- coef(summary(glm.nb(number ~ note1 + offset(log(population)),data = data.cofactors)))
  Mod5 <- coef(summary(glm.nb(number ~ TB_mort + offset(log(population)),data = data.cofactors)))
  Mod6 <- coef(summary(glm.nb(number ~ hosp_group + offset(log(population)),data = data.cofactors)))
  Mod7 <- coef(summary(glm.nb(number ~ Language + offset(log(population)),data = data.cofactors)))
  Mod8 <- coef(summary(glm.nb(number ~ height + Language +offset(log(population)),data = data.cofactors)))
  
  
  results_regression <- rbind(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6,Mod7, Mod8) %>%
  data.frame(.) %>%
  mutate(Cofactor=row.names(.)) %>%
    filter(  Cofactor=="Mortality" |  Cofactor=="BIP" |   Cofactor=="height"
            | Cofactor=="note1"  | Cofactor=="TB_mort" | Cofactor=="hosp_group..1"
            | Cofactor=="LanguageFranzösisch"  | Cofactor=="LanguageItalienisch" |  Cofactor=="height.1"
            | Cofactor=="LanguageFranzösisch.1"  | Cofactor=="LanguageItalienisch.1") %>%
    mutate(
         est= round(exp(Estimate),3),
         Cl = round(exp(Estimate - 1.96* Std..Error),3),
         Cu = round(exp(Estimate + 1.96* Std..Error),3),
         univariate = paste0(est," (",Cl,"-",Cu, ")")) %>%
    select(Cofactor, univariate) %>%
    mutate(Cofactor= recode(Cofactor,
                            "BIP" = "GDP",
                            "note1" = "Education"))


  }
  
  else if (YearM==1900) {

    data.cofactors <-  data.cofactors %>%
      filter(Year == 1900)
    
    Mod1 <- coef(summary(glm.nb(number ~ Mortality  + offset(log(population)),data = data.cofactors)))
    Mod2 <- coef(summary(glm.nb(number ~ BIP + offset(log(population)),data = data.cofactors)))
    Mod3 <- coef(summary(glm.nb(number ~ note1 + offset(log(population)),data = data.cofactors)))
    Mod4 <- coef(summary(glm.nb(number ~ Prop80 + offset(log(population)),data = data.cofactors)))
    Mod5 <- coef(summary(glm.nb(number ~ hosp_group + offset(log(population)),data = data.cofactors)))
    Mod6 <- coef(summary(glm.nb(number ~ Language + offset(log(population)),data = data.cofactors)))
    
    
    results_regression <- rbind(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6) %>%
      data.frame(.) %>%
      mutate(Cofactor=row.names(.)) %>%
      filter(  Cofactor=="Mortality" |  Cofactor=="BIP" 
               | Cofactor=="note1"  | Cofactor=="Prop80" | Cofactor=="hosp_group..1"
               | Cofactor=="LanguageFranzösisch"  | Cofactor=="LanguageItalienisch" ) %>%
      mutate(
        est= round(exp(Estimate),3),
        Cl = round(exp(Estimate - 1.96* Std..Error),3),
        Cu = round(exp(Estimate + 1.96* Std..Error),3),
        univariate = paste0(est," (",Cl,"-",Cu, ")")) %>%
      select(Cofactor, univariate) %>%
      mutate(Cofactor= recode(Cofactor,
                              "BIP" = "GDP",
                              "note1" = "Education"))
    
    
    
  }
  
  results_regression

}

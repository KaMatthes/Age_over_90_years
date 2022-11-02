function_regression <- function(YearM) {
  
  
  if (YearM==1888) {
  data.cofactors <- read_excel("../data_raw/Cofactors_1888.xlsx") %>%
    mutate(U90_inc=  U90/population*10000,  
           Mortality=death/population*10000,
           TB_1888  = as.numeric(TB_1888 ),
           TB_inc= TB_1888/population*10000,
           hosp_group = ifelse( hospitals>0, ">=1",  hospitals),
           hosp_group = factor( hosp_group, levels = c("0", ">=1")))
  
  
    # formula<-as.formula( paste0("U90 ~",eval(substitute( Cofactor)), "+offset(log(population))"))
    # 
    # # results <- summary(robmixglm(formula,data = data.cofactors, family="nbinom"))
    # # results <- summary(glm(formula,data = data.cofactors, family=poisson(link = "log")))
    # # # 
    # results <- summary(glm.nb(formula,data = data.cofactors))
  
  Mod1 <- coef(summary(glm.nb(U90 ~ Mortality  + offset(log(population)),data = data.cofactors)))
  Mod2 <- coef(summary(glm.nb(U90 ~ BIP + offset(log(population)),data = data.cofactors)))
  Mod3 <- coef(summary(glm.nb(U90 ~ height + offset(log(population)),data = data.cofactors)))
  Mod4 <- coef(summary(glm.nb(U90 ~ note1 + offset(log(population)),data = data.cofactors)))
  Mod5 <- coef(summary(glm.nb(U90 ~ TB_inc + offset(log(population)),data = data.cofactors)))
  Mod6 <- coef(summary(glm.nb(U90 ~ hosp_group + offset(log(population)),data = data.cofactors)))
  Mod7 <- coef(summary(glm.nb(U90 ~ Language + offset(log(population)),data = data.cofactors)))
  Mod8 <- coef(summary(glm.nb(U90 ~ height + Language +offset(log(population)),data = data.cofactors)))
  
  
  results_regression <- rbind(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6,Mod7, Mod8) %>%
  data.frame(.) %>%
  mutate(Cofactor=row.names(.)) %>%
    filter(  Cofactor=="Mortality" |  Cofactor=="BIP" |   Cofactor=="height"
            | Cofactor=="note1"  | Cofactor=="TB_inc" | Cofactor=="hosp_group..1"
            | Cofactor=="LanguageFranzösisch"  | Cofactor=="LanguageItalienisch" |  Cofactor=="height.1"
            | Cofactor=="LanguageFranzösisch.1"  | Cofactor=="LanguageItalienisch.1") %>%
    mutate(
         est= round(exp(Estimate),3),
         Cl = round(exp(Estimate - 1.96* Std..Error),3),
         Cu = round(exp(Estimate + 1.96* Std..Error),3),
         univariate = paste0(est," (",Cl,"-",Cu, ")")) %>%
    select(Cofactor, univariate)


  }
  
  else if (YearM==1900) {
    
    data.cofactors <-  read_excel("../data_raw/Cofactors_1900.xlsx") %>%
      mutate(U90_inc=  U90/population*10000,  
             Mortality=death/population*10000,
             hosp_group = ifelse( hospitals>0, ">=1",  hospitals),
             hosp_group = factor( hosp_group, levels = c("0", ">=1")))
    
    
    Mod1 <- coef(summary(glm.nb(U90 ~ Mortality  + offset(log(population)),data = data.cofactors)))
    Mod2 <- coef(summary(glm.nb(U90 ~ BIP + offset(log(population)),data = data.cofactors)))
    Mod3 <- coef(summary(glm.nb(U90 ~ note1 + offset(log(population)),data = data.cofactors)))
    Mod4 <- coef(summary(glm.nb(U90 ~ Prop80 + offset(log(population)),data = data.cofactors)))
    Mod5 <- coef(summary(glm.nb(U90 ~ hosp_group + offset(log(population)),data = data.cofactors)))
    Mod6 <- coef(summary(glm.nb(U90 ~ Language + offset(log(population)),data = data.cofactors)))
    
    
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
      select(Cofactor, univariate)
    
    
    
  }
  
  results_regression
# 
 # write.xlsx(results_regression,file=paste0("output/results_regression_",YearM,".xlsx"),row.names=FALSE, overwrite = TRUE)

}

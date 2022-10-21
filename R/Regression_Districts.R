function_regression <- function(YearM) {
  
  
  data.1888 <- read_excel("data_raw/Cofactors_1888.xlsx")
  data.1900 <- read_excel("data_raw/Cofactors_1900.xlsx")
  
  data.cofactors <- rbind(data.1888, data.1900) %>%
    filter(Year == YearM) %>%
    mutate(U90_inc=  U90/population*10000,  
           Mortality=death/population*10000,
           Birth_inc=birth/population*10000)
  
  
    # formula<-as.formula( paste0("U90 ~",eval(substitute( Cofactor)), "+offset(log(population))"))
    # 
    # # results <- summary(robmixglm(formula,data = data.cofactors, family="nbinom"))
    # # results <- summary(glm(formula,data = data.cofactors, family=poisson(link = "log")))
    # # # 
    # results <- summary(glm.nb(formula,data = data.cofactors))
  
  Mod1 <- coef(summary(glm.nb(U90 ~ offset(log(population)),data = data.cofactors)))
  Mod2 <- coef(summary(glm.nb(U90 ~ Mortality  + offset(log(population)),data = data.cofactors)))
  Mod3 <- coef(summary(glm.nb(U90 ~ Birth_inc + offset(log(population)),data = data.cofactors)))
  Mod4 <- coef(summary(glm.nb(U90 ~ BIP + offset(log(population)),data = data.cofactors)))
  Mod5 <- coef(summary(glm.nb(U90 ~ height + offset(log(population)),data = data.cofactors)))
  Mod6 <- coef(summary(glm.nb(U90 ~ note1 + offset(log(population)),data = data.cofactors)))
  Mod7 <- coef(summary(glm.nb(U90 ~ note5 + offset(log(population)),data = data.cofactors)))
  
  
  results_uni <- rbind(Mod1, Mod2, Mod3, Mod4, Mod5, Mod6,Mod7) %>%
  data.frame(.) %>%
  mutate(Cofactor=row.names(.))%>%
    filter(  Cofactor=="Mortality" | Cofactor=="Birth_inc" |   Cofactor=="BIP" |   Cofactor=="height"
            | Cofactor=="note1"  | Cofactor=="note5") %>%
    mutate(
         est= round(exp(Estimate),3),
         Cl = round(exp(Estimate - 1.96* Std..Error),3),
         Cu = round(exp(Estimate + 1.96* Std..Error),3),
         univariate = paste0(est," (",Cl,"-",Cu, ")")) %>%
    select(Cofactor, univariate)
  
  
  results_multi <- coef(summary(glm.nb(U90 ~ Mortality + Birth_inc + BIP+ height + note1 + 
                                     note5 +offset(log(population)),data = data.cofactors))) %>%
    data.frame(.) %>%
    mutate(Cofactor=row.names(.))%>%
    filter( Cofactor=="Mortality" | Cofactor=="Birth_inc" |   Cofactor=="BIP" |   Cofactor=="height"
             | Cofactor=="note1"  | Cofactor=="note5") %>%
    mutate(
      est= round(exp(Estimate),3),
      Cl = round(exp(Estimate - 1.96* Std..Error),3),
      Cu = round(exp(Estimate + 1.96* Std..Error),3),
      multivariate = paste0(est," (",Cl,"-",Cu, ")")) %>%
    select(Cofactor, multivariate)
  
results_regression  <-   results_uni %>%
  full_join( results_multi )


results_regression 
# 
 write.xlsx(results_regression,file=paste0("output/results_regression_",YearM,".xlsx"),row.names=FALSE, overwrite = TRUE)

}

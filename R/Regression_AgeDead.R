function_age_dead <- function(Var) {
  load("../data/data_age.RData")
  
 
  if (Var=="Year") {
  summary(mod.age <- lm(Age ~ Year, data=data_age))
  }
  else if (Var=="Language") {
    summary(mod.age <- lm(Age ~  Language + Year, data=data_age))
  }
  else if (Var=="Altitude") {
  summary(mod.age <- lm(Age ~ W_Hoehe+ Year, data=data_age))
  }
  else if (Var=="Altitude_cat") {
    summary(mod.age <- lm(Age ~ W_Hoehe_cat + Year, data=data_age))
  }
  
  else if (Var=="Altitude_Language") {
    summary(mod.age <- lm(Age ~ W_Hoehe*Language + Year, data=data_age))
  }
 
}
  
 

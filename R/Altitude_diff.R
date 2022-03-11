function_altitude_diff <- function(YearM) {
  
  load("../data/data.obs.exp.altitude.RData")

  data.model <- data.obs.exp.altitude %>%
      filter(Year == YearM) %>%
      mutate(Altitude=factor(Altitude, levels=c("400-599","< 400","600-999","> 1000")))
    
    summary(Model_alt <- glm(Observed ~ Altitude , family="poisson",offset=log(Population), data=data.model ))

    
}

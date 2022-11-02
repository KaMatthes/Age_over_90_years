data_age90 <- readxl::read_excel("data_raw/Data_1888_1900.xlsx", sheet="Liste 1888 & 1900") %>%
  dplyr::select(-ID, -Ordnungszahl, - Ordnungszahl2)

save(data_age90 ,file=paste0("data/data_age90.RData"))
write.xlsx(data_age90,file=paste0("data/data_age90.xlsx"),rowNames=FALSE, overwrite = TRUE)
function_age_dead_data <- function() {
  load("data/data_age90.RData")
  
  data_age <- data_age90 %>%
    dplyr::select(Erhebungsjahr,Alter, W_Hoehe, Eigener_Beruf, Beruf_sdt,Wohnkanton) %>%
    filter(!is.na(Alter))  %>%
    rename(Age = Alter,
           Year = Erhebungsjahr) %>%
    mutate(Language = Wohnkanton,
           Language = as.character(Language),
           Language = recode(Language,
                             "Aargau" = "German",
                             "AppenzellA.-Rh." = "German",
                             "AppenzellI.-Rh." = "German",
                             "Basel-Stadt" = "German",
                             "Basel" = "German",
                             "Basel-Landschaft" = "German",
                             "Bern" = "German",
                             "Freiburg" = "French",
                             "Genf" = "French",
                             "Glarus" = "German",
                             "Graubünden" = "German",
                             "Luzern" = "German",
                             "Neuenburg" = "French",
                             "Nidwalden" = "German",
                             "Obwalden" = "German",
                             "Schaffhausen" ="German",
                             "Schwyz" = "German",
                             "Solothurn" = "German",
                             "St.Gallen" ="German",
                             "Tessin" = "Italian",
                             "Thurgau" = "German",
                             "Uri" = "German",
                             "Waadt" = "French",
                             "Wallis" ="French",
                             "Zug" = "German",
                             "Zürich" = "German"),
           W_Hoehe_cat = cut(W_Hoehe,breaks=brk_alt_reg, include.lowest = TRUE))
  
  save(data_age,file=paste0("data/data_age.RData"))
  write.xlsx(data_age,file=paste0("data/data_age.xlsx"),rowNames=FALSE, overwrite = TRUE)
 
}
  
 

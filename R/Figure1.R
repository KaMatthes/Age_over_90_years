load("data/data_age90.RData")
  #load("../data/data_age90.RData")

  
  
  data_age90_map <- data_age90 %>%
    dplyr::mutate(Alter_cat = cut(Alter,breaks=brk_age ,include.lowest = TRUE),
                  Alter_cat = ifelse(is.na(Alter_cat), "no Info", Alter_cat),
                  Alter_cat = as.factor(Alter_cat),
                  W_Hoehe_cat = cut(W_Hoehe,breaks=brk_alt_poisson ,include.lowest = TRUE))
  # load map switzerland
  
   map_swiss <- read_sf("data/Maps_Switzerland/g2l15.shp")
   #map_swiss <- read_sf("../data/maps/Timo/g2l15.shp")
   
   map_canton <- read_sf("data/Maps_Switzerland/g2k15.shp") %>%
     mutate(Language = KTNR,
            Language = as.character(Language),
            Language = recode(Language,
                              "19" = "German",
                              "15" = "German",
                              "16" = "German",
                              "13" = "German",
                              "12" = "German",
                              "2" = "German",
                              "10" = "French",
                              "25" = "French",
                              "8" = "German",
                              "18" = "German",
                              "26" = "French",
                              "3" = "German",
                              "24" = "French",
                              "7" = "German",
                              "6" = "German",
                              "14" = "German",
                              "5" = "German",
                              "11" = "German",
                              "17" = "German",
                              "20" = "German",
                              "21" = "Italian",
                              "4" = "German",
                              "23" = "French",
                              "22" = "French",
                              "9" = "German",
                              "1" = "German"),
            Language = as.factor(Language))
   
   # map_lake <- read_sf("data/maps/Timo/g2s15.shp")
   map_lake <- read_sf("data/Maps_Switzerland/g2s15.shp")
   

  relief <- raster("data/Maps_Switzerland/02-relief-ascii.asc") %>%
    # hide relief outside of Switzerland by masking with country borders
    mask(map_swiss) %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame() %>%
    rename(value = X02.relief.ascii)
  
    datMaps <-   data_age90_map %>%
      filter(!W_E==0)
    
   Figure1 <- ggplot()+
      geom_sf(data=  map_canton, aes(fill= Language),alpha=1,col="grey40", size=0.1)+
      facet_wrap(~Erhebungsjahr) +
      geom_raster(data = relief,inherit.aes = FALSE,aes(x = x,y = y,alpha = value), fill="grey10")+
      scale_alpha(name = "",
                  range = c(0.6, 0),
                  guide = "none") +
      geom_sf(data=  map_lake, fill = "#D6F1FF",color = "transparent") +
      geom_point(data=datMaps,aes(x=W_E, y= W_N, col=W_Hoehe_cat, size=Alter_cat))+
      # ggtitle(paste(Year))+
      scale_size_manual("Age:",
                        values = c(2,4,6,8,0.5),
                          breaks=c("1","2","3","4", "no Info"),
                          labels=c("90-92","93-94","95-96","97-102", "no info"))+
      scale_colour_manual("Altitude:",
                          values = col3magma,
                          breaks=c("[0,600]","(600,1e+03]","(1e+03,Inf]"),
                          labels=c("0-599","600-999",">=1000"))+
      scale_fill_manual("Language:",
                        values=col3grey)+
      guides(color = guide_legend(override.aes = list(size = 7)))+
      theme_bw()+
      theme(
        panel.background=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text = element_text(size=size_striptext),
        strip.background = element_blank(),
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.border = element_blank(),
        legend.text=element_text(size=size_legend),
        legend.title =element_text(size=size_legend),
        legend.direction = "horizontal", 
        legend.box = "vertical",
        plot.title = element_text(size =size_ggtitle),
        legend.position = "bottom")
    
    cowplot::save_plot("output/Figure1.pdf", Figure1,base_height=10,base_width=20)
    
    

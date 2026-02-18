rm(list=ls())
source("R/00_setup.R")

bezirk_geo <- read_sf("data/Map_data_1918/districts.shp") %>%
    mutate(
      id= as.character(id)
    ) 

data_map <- read.xlsx("data/Hotspot.xlsx") %>%
    mutate(
      Inc=number/population*100000
    ) %>%
    rename(id=ID) %>%
    mutate(
      id=as.character(id)
    ) %>%
    arrange(id) %>%    
    mutate(Inc=ifelse(is.na(Inc),0, Inc))

dt1888 <-   data_map %>%
    filter(year==1888) 

dt1888 <-  bezirk_geo %>%
    left_join( dt1888) %>%    
    mutate(Inc=ifelse(is.na(Inc),0, Inc),
           year = 1888)

dt1900<-   data_map %>%
    filter(year==1900) 

dt1900 <-  bezirk_geo %>%
    left_join( dt1900) %>%    
    mutate(Inc=ifelse(is.na(Inc),0, Inc),
           year = 1900)
  
sf::sf_use_s2(FALSE)
  

neighbours1888 <- poly2nb(dt1888$geometry)
listw1888 <- nb2listw(neighbours1888)
gi.fixed1888 <- localG(dt1888$Inc, listw1888)

neighbours1900 <- poly2nb(dt1900$geometry)
listw1900 <- nb2listw(neighbours1900)
gi.fixed1900 <- localG(dt1900$Inc, listw1900)

bezirk_geo.gi1888 <- cbind(dt1888, as.matrix(gi.fixed1888)) %>%
  rename(gstat = as.matrix.gi.fixed1888.)
  
bezirk_geo.gi1900 <- cbind(dt1900, as.matrix(gi.fixed1900)) %>%
  rename(gstat = as.matrix.gi.fixed1900.) 

bezirk_geo.gi <- bezirk_geo.gi1888  %>%
  rbind(bezirk_geo.gi1900) %>%
  mutate(
    zvalue = case_when(
      gstat <= -2                   ~ "≤ -2",
      gstat > -2 & gstat <= -1    ~ "> -2 to ≤ -1",
      gstat > -1  & gstat <= 1     ~ "> -1 to < 1",
      gstat >= 1   & gstat < 2     ~ "≥ 1 to <2",
      gstat >= 2                    ~ "≥ 2"),
    zvalue = factor(zvalue, levels = c("≤ -2", "> -2 to ≤ -1", "> -1 to < 1", "≥ 1 to <2", "≥ 2"))
  )

ggplot(bezirk_geo.gi) +
  geom_sf(aes(fill = zvalue), color = "black", size = 0.2) +
  facet_wrap(~year) +
  scale_fill_brewer(
    palette = "RdBu",
    direction = -1,  
    name = "z-values"         
  ) +
  theme_bw()+
  theme(
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    strip.text = element_text(size=size_striptext-5),
    strip.background = element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.text=element_text(size=size_legend-5),
    legend.title =element_text(size=size_legend-5),
    legend.position = "bottom", 
    legend.key.width = unit(2, "cm"),    
    plot.title = element_text(size =size_ggtitle-5))

ggsave("output/Figure3.png",height=8, width=20,  dpi = 1000)
ggsave("output/Figure3.pdf",height=8, width=20)

# ggsave("output/Figure3.eps",height=8, width=20)
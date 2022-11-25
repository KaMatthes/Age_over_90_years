function_hotspot <- function(Year_map) {

  data_map <- readxl::read_excel("data/Hotspot.xlsx") %>%
    mutate(Inc=number/population*100000) %>%
    filter(year==Year_map) %>%
    rename(id=ID) %>%
    mutate(id=as.character(id)) %>%
    arrange(id)

  
  bezirk_geo <- read_sf("data/Map_data_1918/districts.shp") %>%
    mutate(id= as.character(id)) %>%
    left_join(data_map) %>%
    select(geometry,Inc,year) %>%
    mutate(Inc=ifelse(is.na(Inc),0, Inc))
   
  sf::sf_use_s2(FALSE)
  
neighbours <- poly2nb(bezirk_geo$geometry)
listw <- nb2listw(neighbours)
gi.fixed <- localG(bezirk_geo$Inc, listw)

bezirk_geo.gi <- cbind(bezirk_geo, as.matrix(gi.fixed)) %>%
  rename(gstat=as.matrix.gi.fixed.)

plot_hotspot <- tm_shape(bezirk_geo.gi) +
  tm_fill(col = "gstat", 
          style = "pretty",
          palette="-RdBu",
          title = "",
          midpoint = 0,
          legend.is.portrait = FALSE) +
  # tm_facets(by="year", ncol=1)+
  tm_borders(alpha = 0.5) +
  tm_layout(
    # asp=1,
    frame = FALSE,
    main.title = paste0(Year_map),
    main.title.position = "left",
    legend.text.size = legend_size_map,
    legend.width = 3,
    legend.height = 1,
    legend.position = c(0,0),
    legend.show=FALSE,
    legend.title.size=legend_size_title,
    main.title.size = main_size_map)

return(plot_hotspot)

}

function_hotspot_legend <- function(Year_map) {
data_map <- readxl::read_excel("data/Hotspot.xlsx") %>%
  mutate(Inc=number/population*100000) %>%
  filter(year==Year_map) %>%
  rename(id=ID) %>%
  mutate(id=as.character(id)) %>%
  arrange(id)


bezirk_geo <- read_sf("data/Map_data_1918/districts.shp") %>%
  mutate(id= as.character(id)) %>%
  left_join(data_map) %>%
  select(geometry,Inc,year) %>%
  mutate(Inc=ifelse(is.na(Inc),0, Inc))

sf::sf_use_s2(FALSE)

neighbours <- poly2nb(bezirk_geo$geometry)
listw <- nb2listw(neighbours)
gi.fixed <- localG(bezirk_geo$Inc, listw)


bezirk_geo.gi <- cbind(bezirk_geo, as.matrix(gi.fixed)) %>%
  rename(gstat=as.matrix.gi.fixed.)

plot_legend <- tm_shape(bezirk_geo.gi) +
  tm_fill(col = "gstat", 
          style = "pretty",
          palette="-RdBu",
          title = "",
          midpoint = 0,
          legend.is.portrait = FALSE) +
  # tm_facets(by="year", ncol=1)+
  tm_borders(alpha = 0.5) +
  tm_layout(
    # asp=1,
    frame = FALSE,
    legend.only = TRUE,
    legend.text.size = legend_size_map,
    legend.width = 3,
    legend.height = 1,
    legend.position = c(0.15,0.8),
    # legend.show=FALSE,
    legend.title.size=legend_size_title,
    main.title.size = main_size_map)

return(plot_legend)

}


plot1 <- tmap_grob(function_hotspot(1888))
plot2 <- tmap_grob(function_hotspot(1900))
plot_l <- tmap_grob(function_hotspot_legend(1888))


hotspot_maps <- plot_grid(plot1,plot2, ncol=2)
                         
Figure3 <- plot_grid(hotspot_maps,plot_l, nrow=2)
cowplot::save_plot("output/Figure3.pdf", Figure3,base_height=15,base_width=22)

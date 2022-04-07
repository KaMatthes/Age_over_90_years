function_spacial_age <- function() {

load("../data/data_age90.RData")


map_canton <- read_sf("../data/maps/Timo/g2k15.shp") 

map_polygon <- as(map_canton, 'Spatial')


datsp <- data_age90 %>%
  dplyr::select(x=W_E, y=W_N, Alter) %>%
  filter(!is.na(Alter))

coords  <- datsp[ , c("x", "y")]
data   <- datsp[ , 3]               
crs    <- CRS("+init=epsg:28992")

# make the SpatialPointsDataFrame object
spdf <- SpatialPointsDataFrame(coords      = coords,
                               data        = data, 
                               proj4string = crs)


mesh1 <-  inla.mesh.2d(loc = cbind(spdf$x, spdf$y),
                     max.edge = c(15000,40000),
                     cutoff = 5000)


ggplot() + 
  geom_sf(data =map_canton) + 
  gg(mesh1)

# spde <- inla.spde2.pcmatern(mesh1,
#                               prior.sigma = c(0.1, 0.1), 
#                               prior.range = c(10000, 0.5))


spde <- inla.spde2.pcmatern(mesh1,
                            prior.sigma = c(0.1, 0.1), 
                            prior.range = c(10000, 0.5))


proj4string(map_polygon) <- CRS(as.character(NA))
proj4string(spdf) <- CRS(as.character(NA))


cmp  = ~ Intercept(1) + 
  SPDE(coordinates, model = spde) 

# likelihood
lik = like(formula = log(Alter) ~ Intercept + SPDE  ,
           family = "gaussian",
           data = spdf)

# fit the model
fit = bru(cmp, lik,
          options = list(verbose = F,
                         bru_max_iter = 1,
                         inla.mode  = "experimental"))



pxl <- pixels(mesh1, nx=200, ny=200,mask = map_polygon)

pred_space <- predict(fit,pxl,n.samples=10000,
                 ~ data.frame(SPDE=SPDE,
                              logscale=Intercept + SPDE,
                              naturalscale=exp(Intercept+SPDE)))


ggplot() + 
  gg(pred_space$logscale, aes(fill = pred_space$logscale$mean)) +
  geom_sf(data = map_canton, alpha = 0) +
  geom_point(data=datsp,aes(x=x, y= y), lwd=1)+
  scale_fill_viridis("Mean Age") + 
  theme_bw()+
  theme(
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    panel.grid.major=element_blank(),
    axis.title=element_blank(),
    axis.line=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    panel.border = element_blank(),
    legend.text=element_text(size=size_legend),
    legend.title =element_text(size=size_legend),
    legend.direction = "horizontal", 
    legend.box = "vertical",
    legend.key.size = unit(2, "cm"),
    plot.title = element_text(size =size_ggtitle),
    legend.position = "bottom")
# Plot the result


}

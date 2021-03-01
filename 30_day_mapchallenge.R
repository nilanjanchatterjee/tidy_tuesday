########################################################################
############Maps for 30day map challenge
############## Day 3 plotting a polygon

library(raster)
library(rgdal)
library(leaflet)
library(htmlwidgets)

cil <-readOGR("D:/Global rasters/Central Indianb Landscape.kml")
gCentroid(cil)

greenLeafIcon <- makeIcon(
  iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94,
  shadowUrl = "http://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

### The link for different basemaps can be generated from the site 
###http://leaflet-extras.github.io/leaflet-providers/preview/index.html

treeIcons <- iconList(
  tree = makeIcon("C:/Users/HP/Documents/tree_icon.png","tree_icon.png", 64, 64)
)

m1 <- leaflet(cil) %>%
  addTiles(urlTemplate = "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png") %>%  ##Add  map tiles
   addPolygons(color = "blue") %>% ## Add shapefile
  addMarkers(lng=79.56, lat=22.5076, label ="Central Indian Landscape", icon = treeIcons,
             labelOptions = labelOptions(noHide = T, textsize = "18px"))

m1  # Print the map

saveWidget(m1, file="cilLeaflet5.html") ## save the map in html format


##########################################################################################
############ Day 4
############################################################

library(mapdata)
library(sf)
library(raster)
library(rgeos)

ind <-read_sf("D:/Work/Ungulate Point process/India_Boundary/India_Boundary.shp")
ind1 <-shapefile("D:/Work/Ungulate Point process/India_Boundary/India_Boundary.shp")
plot(ind1)
plot(st_make_grid(ind, cellsize = 1, square = FALSE), add=T)


meuse.large = gBuffer(ind1, byid= T, width = 1)
HexPts <-spsample(meuse.large, type="hexagonal", cellsize=1.5)
HexPols <- HexPoints2SpatialPolygons(HexPts)
plot(HexPols[ind1,], add=TRUE)
     
fc <-raster("D:/Global rasters/Forest cover/Forestcover.tif")
plot(fc)
hex_fc <- extract(fc, HexPols, fun = mean, na.rm = TRUE, sp = F)
fc1 <-mask(fc, HexPols)

hex <-st_as_sf(HexPols)
hex$value <-hex_fc

###Plotting the map
plot(hex, bg= "black", main= "")
plot(ind1, add=T, fill=NA, border= "white")

ggplot(hex) + 
  geom_sf(aes(fill = value)) +
  geom_polygon(data=ind1, aes(x=long, y=lat , group=group), colour= "black", fill=NA)+
  coord_sf(xlim = c(65, 100),
           ylim = c(7, 40)) +
  scale_fill_gradient(low = "red", high = "green")+
  theme_bw()+
  labs(x= "Longitude", y= "Latitude", fill= "Forest Cover")+
  theme(axis.title = element_text(size = 16))

ggsave("Indian_forest_hxgn.jpeg", width = 12, height = 8, units = "in", dpi=300)

#############################################################################
########## Day 4

library(raster)
library(spatstat)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(tmap)

ind <-shapefile("D:/Work/Ungulate Point process/India_Boundary/India_Boundary.shp")
riv <-readShapeLines('D:/Global rasters/IND_wat/Ind_riv.shp')
coastlines <- readOGR("D:/Work/River bird/ne-coastlines-10m/ne_10m_coastline.shp")
ocean_shpfl <- shapefile("D:/Work/River bird/ne-ocean-10m/ne_10m_ocean.shp")
ocean <-crop(ocean_shpfl, extent(60,110, 0,40))
pwrline_psp <-as.psp(crop(riv, ind))
px1 <- pixellate(pwrline_psp, eps=0.5)
riv_den <-raster(px1, crs(ind))
riv_den1 <-crop(riv_den, coastlines)
plot(riv_den)


riv_den_plt <-tm_shape(ind) + tm_borders("black")+
  tm_graticules(x=c( 70,  80, 90, 100), y = c(0, 10, 20, 30,40), projection = 4326)+
  tm_shape(riv_den1) + 
  tm_raster(palette = brewer.pal(7, "Blues") , title="Density of rivers", saturation = 1,interpolate = F) + 
  tm_shape(ind)+tm_borders("black")+
  tm_shape(coastlines)+ tm_lines("black")+
  tm_legend(legend.outside=F, position=c("right", "top") )+
  tm_layout(outer.margins = c(.01,.01,.01,.01), legend.bg.color = "grey", legend.bg.alpha = 0.8, bg.color = "grey")  +
  tm_compass(position=c("left", "top")) +    tm_scale_bar() 

tmap_save(riv_den_plt, "River_density.jpeg", height = 9, width = 6, units = "in", dpi = 300)

riv2 <-crop(riv, ind)

ind_riv <-tm_shape(ind) + tm_borders("black")+ tm_fill("grey30")+
  tm_graticules(x=c( 70,  80, 90, 100), y = c(0, 10, 20, 30,40), projection = 4326)+
  tm_shape(riv2) + 
  tm_lines(col = "R_level", palette = "Blues", legend.col.show = F, scale = 1.5) +
  tm_add_legend(title = "Orders of River", type = "line", col = rev(brewer.pal(4, "Blues")),lwd = 3,
                labels = c("First Order", "Second Order","Third Order", "Fourth Order"))+
  tm_layout(outer.margins = c(.01,.01,.01,.01), legend.bg.color = "grey40",legend.position = c("right", "top"), 
            legend.bg.alpha = 0.8, bg.color = "grey", legend.text.size = 2, legend.title.size = 3)  +
  tm_compass(position=c("left", "top")) +   tm_scale_bar() 

tmap_save(ind_riv, "India_river.jpeg", height = 12, width = 10, units = "in", dpi = 300)
#########################################################################
################ Day 5

library(raster)
library(spatstat)
library(maptools)
library(rgdal)
library(RColorBrewer)
library(tmap)

ind <-shapefile("D:/Work/Ungulate Point process/India_Boundary/India_Boundary.shp")
fire1 <-readOGR("D:/Global rasters/DL_FIRE_J1V-C2_164695/fire_nrt_J1V-C2_164695.shp")
plot(fire1)

xx <-ind@polygons[[1]]@Polygons[[1]]@coords[,1]
yy <-ind@polygons[[1]]@Polygons[[1]]@coords[,2]
Z <- owin(poly=list(x=rev(xx), y=rev(yy)))

loc <-ppp(fire1@data$LONGITUDE,fire1@data$LATITUDE , window = Z)
plot(ind)
plot(loc, add=T, col="red")
summary(loc)

trtl_dnsty <-density(loc, 0.0005) ###half degree density plot
den_trtl <-as.SpatialGridDataFrame.im(trtl_dnsty)
den_trtl_rst <-as(den_trtl, "RasterLayer")
plot(den_trtl_rst, col= brewer.pal(7, "Reds"))
plot(ind, add=T)
plot(loc, add=T, cex=0.25, col ="red")

fire_loc <-tm_shape(ind) + tm_borders("black")+
  tm_graticules(x=c( 70,  80, 90, 100), y = c(0, 10, 20, 30,40), projection = 4326)+
  tm_shape(den_trtl_rst) + 
  tm_raster(palette = brewer.pal(7, "OrRd") , title="Active Fires in last month", saturation = 1,alpha = 1) + 
  tm_shape(fire1)+tm_dots(col= "red", size = 0.025, alpha=0.8)+
  tm_shape(den_trtl_rst) + 
  tm_raster(palette = brewer.pal(7, "Reds") , title="Active Fires in last month", saturation = 1,alpha = 0.2) + 
  tm_legend(legend.outside=F, position=c("right", "top"), legend.show=F )+
  tm_layout(outer.margins = c(.01,.01,.01,.01), legend.bg.color = "grey", 
            legend.bg.alpha = 0.8, bg.color = "grey", main.title.position = "center",
            main.title = "Active fire locations in the Last month")  +
  tm_compass(position=c("left", "top")) +    tm_scale_bar() 
  

tmap_save(fire_loc, "Fire_density.jpeg", height = 9, width = 6, units = "in", dpi = 300)

##############################################################################################
############ Day 8
library(raster)
library(sf)
library(maptools)

gp <-shapefile('D:/Global rasters/global_poly/ne_10m_geography_regions_polys.shp')
gp_d <-subset(gp, gp$featurecla=="Desert")

ocean_shpfl <- shapefile("D:/Work/River bird/ne-ocean-10m/ne_10m_ocean.shp")
coastlines <- st_read("D:/Work/River bird/ne-coastlines-10m/ne_10m_coastline.shp")
coastlines1 <- shapefile("D:/Work/River bird/ne-coastlines-10m/ne_10m_coastline.shp")

cst_buf <- st_buffer(coastlines, 0.01)
cst_buf3 <-as(cst_buf, 'Spatial')
cst_buf1 <- SpatialLines2PolySet(coastlines1)
cst_buf2 <- PolySet2SpatialPolygons(cst_buf1, close_polys = T)

jpeg("Desert_world.jpeg", width = 12, height = 8, units = "in", res = 300)
plot(ocean_shpfl, col= "skyblue1", bg= "lightyellow")
abline(h=0)
abline(h=23.5, lty=2)
abline(h=-23.5, lty=2)
plot(countries50, col ="tan4", add=T)
plot(gp_d, col= "gold", add=T)
text(78, -20, "Indian\nOcean")
text(-120, 0, "Pacific\nOcean")
text(-50, 24, "Atlantic\nOcean")
text(-3, 70, "Arctic\nOcean")
dev.off()

#######################################################################################
############## Day 9

library(raster)
library(RColorBrewer)
library(tmap)
library(rnaturalearth)

elev <-raster('D:/Global rasters/worlddemhi3.tif')
ocean_shpfl <- shapefile("D:/Work/River bird/ne-ocean-10m/ne_10m_ocean.shp")
coastlines1 <- shapefile("D:/Work/River bird/ne-coastlines-10m/ne_10m_coastline.shp")

plot(elev, col =brewer.pal(9, "Greys"), ylim=c(-90,90))
plot(coastlines1, add=T, col ="Grey30")


wrld_elev <-tm_graticules(x=c(-90,  0, 90), y = c(-45, 0,45), projection = 4326)+
  tm_shape(elev) + 
  tm_raster(palette = brewer.pal(9, "Greys") , title="Elevation (in mtrs)", legend.hist = F,
            saturation = 1, breaks = c(0,500,1000,1500,2300,3000,4000,5000,9000)) + 
  tm_shape(coastlines1) + tm_lines("grey30")+
  tm_legend(legend.outside=F, position=c("left", "bottom"), legend.show=T )+
  tm_layout(outer.margins = c(.01,.01,.01,.01), legend.bg.color = "grey80", 
            legend.bg.alpha = 0.8, bg.color = "white", frame = F)  +
  tm_compass(position=c("left", "top")) +    tm_scale_bar() 

  tmap_save(wrld_elev, "World_elevation.jpeg", height = 6, width = 9, units = "in", dpi = 300)

####################################################################################
######################## Day 10
  
  library(INLA)
  library(raster)
  library(ggplot2)
  
  
  ind <-shapefile("D:/Work/Ungulate Point process/India_Boundary/India_Boundary.shp")
  ind_st1 <-shapefile('D:/Global rasters/IND_adm/IND_adm1.shp')
  uk <-subset(ind_st1, ind_st1$NAME_1== "Uttaranchal")
  uk_dem <-(crop(raster('D:/Global rasters/worlddemhi3.tif'),uk))

  uk_pnt <-spsample(uk, 100, type = "stratified")  
  head(uk_pnt@coords)

  mesh2 = inla.mesh.2d(loc = uk_pnt@coords, max.edge = c(0.1, 0.5))
  plot(mesh2, col= "purple")  
  points(uk_pnt@coords,col="red",pch=19)
  plot(uk, add=T, fill= NA, border="Blue", lwd=2)

  ggplot()+
    gg(mesh2,cex=1.5 )+
    gg(uk)+
    gg(uk_pnt, col= "purple", cex=2)+
    labs(x= "Longitude", y= "Latitude")+
    theme_bw()+
    theme(axis.title = element_text(size = 16))+
    coord_equal()

  ggsave("UK_inla.jpg", width = 9, height = 6, units = "in", dpi = 300)  
  
###############################################################################
############################## Day 11
  library(rgl)
  library(raster)
  library(rasterVis)
  library(rayshader)
  
  ind_st1 <-shapefile('D:/Global rasters/IND_adm/IND_adm1.shp')
  uk <-subset(ind_st1, ind_st1$NAME_1== "Uttaranchal")
  sk <-subset(ind_st1, ind_st1$NAME_1== "Sikkim")
  as_mtn<-shapefile('D:/Global rasters/GMBA_mountain inventory/GMBA Mountain Inventory_v1.2-Asia.shp')
  krkm <- subset(as_mtn, as_mtn$Name == "Karakorum")
  hmlya <- subset(as_mtn, as_mtn$Name == "Himalaya")
  hmlya_dem <-(crop(raster('D:/Global rasters/worlddemhi3.tif'),hmlya))
  krkm_dem <-(crop(raster('D:/Global rasters/worlddemhi3.tif'),krkm))
  uk_dem <-(crop(raster('D:/Global rasters/worlddemhi3.tif'),uk))
  sk_dem <-(crop(raster('D:/Global rasters/worlddemhi3.tif'),sk))
  
  him_elev <-raster_to_matrix(hmlya_dem)
  krkm_elev <-raster_to_matrix(krkm_dem)
  him_elev <-raster_to_matrix(uk_dem)
  
  him_elev %>%
    sphere_shade(texture = "desert") %>%
    plot_map()
  
  him_elev %>%
    sphere_shade(texture = "desert") %>%
    add_water(detect_water(him_elev), color = "desert") %>%
    plot_3d(him_elev, zscale = 10, fov = 0, theta = -135, zoom = 0.75, phi = 45, 
            windowsize = c(1000, 800), water = T, watercolor = "lightblue")
  render_label(him_elev, x=100,y= 100, z = 1000, 
               zscale = 50,textcolor = "skyblue", 
               text = "Dehradun", textsize = 2, linewidth = 5)
  Sys.sleep(0.2)
  render_snapshot()
  
  plot(uk_dem)
  rgl.open()
  rgl.bg(color = "black")
  #plot3D(krkm_dem)  
  plot3D(sk_dem, adjust=T)
  plot3
  decorate3d(col= "white", lwd=2)

  ggmtn = krkm_elev %>% 
    melt() %>%
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = value)) +
    geom_contour(aes(x = Var1, y = Var2, z = value), color = "black") +
    scale_x_continuous("X", expand = c(0, 0)) +
    scale_y_continuous("Y", expand = c(0, 0)) +
    scale_fill_gradientn("Z", colours = terrain.colors(10)) +
    coord_fixed()  
  
  par(mfrow = c(1, 1))
  plot_gg(ggvolcano, width = 7, height = 4, raytrace = FALSE, preview = TRUE)
  
  plot_gg(ggmtn, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
          scale = 300, windowsize = c(1000, 666), zoom = 0.6, phi = 30, theta = -30)  
  render_snapshot("uk_dem_3d1.jpeg")
  
#####################################################################################
################# Day 13 and Day 20 and Day 25
  
  library(raster)
  library(tmap)
  library(sf)
  library(tidyverse)
  library(cowplot)
  library(ggspatial)
  
  ind <-read_sf("D:/Work/Ungulate Point process/India_Boundary/India_Boundary.shp")
  ind_st1 <-read_sf('D:/Global rasters/IND_adm/IND_adm1.shp')
  gdp <-read.csv("ind_st_gdp.csv")
  pop <-read.csv("ind_st_pop.csv")
  cvd <-read.csv("Ind_cov.csv")

#ind_st1 %>%  left_join(data, by = c("NAME_1" = "NAME_1"), copy=T) not working 

###join gdp and pop data with the state names
ind_st <-merge(ind_st1, gdp)
ind_st <-merge(ind_st1, pop)
ind_st <-merge(ind_st1, cvd)
head(ind_st)

### define Theme for bivaraite plot...maps can be plotted even without the theme
theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = default_font_family,     color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,          color = NA),
      panel.background = element_rect(fill = default_background_color,            color = NA),
      legend.background = element_rect(fill = default_background_color,             color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,              color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5,             color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5,             color = default_font_color,
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),            debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...   )}

###Plotting the map
ggplot(ind_st) + 
  geom_sf(aes(fill = yr17.18)) 
  
# create 3 buckets for gdp of 2017-18
quantiles_gdp <- ind_st %>%
  pull(yr17.18) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for population
quantiles_pop <- ind_st %>%
  pull(density.1) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for urban population
quantiles_urb <- ind_st %>%
  pull(urban) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for recovery
quantiles_rcv <- ind_st %>%
  pull(recovery) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create 3 buckets for urban population
quantiles_pst <- ind_st %>%
  pull(positive) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#804d36", #3F2949" #2a5a5b, # high popl, high gdp
  "2 - 3" = "#976b82", #5a9178",#435786",
  "1 - 3" = "#9972af", #73ae80", #4885C1", # low popln, high gdp
  "3 - 2" = "#af8353", #567994", #77324C"
  "2 - 2" = "#c8ada0", #90b2b3", #806A8A", # medium popln, medium gdp
  "2 - 1" = "#e4d9ac", #b5c0da",#89A1C8",
  "3 - 1" = "#c8b35a", #6c83b5", #AE3A4E", # high popln, low gdp
  "1 - 2" = "#cbb8d7", #b8d6be", #BC7C8F",
  "1 - 1" = "#e8e8e8" #CABED0" # low popln, gdp
) %>%
  gather("grp", "col_fill")

ind_st2 <-ind_st %>%
  mutate(rcv_quantiles = cut(recovery,
      breaks = quantiles_rcv,
      include.lowest = TRUE ),
    pst_quantiles = cut(positive,
      breaks = quantiles_pst,
      include.lowest = TRUE ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    grp = paste(as.numeric(rcv_quantiles), "-", as.numeric(pst_quantiles)    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "grp")

head(ind_st2)

###Plotting the map
map <-ggplot() +
  geom_sf(data = ind)+
  geom_sf(data=ind_st2, aes(fill = col_fill), color="black") +
  scale_fill_identity()+
  theme_bw()+
  annotation_scale(location= "bl")+
  annotation_north_arrow(location = "tl")
  
  # separate the groups
 col_sc <- bivariate_color_scale %>%
    separate(grp, into = c("rcv", "pst"), sep = " - ") %>%
    mutate(rcv = as.integer(rcv),
           pst = as.integer(pst))

###plotting the legend  
  legend <- ggplot() +
    geom_tile(data = col_sc,
      mapping = aes(
        x = rcv,
        y = pst,
        fill = col_fill)
    ) +
    scale_fill_identity() +
    labs(x = "Higher recovery???",
         y = "Higher test positivity")+
    # make font small enough
    theme(  axis.title = element_text(size = 14) ,
            axis.text = element_blank()) +
    # quadratic tiles
    coord_fixed()
  
  ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.55, 0.085, 0.3, 0.3)
  
ggsave("India_corona_rcv_pst.jpeg", width = 12, height = 8, units = "in", dpi = 300)

##############################################################################
################# Day 14

library(rnoaa)
library(ggplot2)
library(gganimate)
library(dplyr)
library(maptools)
library(raster)
library(sf)
library(rgdal)
options(noaakey = "tCGEwCofJMMJTRczWapwDJdQXRkUlBWf")

proj1 <- "+proj=stere +lat_0=90"

data("wrld_simpl", package = "maptools")                                                                            
wm <- ggplot2::map_data(wrld_simpl)

wm2 <- spTransform(ocean_shpfl, CRSobj = CRS(proj))
plot(wm2)

dat <- sea_ice(year = 2000:2020, month = 'Oct', pole = 'N')
dat1 <- sea_ice(year = 2019, month = 'Nov', pole = 'S')
df1 <- bind_rows(dat1, .id = "x")
df <- bind_rows(dat, .id = "x")
head(df)
df <-transform_coord(df[, 2:3], bind=TRUE)

seaice2 <-SpatialPoints(df1[,2:3],  proj4string=CRS(proj1))

df$x_f = as.numeric(df$x)+1999

###animation plot for the sea-ice change
ga <-ggplot(df)+
  geom_polygon( aes(long, lat, group=group), fill= "steelblue") +
    theme_bw() +coord_equal()+
  theme(axis.title = element_text(size=16))+
  transition_states(states= x_f, state_length = 4, wrap= F)+
 labs(title = 'Year: {closest_state}', size=2)
    
gganimate::animate(ga, nframes=50)

###Map to create polar centric plot in ggplot
ggplot() +
  geom_polygon(data = wm, aes(x = long, y = lat, group = group),
               fill = "grey", colour = "black", alpha = 0.8) +
#geom_polygon(data= df, aes(x= long, y= lat, group = group), fill = "blue")+
    coord_map("ortho", orientation = c(90, 0, 0), xlim = c(-180, 180), ylim = c(65, 90)) +
    scale_y_continuous(breaks = seq(65, 90, by = 5), labels = NULL) 
    
#########################################################################
################ Day 15

library(raster)
library(sf)
library(ggplot2)
library(tmap)

rlwy <-shapefile('D:/Global rasters/Railways/Railways/railways_filtered2.shp')
rlwy_sin <-subset(rlwy, rlwy$Lines=="Single")
rlwy_dbl <-subset(rlwy, rlwy$Lines=="Double")

ind <-shapefile("D:/Work/Ungulate Point process/India_Boundary/India_Boundary.shp")
plot(ind, col= "black")
plot(rlwy, col="yellow",add=T)

ind_rlwy <-tm_graticules(x=c(70,  80, 90, 100), y = c(15, 25,35), projection = 4326)+
  tm_shape(ind) + tm_fill("black")+
  tm_shape(rlwy_sin)+tm_lines("violet", lwd=1.5)+
  tm_shape(rlwy_dbl)+tm_lines("yellow", lwd=2)+
  tm_add_legend(type= "line", col =c("violet", "yellow"), labels = c("Single line", "Double line"), 
                lwd=c(2,2), title= "Type of line" )+
  tm_layout(outer.margins = c(.01,.01,.01,.01), legend.bg.color = "grey80", 
            bg.color = "grey", frame = T, 
            legend.position = c("right","top"), legend.text.size = 1.5,legend.title.size = 2,
            main.title.position = "center",    main.title = "The Great Indian Railway",
            main.title.fontface = 2)  +
  tm_compass(type= "4star", position=c("left", "top")) +    tm_scale_bar() 

tmap_save(ind_rlwy, "Indian_railway.jpeg", height = 9, width = 6, units = "in", dpi = 300)

#######################################################################################
################ Day 16

library(mapview)
library(raster)
library(elevatr)
library(rayshader)
library(RColorBrewer)
library(prettymapr)

ind_st <-shapefile('D:/Global rasters/IND_adm/IND_adm1.shp')
and <-subset(ind_st, ind_st$NAME_1 =="Andaman and Nicobar")

and_elev3 <-get_elev_raster(and, z=12)
and_elv_crp <-mask(and_elev, and)

jpeg("And_nicbr.jpeg", width = 6, height = 6, units = "in", res = 300)
par(mar=c(5,5,2,2))
plot(and_elev, col =brewer.pal(7, "Blues"), xlab= "Longitude", ylab= "Latitude", cex.lab=1.5)
plot(and_elv_crp, legend= F, add=T)
plot(and, add=T, lwd=0.5)
text(95.6, 11.5, "Andaman \nIslands", cex=1.5)
text(91, 7, "Nicobar \nIslands", cex=1.5)
addnortharrow("topleft")
addscalebar(plotunit = "latlon", plotepsg = 4326, pos= "bottomright")
dev.off()

and_elev1 <-raster_to_matrix(and_elev)

and_elev1 %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(and_elev1), color = "desert") %>%
  plot_3d(and_elev1, zscale = 10, fov = 0, theta = -135, zoom = 0.75, phi = 45, 
          windowsize = c(1000, 800), water = T, watercolor = "lightblue", waterdepth= 0)
render_label(him_elev, x=100,y= 100, z = 1000, 
             zscale = 50,textcolor = "skyblue", 
             text = "Dehradun", textsize = 2, linewidth = 5)
Sys.sleep(0.2)

render_snapshot("And_elevation.jpeg")

##########################################################################
############# Day 17

library(raster)
library(tmap)

ers <-shapefile('D:/Global rasters/eurasia.shp/eurasia.shp')
ocean_shpfl <- shapefile("D:/Work/River bird/ne-ocean-10m/ne_10m_ocean.shp")
plot(ers, col= ers$VEG_ID)

plot(ers, col= c("yellow", "green", "yellowgreen","darkgreen",  "blue", "black", "lightgreen","brown",
                 "seagreen",  "tan4","tomato", "red", "wheat","grey30", "grey80","gold","white", "steelblue"))

region_col <- data.frame(
  colr = ers$VEG_ID,
  mycolors = c(rep("#94b8b8",49)), #Assign the same color to all regions initilally
  stringsAsFactors = FALSE
)

region_col$mycolors[region_col$colr == "1"] <- "darkgreen"
region_col$mycolors[region_col$colr == "2"] <- "green"
region_col$mycolors[region_col$colr == "3"] <- "yellowgreen"
region_col$mycolors[region_col$colr == "5"] <- "gold"
region_col$mycolors[region_col$colr == "4"] <- "yellow"
region_col$mycolors[region_col$colr == "6"] <- "lightgreen"
region_col$mycolors[region_col$colr == "7"] <- "khaki"
region_col$mycolors[region_col$colr == "10"] <- "greenyellow"
region_col$mycolors[region_col$colr == "11"] <- "chocolate"
region_col$mycolors[region_col$colr == "12"] <- "cornsilk"
region_col$mycolors[region_col$colr == "13"] <- "tan4"
region_col$mycolors[region_col$colr == "15"] <- "wheat"
region_col$mycolors[region_col$colr == "14"] <- "steelblue"
region_col$mycolors[region_col$colr == "16"] <- "grey30"
region_col$mycolors[region_col$colr == "18"] <- "brown"
region_col$mycolors[region_col$colr == "22"] <- "seagreen"
region_col$mycolors[region_col$colr == "25"] <- "blue"
region_col$mycolors[region_col$colr == "26"] <- "white"

#Import color selection from region_col dataframe into Europe SpatialPolygonDataFrame
ers$color <- region_col$mycolors

#Plot resultant map:
lgm_veg <-tm_graticules(x=c(70, 90, 110,130, 160), y = c(0, 25,45, 65), projection = 4326)+
tm_shape(ers)+ tm_fill("color")+ 
  tm_shape(ers)+tm_borders("black")+
tm_add_legend(type= "fill", col =c("darkgreen", "green", "yellowgreen", "gold", "yellow", "lightgreen",
                                   "khaki",   "greenyellow","chocolate","cornsilk", "tan4","wheat"  ,
                                   "steelblue", "grey30","brown","seagreen","blue", "white"), 
              labels = c("Tropical rainforest",
" Monsoon or dry forest", "Tropical woodland", "Tropical thorn scrub and scrub woodland", 
"Tropical semi-desert" ,	"Tropical grassland", " Tropical extreme desert",
"Montane tropical forest"," Open boreal woodlands", " Semi-arid temperate woodland or scrub",
" Tundra ", " Steppe-tundra", "Polar and alpine desert"," Temperate desert",
"Forest steppe", "Dry steppe", "Lakes and open water", "Ice sheet and other permanent ice"), 
               title= "Vegetation Type" )+
  tm_layout(outer.margins = c(.01,.01,.01,.01), legend.bg.color = "grey80", 
            bg.color = "grey", frame = T, legend.outside = T,
            legend.position = c("right","top"), legend.text.size = 1.5,legend.title.size = 2,
            main.title.position = "center",    main.title = "Vegetation Type of Last Glacial Maximum",
            main.title.fontface = 2)  +
  tm_compass(type= "4star", position=c("left", "bottom")) +    tm_scale_bar() 

tmap_save(lgm_veg, "LGM_vegetation.jpeg", height = 6, width = 9, units = "in", dpi = 300)

#################################################################################################
############## Day 18

library(raster)
library(tmap)
library(rgdal)
library(rasterVis)

cil <-readOGR("D:/Global rasters/Central Indianb Landscape.kml")

lulc_15 <-raster("D:/Global rasters/E060N40_PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.tif")
lulc_115 <-raster("D:/Global rasters/E060N20_PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.tif")

lulc_1 <-raster::merge(lulc_15, lulc_115)
lulc_12 <-crop(lulc_1, cil)

lulc_19 <-raster("D:/Global rasters/E060N40_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")
lulc_119 <-raster("D:/Global rasters/E060N20_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif")

lulc_2 <-raster::merge(lulc_19, lulc_119)
lulc_22 <-crop(lulc_2, cil)

lulc_25 <-raster("D:/Global rasters/E080N40_PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.tif")
lulc_215 <-raster("D:/Global rasters/E080N20_PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.tif")

lulc_21 <-raster::merge(lulc_225, lulc_235)
lulc_225 <-crop(lulc_25, cil)

lulc <-raster::merge(lulc_12, lulc_21)
par(mfrow=c(1,1))
plot(lulc)
plot(cil, add=T)

#define color palette
cpal <- c('gold', 'yellow', 'brown2','red', 'gray','blue','seagreen','green','green',
          'green','green', 'darkgreen', 'darkgreen', 'darkgreen', 'darkgreen')

#make categorical
lulc2 <- ratify(lulc)

#plot
 p1 <- levelplot(lulc2,col.regions=cpal,att='ID')+
    layer(sp.polygons(cil, lwd=2, col ="black"))
p1

###Using ggplot for better control over the graphics
gplot(lulc2) + 
  geom_tile(aes(fill = as.factor(value))) +
  geom_polygon(data = cil, aes(long, lat, group=group), col = "black", fill=NA, lwd=1) +
  scale_fill_manual("Landuse types",
                      breaks= c("20","30","40","50","60","80","90","111","112","114","116", "121","122","124","126"),
                      labels =c("Shrubs",  "Herbaceous vegetation", 
                                "Agriculture", "Built-up", "Bare ground", 
                                "Waterbody", "Wetland", rep("Open forest",4),
                                rep("Closed forest",4)),
                   values =   c("20"="gold", "30"="yellow", "40"="brown2", "50"="red", "60"="gray",
                                "80"= "blue", "90"= "seagreen", "111"="green","112"="green","114"="green","116"="green",
                                "121"="darkgreen","122"="darkgreen","124"="darkgreen","126"="darkgreen"))+
  labs(x= "Longitude", y= "Latitude")+
  theme_bw()+
  theme(axis.title = element_text(size=16), lege)
                     
ggsave("CIL_LULC.jpeg", width = 9, height = 6, dpi = 300)

####################################################################################
############## Day 19

library(raster)
library(tmap)
library(sf)

ocean_shpfl <- shapefile("D:/Work/River bird/ne-ocean-10m/ne_10m_ocean.shp")
cstl <- read_sf("D:/Work/River bird/ne-coastlines-10m/ne_10m_coastline.shp")

plot(ocean_shpfl, col= "skyblue")
data("World")

World.robin <- st_transform(World,"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

tm_graticules(x=c(70, 90, 110,130, 160), y = c(0, 25,45, 65), projection = 4326)+
tm_shape(World.robin) + tm_fill()  

world_mollweide = st_transform(World, crs = "+proj=moll")
worldcst_mollweide = st_transform(cstl, crs = "+proj=moll")

wrld_blnk <-tm_graticules(x=c(-180,-150,-110,-70,-30,0,30,70, 110,150,180), y = c(-65,-45,-25,0, 25,45, 65))+
  tm_shape(worldcst_mollweide) + tm_lines("grey30") 
    
tmap_save(wrld_blnk, "World_map1.jpeg", height = 6, width = 9, units = "in", dpi = 300)

#################################################################################################
################# Day 22
library(raster)
library(tmap)
library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)

elk1 <-shapefile("D:/Data_hub/UngulateMigrations/Elk_AZ_Interstate17_Routes_Ver1_2019/Elk_AZ_Interstate17_Routes_Ver1_2019.shp")

elk2 <-read_sf("D:/Data_hub/UngulateMigrations/Elk_WY_Jackson_Routes_Ver1_2019/Elk_WY_Jackson_Routes_Ver1_2019.shp")
elk3 <-read_sf("D:/Data_hub/UngulateMigrations/Elk_WY_FossilButte_Routes_Ver1_2019/Elk_WY_FossilButte_Routes_Ver1_2019.shp")
elk4 <-read_sf("D:/Data_hub/UngulateMigrations/Elk_WY_SouthWindRiver_Routes_Ver1_2019/Elk_WY_SouthWindRiver_Routes_Ver1_2019.shp")

data("World")
st_crs(World)

wld.us <-  World %>% filter(iso_a3 == "USA") %>% st_transform(crs(elk1))


  tm_shape(elk2$season)+tm_lines()
  
  plot(elk3["season"], xlim=c(-1200000,-1100000), ylim=c(2100000,2500000), bg="black")
  plot(elk2["season"], xlim=c(-1200000,-1100000), ylim=c(2100000,2500000), bg="black")
  
  ggplot() + 
    geom_sf(data = elk2, aes(colour=season))+
    geom_sf(data = elk3, aes(colour=season))+
    geom_sf(data = elk4, aes(colour=season))+
    theme_bw()+
    scale_colour_manual(values=c("#CC6666", "#2a5a5b"), labels= c("Fall", "Spring"))+
    theme(legend.title = element_text(size = 20), legend.text = element_text(size = 14))+
    annotation_scale(location= "bl")
  
  ggplot() + 
    geom_sf(data = elk2, aes(colour=year))+
    geom_sf(data = elk3, aes(colour=year))+
    geom_sf(data = elk4, aes(colour=year))+
    theme_bw()+
    #scale_colour_manual(values=c("#CC6666", "#2a5a5b"))+
    theme(legend.title = element_text(size = 16))+
    annotation_scale(location= "bl")
  
  ggsave("Ungulate_movement.jpeg", width = 10, height = 8, units = "in", dpi = 300)

################################################################################################
################## Day 23
  library(raster)
  library(sf)
  library(tmap)
  cstl <- shapefile("D:/Work/River bird/ne-coastlines-10m/ne_10m_coastline.shp")
  ind <- shapefile("D:/Work/Ungulate Point process/India_Boundary/India_Boundary.shp")
  chn_shp <-shapefile("D:/country_shapefile/CHN_adm0.shp")
  bgd_shp <-shapefile("D:/country_shapefile/BGD_adm0.shp")
  w_in <-raster::intersect(ind, extent(68.106,75.5, 23.55,32.2))
  wld.bgd <-  World %>% filter(iso_a3 == "BGD") %>% st_transform(crs(ind))
  
  
  ind_st1 <-shapefile('D:/Global rasters/IND_adm/IND_adm1.shp')
  arn_ind <-subset(ind_st1, ind_st1$NAME_1=="Arunachal Pradesh")
  e_in <- crop(chn_shp, extent(91.6,97.415, 27.6,30))
  e_in1 <- crop(ind, extent(88,93, 21.6,27))
  e_in2 <- crop(subset(mmr_shp, mmr_shp$Landlocked==0), extent(92.19, 94, 20,22))
  
  plot(ind)
  axis(2)
  plot(wld.bgd, add=T)
  plot(w_in, add=T, lwd=2)
  plot(e_in, add=T, lwd=2)
  
ind_int_line <- tm_graticules(x=c( 70,  80, 90, 100), y = c(0, 10, 20, 30,40), projection = 4326)+
  tm_shape(ind)+tm_polygons(border.col = "black", lwd=2)+
    tm_shape(w_in)+tm_polygons(border.col = "purple", lwd=2)+
  tm_shape(e_in)+tm_polygons(border.col = "blue",col = "grey", lwd=2)+
    tm_shape(bgd_shp)+tm_polygons(border.col = "purple", lwd=2)+
    tm_shape(cstl)+tm_lines("black", lwd=2)+
    tm_add_legend(type= "line", col =c("purple", "blue"), labels = c("Radcliffe line", "McMahon line"), 
                  lwd=c(2,2), title= "Name of \nborder line")+
    tm_layout(outer.margins = c(.01,.01,.01,.01), legend.bg.color = "grey80", 
              bg.color = "grey", frame = T, 
              legend.position = c("right","top"), legend.text.size = 1.5,legend.title.size = 2)  +
    tm_compass(type= "4star", position=c("left", "top")) +    tm_scale_bar() 
  
tmap_save(ind_int_line, "Ind_int_line.jpeg", width = 6.5, height = 8.5, dpi=300, units = "in")    

################################################################################################
################## Day 24

library(raster)
library(tmap)
library(tmaptools)
library(sf)

ind <- shapefile("D:/Work/Ungulate Point process/India_Boundary/India_Boundary.shp")
ind_st1 <-shapefile('D:/Global rasters/IND_adm/IND_adm1.shp')
skm <-subset(ind_st1, ind_st1$NAME_1=="Sikkim")
skm_bb <-cbind(x=c(87.5,89.5,89.5,87.5),y=c(30,30,26,26))
skm_bb_ply <-SpatialPolygons(list(Polygons(list(Polygon(skm_bb)),"1")), proj4string=crs(ind))

ind_skm <-tm_graticules(x=c(70, 80, 90, 110), y = c(10, 20,30), projection = 4326)+
  tm_shape(ind)+ tm_borders("black")+ 
  tm_shape(skm)+tm_fill("grey20")+
  tm_shape(skm_bb_ply)+ tm_borders("black", lwd=2)+
  tm_compass(type= "4star", position=c("right", "top")) +    tm_scale_bar() 

tmap_save(ind_skm, "india_sikkim.jpeg", width = 6, height = 9, units = "in", dpi = 300)

######################################################################################
################# Day 26


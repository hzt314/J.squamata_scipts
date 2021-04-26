rm(list = ls());

library(elevatr)
library(raster)
library(tidyverse)
library(rgdal)
library(scales)


### plot a map of Qinghai-Tibet Pleteau
### reference 1: https://rpubs.com/womeimingzi11/plot-map-by-ggplot2
### reference 2: https://cloud.tencent.com/developer/article/1790200

ext_sample <- extent(70, 105, 25, 45)
bg_init <- raster(ext = ext_sample, resolution = 0.01)
ll<-data.frame(x = c(70, 105),
               y = c(25, 45))
bg_rst <- get_elev_raster(bg_init, z = 5) %>% crop(ext_sample)
loc_df <- data.frame(x = runif(6,min=sp::bbox(lake)[1,1], 
                               max=sp::bbox(lake)[1,2]),
                     y = runif(6,min=sp::bbox(lake)[2,1], 
                               max=sp::bbox(lake)[2,2]))
x <- get_elev_raster(locations = ll, prj = '+init=epsg:4326', z=4)
tp_ext <- readOGR("/Users/han/data/SCU/cmj_project/结果图表/new/map/DBATP/DBATP_Polygon.shp")
tp_rst <- mask(x, tp_ext)

city_ls <- list(x = c(99+58/60+22.10/3600, 97+55/60+35.56/3600), y = c(30+11/60+21.08/3600, 29+42/60+29.55/3600), 
                label = c("Litang", "Zuogong"))
str(city_ls)

bg_rst_df <- as.data.frame(x, xy = TRUE) %>% mutate(alpha = rescale(file133427f5f6cb0, to = c(0.25, 0.75)))
tp_rst_df <- as.data.frame(tp_rst, xy = TRUE) %>% mutate(alpha = ifelse(is.na(file133427f5f6cb0),  0, 1))

scale_parm <- 2
# Init ggplot
gmap<-ggplot() + # plot the backgroun layer, set the alpha without color will make a grey
  # background
  geom_raster(data = bg_rst_df, aes(x = x, y = y, alpha = alpha)) + # plot the topographic layer, set alpha to keep the shape of Tibetan Plateau
  # (TP). Color indicates the elevation.
  geom_raster(data = tp_rst_df, aes(x = x, y = y, fill = file133427f5f6cb0, alpha = alpha)) + # terrain.colors is an built-in function to generate a list of color palettes.
  # set the legend title of evelation by name parameter.
  scale_fill_gradientn(colours = terrain.colors(100), name = "Elevation (m)") + # As we said before, the alphas is used to determine the shpae of TP, we don't
  # need to show them as legends.
  scale_alpha(guide = "none") + # Project this figure as a map but not a normal figure
  coord_quickmap() + # Set preset theme makes things easire
  theme_minimal() + # Set the limititions of axes. `expand` parameter will remove the gaps between
  # the rectangle map and axes.  If you are not sure what's this mean, remove the
  # parameter by yourself and you will find it out.
  scale_x_continuous(limits = c(70, 105), expand = c(0, 0)) + scale_y_continuous(limits = c(25, 
                                                                                            45), expand = c(0, 0)) + # Set the titles of axis
  labs(x = "Longtitude (E)", y = "Laitude (N)") + # remove the background color and background grid, you know the classical
  # ggplot's grid, don't you?
  theme(panel.grid = element_blank(), panel.background = element_blank()) + # Set the size of axis and legend
  theme(axis.title = element_text(size = 7 * scale_parm), axis.text = element_text(size = 6 * 
                                                                                     scale_parm)) + theme(legend.key.width = unit(0.2 * scale_parm, "cm"), legend.key.height = unit(0.5 * 
                                                                                                                                                                                      scale_parm, "cm"), legend.text = element_text(size = 5 * scale_parm), legend.title = element_text(size = 6 * 
                                                                                                                                                                                                                                                                                          scale_parm))
gmap <- gmap +
  # Add the city_ls to the main plot as landmarks.
  geom_text(
    mapping = aes(x = x, y = y, label = label),
    # geom_text don't support the structure we used. 
    # convert the list into data.frame, every element is used as column here.
    data = bind_cols(city_ls),
    size = 2 * scale_parm
  )



### change range of lat and lon to plot in smaller scale
ext_sample_1 <- extent(94, 105, 26, 34)
bg_init_1 <- raster(ext = ext_sample_1, resolution = 0.01)
ll_1<-data.frame(x = c(94, 105),
               y = c(26, 34))
#bg_rst_1 <- get_elev_raster(bg_init_1, z = 5) %>% crop(ext_sample_1)
#loc_df <- data.frame(x = runif(6,min=sp::bbox(lake)[1,1], 
#                               max=sp::bbox(lake)[1,2]),
 #                    y = runif(6,min=sp::bbox(lake)[2,1], 
  #                             max=sp::bbox(lake)[2,2]))
x_1 <- get_elev_raster(locations = ll_1, prj = '+init=epsg:4326', z=5)

bg_rst_df_1 <- as.data.frame(x_1, xy = TRUE) %>% mutate(alpha = rescale(file133422e29ec4d, to = c(0.25, 0.75)))
bg_rst_df_1$file133422e29ec4d[bg_rst_df_1$file133422e29ec4d<0] <- 0
bg_rst_df_1$file133422e29ec4d[1]<-7017
# Init ggplot
gmap_1<-ggplot() + # plot the backgroun layer, set the alpha without color will make a grey
  # background
  geom_raster(data = bg_rst_df_1, aes(x = x, y = y, alpha = alpha, fill = file133422e29ec4d)) + # plot the topographic layer, set alpha to keep the shape of Tibetan Plateau
  # (TP). Color indicates the elevation.
  scale_fill_gradientn(colours = terrain.colors(100), name = "Elevation (m)") + # As we said before, the alphas is used to determine the shpae of TP, we don't
  # need to show them as legends.
  scale_alpha(guide = "none") + # Project this figure as a map but not a normal figure
  coord_quickmap() + # Set preset theme makes things easire
  theme_minimal() + # Set the limititions of axes. `expand` parameter will remove the gaps between
  # the rectangle map and axes.  If you are not sure what's this mean, remove the
  # parameter by yourself and you will find it out.
  scale_x_continuous(limits = c(94, 105), expand = c(0, 0)) + 
  scale_y_continuous(limits = c(26, 34), expand = c(0, 0)) + # Set the titles of axis
  labs(x = "Longtitude (E)", y = "Laitude (N)") + # remove the background color and background grid, you know the classical
  # ggplot's grid, don't you?
  theme(panel.grid = element_blank(), panel.background = element_blank()) + # Set the size of axis and legend
  theme(axis.title = element_text(size = 7 * scale_parm), axis.text = element_text(size = 6 * 
                                                                                     scale_parm)) + theme(legend.key.width = unit(0.2 * scale_parm, "cm"), legend.key.height = unit(0.5 * 
                                                                                                                                                                                      scale_parm, "cm"), legend.text = element_text(size = 5 * scale_parm), legend.title = element_text(size = 6 * 
                                                                                                                                                                                                                                                                                          scale_parm))
gmap_1 +
  geom_point(mapping = aes(x = x, y = y),
             data = bind_cols(city_ls), size = 1.7* scale_parm
    
  )


##### try another style
library(tanaka)

ras <- get_elev_raster(locations = data.frame(x = c(6.7, 7), y = c(45.8,46)),
                       z = 10, prj = "+init=epsg:4326", clip = "locations")
# custom color palette
cols <- c("#F7E1C6", "#EED4C1", "#E5C9BE", "#DCBEBA", "#D3B3B6", "#CAA8B3", 
          "#C19CAF", "#B790AB", "#AC81A7", "#A073A1", "#95639D", "#885497", 
          "#7C4692", "#6B3D86", "#573775", "#433266", "#2F2C56", "#1B2847")
# display the map
tanaka(ras, breaks = seq(500,4800,250), col = cols)

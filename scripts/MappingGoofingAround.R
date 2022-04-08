#Title: Black River Map
#Author: Doug Larson


rm(list=ls());  options(show.error.locations = TRUE); #clean up environment
source(file="scripts/spatial-header.R"); #bring in all mapping packages

BR = st_read(dsn="data/BlackRiver/Black_River.shp");  
BR_SF = st_as_sf(BR); 

RM = st_read(dsn="data/RiverMap/RiverMap7DPoly.shp");  
RM_SF = st_as_sf(RM); 

states = ne_states(country = "United States of America");
states_SF = st_as_sf(states);

BlackLake = st_read(dsn="data/BlackLake.kml");
BlackLake_SF = st_as_sf(BlackLake); 

Antennas = c("FO5", "Section 7", "Section 1", "Kliber Dam");
lng = c(-84.303325, -84.335987, -84.332772, -84.336082);
lat = c(45.467837, 45.416355, 45.406574, 45.395531);
Points = data.frame(Antennas, lat, lng);
Antennas_SF1 = st_as_sf(Points, 
                        coords = c("lng", "lat"),
                        crs = 4326);

#####Plot 1/3 - Black Lake Only#######

plotBR = ggplot() +
  geom_sf(data = states_SF,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "white") +
  geom_sf(data = BR_SF,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "white")+
  geom_sf(data = BlackLake_SF,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "grey")+
  geom_sf(data = Antennas_SF1,
          mapping = aes(geometry = geometry, shape = Antennas), 
          size = 5,
          color = c("black", "black", "black", "black"))+
  scale_shape_manual(values = c("\u25A0","\u25B2","\u25CF","\u2605"))+
  theme_classic()+
  theme(legend.position = (c(0.15,0.125)), 
        legend.title = element_text(face = "bold"))+
  labs(title = NULL,
       x = "Latitude", 
       y = "Longitude");

plotBR2 = plotBR +
  coord_sf(crs = 26988,  
           xlim = c(8206000, 8221000),  # note the negative number (false easting)
           ylim = c(65000, 85000),
           expand = TRUE)+
  annotate(geom="text",
           x=8214500, 
           y=78000,
           label="Black Lake",
           color = "Black",
           fontface = 2)+
  annotate(geom="rect",
           xmin = 8208000,
           xmax = 8209000,
           ymin = 72200,
           ymax = 74200,
           alpha = 0.2,
           linetype=2,
           color = "black",
           fill = "grey80")+
  annotation_scale(location = "br",  
                   width_hint = 0.3,
                   height = unit(0.2, "in"),
                   pad_x = unit(0.1, "in"),
                   bar_cols = c("black", "gray90"),
                   line_col = "gray20",   
                   text_col = "black") +
  annotation_north_arrow(location = "tl", 
                         height = unit(0.5, "in"),
                         width = unit(0.5, "in"),
                         which_north = "true", 
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering(
                           text_col = 'black',
                           line_col = 'black',
                           fill = c("black", "white")));
 #annotate("segment", 
  #         x = 8209000, 
   #        xend = 8213755,
    #       y = 74200, 
     #      yend = 75200, 
      #     colour = "black")+
  #annotate("segment", 
   #        x = 8209000, 
    #       xend = 8213755,
     #      y = 72200, 
      #     yend = 65500, 
       #    colour = "black");

plot(plotBR2); 
saveRDS(plotBR2, file="data/BRZoomedOut.RData");
 

### Plot 2 / 3 - zoomed out US and Canada ###

Lake = "Black Lake"
lngBL = -84.264298;
latBL = 45.464447;
BlackLake = data.frame(Lake, latBL, lngBL);
BlackLake_SF1 = st_as_sf(BlackLake, 
                        coords = c("lngBL", "latBL"),
                        crs = 4326);


Canada = ne_states(country = "Canada");
Canada_sf = st_as_sf(Canada);

plotUSCA = ggplot() +
  geom_sf(data = states_SF,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "grey70") +
  geom_sf(data = Canada_sf,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "grey40") +
  geom_sf(data = BlackLake_SF1,
          mapping = aes(geometry = geometry), 
          size = 8,
          shape = "\U2605", 
          color = "black")+
  theme_void()+
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  labs(title = NULL,
       x = NULL, 
       y = NULL);


plotUSCA2 = plotUSCA +
  coord_sf(crs = 26988,  
           xlim = c(7500000, 8900000),  # note the negative number (false easting)
           ylim = c(-500000, 600000),
           expand = TRUE)+
  annotate(geom="text",
           x=8500000, 
           y=400000,
           label="Canada",
           color = "Black",
           fontface = 2)+
  annotate(geom="text",
           x=7750000,
           y=0,
           label="USA",
           color = "Black",
           fontface = 2)+
  annotation_scale(location = "br",  # options: tr, br, tl, bl
                   #  plot_unit ="m",  # needs to match CRS
                   width_hint = 0.2,
                   bar_cols = c("black", "white"),
                   line_col = "gray20",   
                   text_col = "blue") +
  annotation_north_arrow(location = "tl", 
                         height = unit(0.5, "in"),
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering(
                           text_col = 'black',
                           line_col = 'black',
                           fill = 'white'));
plot(plotUSCA2);  

### Plot 3 / 3 - zoomed in on the river ###
plotBROnly = ggplot() +
  geom_sf(data = BR_SF,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "white")+
  geom_sf(data = Antennas_SF1,
          mapping = aes(geometry = geometry, shape = Antennas), 
          size = 5,
          color = c("black", "black", "black", "black"))+
  scale_shape_manual(values = c("\u25A0","\u25B2","\u25CF","\u2605"))+
  theme_void()+
  theme(panel.border = element_rect(color = "black", fill = NA),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position='none')+
  labs(title = NULL,
       x = NULL, 
       y = NULL);

plotBROnly2 = plotBROnly +
  coord_sf(crs = 26988,  
           xlim = c(8208000, 8209000),  # note the negative number (false easting)
           ylim = c(72500, 74000),
           expand = TRUE)+
  annotate(geom="text",
         x=8208600, 
         y=73400,
         label="Spawning 
         Grounds",
         color = "Black",
         fontface = 2)+
  annotation_scale(location = "bl",  # options: tr, br, tl, bl
                   #  plot_unit ="m",  # needs to match CRS
                   width_hint = 0.2,
                   bar_cols = c("black", "white"),
                   line_col = "gray20",   
                   text_col = "blue") +
  annotation_north_arrow(location = "tr", 
                         height = unit(0.5, "in"),
                         which_north = "true", 
                         style = north_arrow_fancy_orienteering(
                           text_col = 'black',
                           line_col = 'black',
                           fill = 'white'));
 plot(plotBROnly2);  

####Multipaneling(big map) ###

BigMap = arrangeGrob(plotBR2, plotUSCA2, plotBROnly2,
                              layout_matrix = rbind(c(1,1,1,1,1,2,2,2,2,2,1),   
                                                    c(1,1,1,1,1,2,2,2,2,2,1),
                                                    c(1,1,1,1,1,2,2,2,2,2,1),
                                                    c(1,1,1,1,1,2,2,2,2,2,1),
                                                    c(1,1,1,1,1,1,1,1,1,1,1),
                                                    c(1,1,1,1,1,1,1,1,1,1,1),
                                                    c(1,1,1,1,1,1,1,1,1,1,1),
                                                    c(1,1,1,1,1,1,1,1,1,1,1),
                                                    c(1,1,1,1,3,3,3,3,3,3,1),
                                                    c(1,1,1,1,3,3,3,3,3,3,1),
                                                    c(1,1,1,1,3,3,3,3,3,3,1),
                                                    c(1,1,1,1,3,3,3,3,3,3,1),
                                                    c(1,1,1,1,3,3,3,3,3,3,1),
                                                    c(1,1,1,1,3,3,3,3,3,3,1),
                                                    c(1,1,1,1,3,3,3,3,3,3,1),
                                                    c(1,1,1,1,1,1,1,1,1,1,1),
                                                    c(1,1,1,1,1,1,1,1,1,1,1))); 
plot(BigMap)
grid.draw(BigMap)

####Interactive Zoom####

fig <- ggplotly(plotBR)
fig


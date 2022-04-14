#Author: Doug Larson
#Date: 4/7/2022
#Title: App 12

rm(list=ls());  options(show.error.locations = TRUE);
source(file="scripts/spatial-header.R"); 

#### Application
#    1) Make these changes to plot2:
#    - position the compass on the right side and centered vertically
#    - position scale at the top-center 
#        - you need to one of the padding subcomponents
#    - Try to make the scale width about 30% of the plot
#    - increase the height of the scale bar (note: you can use cm or in)
#    - (approximately) double the size of the compass without 
#       changing the aspect ratio (i.e., the shape stays the same)
#    - Add two latitude lines at 40 and 50 degrees
#       - color the lines red
#       - use annotation_spatial_hline 

lakes = st_read(dsn="data/lakes/ne_10m_lakes.shp"); 
lakes_SF = st_as_sf(lakes); 

states = ne_states(country = "United States of America");
states_SF = st_as_sf(states);


plot1 = ggplot() +
  geom_sf(data = states_SF,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "grey") +
  geom_sf(data = lakes_SF,
          mapping = aes(geometry = geometry),
          color = "lightblue",
          fill = "lightblue") +
  coord_sf(crs = 26917,  
           xlim = c(-3000000, 2000000),  # not the ideal CRS...
           ylim = c(3000000, 7000000),
           expand = TRUE);
plot2 = plot1 +
  annotation_scale(location = "tl",  
                   width_hint = 0.3,
                   height = unit(0.5, "in"),
                   pad_x = unit(3, "in"),
                   bar_cols = c("red", "orange"),
                   line_col = "gray20",   
                   text_col = "blue") +
  annotation_north_arrow(location = "tr", 
                         height = unit(1, "in"),
                         width = unit(1, "in"),
                         which_north = "true", 
                         pad_y = unit(2.5, "in"),
                         style = north_arrow_fancy_orienteering(
                           text_col = 'red',
                           line_col = 'blue',
                           fill = 'yellow'))+
   annotation_spatial_hline(intercept = c(40,50), color = "red");
plot(plot2);    

#saveRDS(plot2, file="data/plot.RData");

#   2) Add the compass and scale to a plot with your spatial data
#    - include at least three subcomponent changes for both scale and compass
#    - Add two green longitude lines 

BRZoomedOut = readRDS(file="data/BRZoomedOut.RData");
plot(BRZoomedOut)

ApplicationPlot = BRZoomedOut +
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
                       fill = c("black", "white")))+
  annotation_spatial_vline(intercept = c(-84.30, -84.25), color = "green");
plot(ApplicationPlot)
  

# Application 2
# 1) Redo multi1 so that the picture of the llama and the Lake Michigan plot 
#    are to the right of the main plot.
#    Make sure nothing overlaps (i.e., you can see everything)

plotInfo = readRDS(file="data/plot.RData");

imgJPG = readJPEG("images/alpaca.jpg");   
imgGrobJPG = rasterGrob(imgJPG);

lakeMichigan = st_read(dsn="data/Lake_Michigan_shoreline.kml");
lakeMI_SF = st_as_sf(lakeMichigan); 

plot_LakeMI = ggplot() +
  geom_sf(data = lakeMI_SF,
          mapping = aes(geometry = geometry),
          color = "blue",
          fill = "lightgreen") +
  theme_void() +   
  theme(panel.background=element_rect(color = "black", fill="transparent", 
                                      size=3));
plot(plot_LakeMI);

matrixLayout = matrix(nrow=4, ncol=4, 
                      byrow=TRUE, 
                     
                      data = c(1,  NA, NA, 2,
                               NA, NA, NA, 3,
                               NA, NA, NA, NA,
                               NA, NA,  1, NA));


multi1=arrangeGrob(plotInfo, plot_LakeMI, imgGrobJPG, 
                   top = textGrob(label = "Llamas",
                                  gp=gpar(fontsize=25,
                                          col="blue")), 
                   right="Alpaca",
                   layout_matrix = matrixLayout);
plot(multi1); 

# 2) Create a SF of Lake Michigan that has only the outline of the lake
#     - make the background color of Lake Michigan transparent

plot_LakeMI2 = ggplot() +
  geom_sf(data = lakeMI_SF,
          mapping = aes(geometry = geometry),
          color = "blue",
          fill = "transparent") +
  theme_void() +   
  theme(panel.background=element_rect(color = "black", fill="transparent", 
                                      size=3));

ggsave(filename="images/LakeMI2.jpg", plot=plot_LakeMI2, device=jpeg);
LakeMI2_JPG = readJPEG("images/LakeMI2.jpg");

plot(plot_LakeMI2);

# 3) Create a rasterGrob of your own image

imgRFID = readJPEG("images/InsertRFID.jpg");   
imgGrobRFID = rasterGrob(imgRFID);

# 4) Create a second multipaneling that uses the US map
#   - put the transparent Lake Michigan in the bottom-right corner  
#   - set up the paneling so that your four image corners touches these four states:
#       Idaho, Minnesota, Arizona, and Arkansas 
#   - note: you will need to change the nrow/ncol of the matrixLayout
lakes = st_read(dsn="data/lakes/ne_10m_lakes.shp"); 
lakes_SF = st_as_sf(lakes);

states = ne_states(country = "United States of America");
states_SF = st_as_sf(states);

StatesPlot = ggplot() +
  geom_sf(data = states_SF,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "grey") +
  geom_sf(data = lakes_SF,
          mapping = aes(geometry = geometry),
          color = "lightblue")+
  coord_sf(crs = 4326,    
           xlim = c(-130, -60),  # in degrees...
           ylim = c(20, 60),
           expand = TRUE);
plot(StatesPlot);


matrixLayout = matrix(nrow=17, ncol=13, 
                      byrow=TRUE, 
                      data = c(1,  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,    
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                               NA, NA, NA, NA, 2,  NA, 2,  NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                               NA, NA, NA, NA, 2,  NA, 2,  NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3,  3,  NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 3,  3,  NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1));


multi2=arrangeGrob(StatesPlot, imgGrobRFID, plot_LakeMI2, 
              layout_matrix = matrixLayout);
plot(multi2); 


# Application 3:
# Recreate the two multipanelings from previous lesson using annotation_raster

LakeMI_PNG = readPNG("images/LakeMI.png")

newPlot = plotInfo +
  annotation_raster(imgJPG,     
                    xmin=2100000, xmax=3100000, 
                    ymin=4200000, ymax=5000000) +
  annotation_raster(LakeMI_PNG, 
                    xmin=2100000, xmax=3100000, 
                    ymin=6000000, ymax=7000000) +
  coord_sf(crs = 26917,  
           xlim = c(-3000000, 3500000),  
           ylim = c(3000000, 7000000),
           expand = TRUE);
plot(newPlot);

newPlot2 = plotInfo +
  annotation_raster(imgRFID,     # does not matter JPG vs PNG
                    xmin=-2400000, xmax=-0500000, 
                    ymin=4000000, ymax=5400000) +
  annotation_raster(LakeMI2_JPG, # does matter -- image has transparency
                    xmin=1000000, xmax=2000000, 
                    ymin=3000000, ymax=4000000) +
  coord_sf(crs = 26917,  
           xlim = c(-3000000, 2000000),  
           ylim = c(3000000, 7000000),
           expand = TRUE);
plot(newPlot2);




    


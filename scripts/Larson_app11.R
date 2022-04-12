#Author: Doug Larson
#Date: 4/6/2022
#Title: Lesson 11

#Note - Edited on 4/11/2022 for corrections to application 3

rm(list=ls());  options(show.error.locations = TRUE);

library(package = "sp");       #old Simple Features (but still needed)
library(package = "rgeos");    # getting/converting crs
library(package = "rgdal");    # getting/converting crs
library(package = "ggplot2");
library(package = "dplyr");
library(package = "sf");       # Simple Features
library(package = "rnaturalearth");     # for getting coord data
library(package = "rnaturalearthdata"); # for getting coord data

#### Application 1 #####
#   Add three new points to the data frame 
#   - use only one SF
#   - make each point a different color, size, and shape
#   - make sure the bounding box does not change because of the new points  
museums = st_read(dsn="data/museum.csv");
museums_SF1 = st_as_sf(museums, 
                       coords = c("lng", "lat"),
                       crs = 4326);

museums_SF2 = st_transform(museums_SF1, crs = 3593);

PointDF = data.frame(label = c("\u25A0","\u25B2","\u25CF"),
                    northing = c(12000000, 15000000, 12000000),
                    easting = c(-1200000, -1600000, -2000000));

PointDF_SF = st_as_sf(PointDF,
                     coords = c("northing", "easting"),  # yes, this is opposite of lon,lat
                     crs = 3593);

plotApp1 = ggplot() +
  geom_sf(data = museums_SF2,
          mapping = aes(geometry = geometry),
          color = "red") +
  geom_sf_text(data = PointDF_SF,
               mapping=aes(geometry=geometry, label=label),
               color = c("purple", "green", "blue"),
               size = c(4,5,6)) +
  coord_sf(crs = 26917);
plot(plotApp1);

boundBox2 = st_bbox(museums_SF2);
boundBox2 #Didn't change

#### Application 2 #####
#   Create a new type of shapefile from your own data file
#     - in R, create a SF from your data
#     - in R, save the SF in a different format (KML, geoJSON, SHP)
#   Open the new shapefile in R and plot it


Antennas = c("FO5", "Section 7", "Section 1", "Kliber Dam");
lng = c(-84.303325, -84.335987, -84.332772, -84.336082);
lat = c(45.467837, 45.416355, 45.406574, 45.395531);
Points = data.frame(Points, lat, lng);
Antennas_SF1 = st_as_sf(Points, 
                        coords = c("lng", "lat"),
                        crs = 4326);

BR = st_read(dsn="data/BlackRiver/Black_River.shp");  
BR_SF = st_as_sf(BR); 

states = ne_states(country = "United States of America");
states_SF = st_as_sf(states);

BlackLake = st_read(dsn="data/BlackLake.kml");
BlackLake_SF = st_as_sf(BlackLake); 


if(!file.exists("shapefiles/Antennas.kml"))
{
  # Shapefiles create 4 files: shp, shx, dbf, proj
  st_write(Antennas_SF1, dsn = "shapefiles/Antennas.kml", 
           driver = "kml");
}

AntennaPositions = st_read(dsn="shapefiles/Antennas.kml");
# KML files have the lat, long, and crs built in  -- you (usually) do not need to declare it
Antennas_SF3 = st_as_sf(AntennaPositions); 


plotBR5 = ggplot() +
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
  geom_sf(data = Antennas_SF3,
          mapping = aes(geometry = geometry),
          color = "blue",
          fill = "blue")+
  theme_classic()+
  labs(title = NULL,
       x = "Latitude", 
       y = "Longitude")+
  coord_sf(crs = 26988,  
           xlim = c(8206000, 8221000),  
           ylim = c(65000, 85000),
           expand = TRUE);
plot(plotBR5)

#### Application 3 #####
#   Add a color mapping that distinguishes presidential library from 
#     non-presidential libraries (it's a column in the data frame...)
#   Change the default mapped colors using style_* component 

museums = st_read(dsn="data/museum.csv");
museums_SF = st_as_sf(museums, 
                      coords = c("lng", "lat"),
                      crs = 4326);

lakeMichigan = st_read(dsn="data/Lake_Michigan_shoreline.kml");
lakeMI_SF = st_as_sf(lakeMichigan); 

lakes = st_read(dsn="data/lakes/ne_10m_lakes.shp");  
lakes_SF = st_as_sf(lakes); 

states = ne_states(country = "United States of America");
states_SF = st_as_sf(states);

states_SF$adm0_sr

plotApp3 = ggplot() +
  geom_sf(data = states_SF,
          mapping = aes(geometry = geometry, size=as.factor(adm0_sr))) +
  geom_sf(data = lakes_SF,
          mapping = aes(geometry = geometry, color = min_zoom),
          fill = "lightblue") +
  scale_color_gradient(low = "purple", high = "green")+
  geom_sf(data = lakeMI_SF,
          mapping = aes(geometry = geometry),
          color = "blue",
          fill = "blue")+
  geom_sf_label(data = museums_SF,
          mapping = aes(geometry=geometry, label=substr(museum,
                                                        start=1,
                                                        stop=3), fill = Presidential.Library))+
  labs(color = "Minimum Zoom",
       fill = "Presidental Library",
       size = "State Size");
plotApp3_2 = plotApp3 +
  coord_sf(crs = 4326,    
           xlim = c(-120, -70), 
           ylim = c(20, 60),
           expand = TRUE);
plot(plotApp3_2);

#Application 4

#Change properties of the mappings (uses lesson11e.Mappings.R): 
  #Change the default mapped properties for the four mapped values in the legend 
  #(gradient color in scalerank, linetype in type, color in region, shape in Presidential.Library) 
          #this is in lesson 5: Modifying Mapped Elements all mapped changes use a scale_* components 

  #Change the titles on the four mapped values in the legend. 
          #This can be done using the labs() component (this info is in many lessons including 5) 

plotApp4 = ggplot() +
  geom_sf(data = lakes_SF,
          mapping = aes(geometry = geometry, color=scalerank),
          fill = "lightblue") +
  scale_color_gradient(low = "green", high = "purple")+
  geom_sf(data = states_SF,
          mapping = aes(geometry = geometry, fill=region, linetype=type),
          color = "black") +
  scale_fill_manual(values = c("Orange", "Blue", "seagreen", "coral"))+
  scale_linetype_manual(values = c("dotted", "dashed"))+
  geom_sf_text(data = states_SF,
               mapping = aes(geometry=geometry, label=postal),
               color="yellow") + 
    geom_sf(data = museums_SF,
          mapping = aes(geometry = geometry, shape=Presidential.Library),      
          color = "red", 
          size = 4) +
  scale_shape_manual(values = c("\u25A0","\u2605"))+
  geom_sf(data = lakeMI_SF,
          mapping = aes(geometry = geometry),
          color = "blue",
          fill = "blue")+
  labs(color = "Lake Scale Rank",
       fill = "Region",
       linetype = "Postal District", 
       shape = "Presidential Library");

plotApp4_2 = plotApp4 +
  coord_sf(crs = 4326,    
           xlim = c(-120, -70),  
           ylim = c(20, 60),
           expand = TRUE);
plot(plotApp4_2);




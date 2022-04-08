#Author: Doug Larson
#Date: 3/29/2022
#Title: Larson_app10

rm(list=ls());  options(show.error.locations = TRUE);

library(package = "sp");       
library(package = "rgeos");   
library(package = "rgdal");    
library(package = "ggplot2");
library(package = "sf");       
library(package = "rnaturalearth");     
library(package = "rnaturalearthdata");


#2. How does False Northing and False Easting prevent the use of negative numbers when setting coordinates? 
#Note: "prevent" is the wrong word as you still can use negative number – but, if you are using negative 
#numbers for coordinates, then you are probably using the wrong CRS/Datum. 

#False northing sets the equator to zero in the Northern Hemisphere, whereas the equator is set at 10,000,000 meters 
#southern hemisphere. The idea here is to set each hemisphere such that the neither the southern nor the northern most 
#point of the hemisphere is a negative number. False easting works in a similar way, except UTM splits the earth into 
#6° segments with 500,000 meters as the central meridian for each zone. This way any point east of the central meridian 
#is positive (assuming it's correctly in that UTM  zone), as would be all points west of the central meridian.

#### Group 1:
#  Using a text editor (RStudio is a text editor):    
#    Create a CSV file that has lat/long for Detroit, Chicago, and Toronto
#  Create a Simple Feature from the file

City = c("Detroit", "Chicago", "Toronto");
CityAbv = c("Det", "Chi", "Tor");
lng = c(-83.0458, -87.6298, -79.3832);
lat = c(42.3314, 41.8781, 43.6532);
Cities = data.frame(City, lng, lat, CityAbv);
Cities_SF2 = st_as_sf(Cities, 
                       coords = c("lng", "lat"),
                       crs = 4326); #most common crs in the United States (WGS84)
Cities_SF2

####Group 2
# - Zoom this map into Great Lakes region
# - Use UTM 14N (CRS = 26914)
# - add Canada to the map (from naturalearth.com or using ne_download()
# - add Lake Erie to the map (download from Michigan arcgis)
# - add Detroit, Chicago, Toronto as points to the map
# - Do the same plot with 2 other CRS 

states = ne_states(country = "United States of America");
states_SF = st_as_sf(states);

lakeMichigan = st_read(dsn="data/Lake_Michigan_shoreline.kml");
lakeMI_SF = st_as_sf(lakeMichigan); 

Canada = ne_states(country = "Canada");
Canada_sf = st_as_sf(Canada);

lakeErie = st_read(dsn="data/Lake_Erie_shoreline.kml")
lakeErie_SF = st_as_sf(lakeErie); 

lakes = st_read(dsn="data/lakes/ne_10m_lakes.shp");  
lakes_SF = st_as_sf(lakes); 

plot1 = ggplot() +
  geom_sf(data = states_SF,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "grey") +
  geom_sf(data = Canada_sf,
          mapping = aes(geometry = geometry),
          color = "black",
          fill = "grey60") + 
  geom_sf(data = lakes_SF,
          mapping = aes(geometry = geometry),
          color = "lightblue",
          fill = "lightblue") +
  geom_sf(data = lakeMI_SF,
          mapping = aes(geometry = geometry),
          color = "blue",
          fill = "blue") + 
  geom_sf(data = lakeErie_SF,
          mapping = aes(geometry = geometry),
          color = "blue",
          fill = "blue")+
  geom_sf(data = Cities_SF2,
          mapping = aes(geometry = geometry),      
          color = "red", 
          fill = "red",
          size = 3, 
          shape = 18);
  

###### plot 1 of 3 - CRS = 26914 UTN Zone 14N ######

plot2 = plot1 +
  coord_sf(crs = 26914,  
           xlim = c(1000000, 2500000),  
           ylim = c(4300000, 5450000),
           expand = TRUE);
plot(plot2);

###### plot 2 of 3 - CRS = 26988 NAD83 / Michigan North #####

plot3 = plot1 +
  coord_sf(crs = 26988,  
           xlim = c(7550000, 8900000), 
           ylim = c(-750000, 500000),
           expand = TRUE);
plot(plot3);

##### Plot 3 of 3 - CRS = 3592 NAD83 (NSRS2007) - Michigan South #####

plot4 = plot1 +
  coord_sf(crs = 3592,  
           xlim = c(3250000, 4750000), 
           ylim = c(-375000, 875000),
           expand = TRUE);
plot(plot4);

### Part 5 - Add Abv ###

plot5 = plot2 +
  geom_sf_label(data = Cities_SF2,
                mapping = aes(geometry=geometry, label=substr(CityAbv,1,3)),
                color="Black",
                fill = "White",
                nudge_y = 50000)
plot(plot5);

##### Part 6 ##### Manipulate shape file. 
#Quick note here - I got carried away and didn't use the shape file I put in from last week
#It was only after class last week that I realized that shape file was a big nothing burger. 
#That said, I think I more than acomplished what you were looking for here. 
#Please note, the Michigan map is here, I just zoomed way in. 

BR = st_read(dsn="data/BlackRiver/Black_River.shp");  
BR_SF = st_as_sf(BR); 

RM = st_read(dsn="data/RiverMap/RiverMap7DPoly.shp");  
RM_SF = st_as_sf(RM); 

states = ne_states(country = "United States of America");
states_SF = st_as_sf(states);

BlackLake = st_read(dsn="data/BlackLake.kml");
BlackLake_SF = st_as_sf(BlackLake); 

Points = c("FO5", "Section 7", "Section 1", "Kliber Dam");
lng = c(-84.303325, -84.335987, -84.332772, -84.336082);
lat = c(45.467837, 45.416355, 45.406574, 45.395531);
Antennas = data.frame(Points, lat, lng);
Antennas_SF1 = st_as_sf(Antennas, 
                        coords = c("lng", "lat"),
                        crs = 4236);

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
          mapping = aes(geometry = geometry, color = Points),      
          fill = "red", 
          size = 2,
          pch = 20)+
  theme_bw()+
  labs(title = "Black Lake, Michigan",
       x = "Latitude", 
       y = "Longitude");

plotBR2 = plotBR +
  geom_sf(data = Cities_SF2,
          mapping = aes(geometry = geometry),      
          color = "red", 
          fill = "red",
          size = 3, 
          shape = 18) + 
  coord_sf(crs = 26988,  
           xlim = c(7217500, 9217500),  # note the negative number (false easting)
           ylim = c(-466000, 983000),
           expand = TRUE)+
  annotate(geom="text",
           x=8214500, 
           y=78000,
           label="Black Lake",
           color = "Black");
plot(plotBR2);  

#Answer the following in comments inside your application script: 
  
  #What was your level of comfort with the lesson/application? 

# I was fairly comfortable with the changes, though I think adding text could you some additional explaination. 
  
 #Approximately how long did you work on this lesson? 

# I spent several hours on this, but that's mostly because I wanted to build a useful map of Black Lake. 
  
 #What areas of the lesson/application confused or still confuses you? 

#As you probably noticed, I built most of my maps in UTM, rather than in WGS84, and I'm not sure how intermixable
#those are. For example, I notice that when I mapped the cities in the first map, the cities were projected in 
#WGS 84, however, the map I drew out in UTM, which seemed to work. However, when mapping in Black Lake (part 6), 
#FOR THE LIFE OF ME, I could not get those points to appear on the map. I'm just wondering if this is a projection issue
#or where I went wrong. 

  #What are some things you would like to know more about that is related to, but not covered in, this lesson? 

#Honestly, I just want to see more. For example, I'd love to know how to take the map I made for Black lake, and 
#display that as a pop out on a map of michigan (ie: I want to make a publication grade figure). 
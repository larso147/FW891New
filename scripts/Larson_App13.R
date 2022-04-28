#Author: Doug Larson
#Date: 4/14/2022
#Title: App. 13

rm(list=ls());  options(show.error.locations = TRUE);
source(file="scripts/spatial-header.R"); # moved all package info to header.r

pseudoData = read.csv("data/pseudoData.csv");

### Application 1 (13b):
# 1) Create a script file in your script folder named app13.r
# 2) Create a properly formatted Date object from the pseudoData 
#    columns date2 and date3

Date2Read = as.Date(pseudoData$date2, 
                  format="%Y-%m-%d");
Date2_formatted = format(Date2Read, format="%m-%d-%y");

Date3Read = as.Date(pseudoData$date3, 
                    format="%m/%d/%y")
Date3_formatted = format(Date3Read, format="%m-%d-%y");

# 3) Create a properly formatted POSIXct object from the 
#    pseudoData columns dateTime2 and dateTime3

DateTime2 = as.POSIXct(pseudoData$dateTime2,
                         format="%Y-%m-%d %H:%M:%S");
DateTime2_formatted = format(DateTime2, 
                             format="%m-%d-%y %H:%m"); 

DateTime3 = as.POSIXct(pseudoData$dateTime3,
                       format="%Y-%m-%d %Hh%Mm"); 
DateTime3_formatted = format(DateTime3, 
                             format="%m-%d-%y %H:%m");

# 4) Create a vector that has the dates in this format: 15-April, 2022
#    - add this vector to a column named date_formatted in pseudoData

pseudoData$date_formatted = format(Date2Read, 
                                   format="%d-%B, %Y");

# 5) Create a vector that has the date-times in this format: 09:36 on Fri 04/15/22 
#    - add this vector to a column named dateTime_formatted in pseudoData

pseudoData$dateTime_formatted =  format(DateTime3, 
                                        format="%H:%M on %a %m/%d/%y");

# 6) Create a vector that has the number of seconds since the epoch 
#    - this is since Jan 1, 1970 at midnight GMT (but you do not need this info)
#    - add this vector to a column named epoch in pseudoData

pseudoData$epoch =  format(DateTime3, 
                           format="%s") 

### Application 2 (13C)
# 1) Add the following to the script file app13.r
# 2) Save the GLATOS fish movement data (CRS 4326) in the file 
#    data/Two_Interpolated_Fish_Tracks.csv to a Simple Feature

GLATOSdata = read.csv("data/Two_Interpolated_Fish_Tracks.csv");

GLATOSdata_SF = st_as_sf(GLATOSdata, 
                         coords=c("longitude", "latitude"), 
                         crs=4326);

# 3) The fish are in Lake Erie 
#    - Create a spatial plot of the fish locations over a map of Lake Erie
# 4) Create an animation of the fish movement 
#    - map the record_type to color (use the colors blue and orange)
#    - map the animal_id to shape (use a star and a triangle)
#    - In the title put the time in this format: 15-May @ 4:57AM

lakeErie = st_read(dsn="data/Lake_Erie_shoreline.kml");
lakeErie_SF = st_as_sf(lakeErie); 

stnTime = as.POSIXct(GLATOSdata_SF$bin_timestamp,    
                     format="%m/%d/%Y %H:%M");

plot1 = ggplot() +
  geom_sf(data = lakeErie_SF,
          mapping = aes(geometry = geometry)) +
  geom_sf(data = GLATOSdata_SF,
          mapping = aes(geometry = geometry, 
                        color=record_type, shape = as.factor(animal_id)),size = 4) +
  scale_color_manual(values = c("blue", "orange")) +
  scale_shape_manual(values = c("\u25B2","\u2605")) +
  labs(title = "Timestamp: {format(frame_time, '%d-%b @ %I:%M%p')}",
       color = "Record Type",
       shape = "Individual") +
  coord_sf(crs = 4326,    
           xlim = c(-83.8, -78.8),
           ylim = c(41.20, 43))+
  transition_time(time=stnTime);
plot(plot1);

animate(plot=plot1, 
        nframes = 300,
        fps=10);  

# 5) Save the animation as FishTracks.gif to a folder called images 
#    in your Project Folder
#    - Use 300 frames in the animation (this will take time to render)
#    - have to animation run at 10 frames per second

anim_save(filename = "images/FishTracks.gif",   
          animation = plot1,# plot to animate
          nframes = 300,      # number of frames in animation
          fps = 10); 

# 6) Answer the following questions in comments:
#    a. Why are there many frames without any movement?
#           Either because fish didn't move, or were not detected on a reader.
#    b. Why are there so many points in each of the frames?
#           We compressed 1,366 into 300 frames and then animated them at 
#           10 frames per second. So in each second, there are 45.5-ish points
#           to animate.
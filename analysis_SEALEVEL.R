## FUTUREMARES WP5 - HAZARDS ANALYSIS
## Contact: Juan Bueno Pardo (jbuenopardo@gmail.com)

## CLEAN R MEMORY
rm(list = ls())
ls()

## LOAD LIBRARIES
library (ncdf4) ## if not installed run install.packages ("ncdf4")
library (raster) ## if not installed run install.packages ("raster")

###################################################################################################
## PART TO BE MODIFIED BY USER                                                                   ##
                                                                                                 ##
## DEFINE THE COORDINATES FROM WHERE YOU WANT TO OBTAIN THE TIME SERIES OF THE VARIABLE.         ##
## Be careful, do not set them too close to land, otherwise you will get NA values               ##
lon_wanted <- -9.6                                                                               ##
lat_wanted <- 36                                                                              ##
                                                                                                 ##
###################################################################################################

## SET WORKING DIRECTORY TO THE ONE WITH THE wp2-wp5-analysis.R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()

## READ ONE .nc FILE
## USE "STATS" FILES (NO "PERC")
## CHOOSE THE DEPTH OF THE VARIABLE USING ONE FILE OR THE OTHER (depth_5 vs depth_25)

## List the names of the .nc files in the folder
list.files(pattern = "\\.nc$")

###################################################################################################
## Read the file of interest (substitute the name of the file)
file1 <- nc_open ("historical-tidal_range-v0.0.nc")
file2 <- nc_open ("historical-PCT-prct90-v0.0.nc")
file3 <- nc_open ("rcp45_end_century-PCT-prct90-v0.0.nc")
file4 <- nc_open ("rcp85_mid_century-PCT-prct90-v0.0.nc")

names(file1$var)  
names(file3$var)                                                                 

###################################################################################################

## DEFINE VARIABLES FROM THE FILE
lat <- ncvar_get (file1, varid="station_y_coordinate")
lon <- ncvar_get (file1, varid="station_x_coordinate") 
variable <- ncvar_get (file1, varid="tidal_range")
variable.ref <- ncvar_get(file2, varid="water_level_prct")
variable.future45 <- ncvar_get(file3, varid="water_level_prct")
variable.future85 <- ncvar_get(file4, varid="water_level_prct")
dim (variable)
dim (variable.future85)

## CORRECT LONGITUDE VALUES
lon [which (lon > 200)] <- lon [which (lon > 200)] - 360

## VISUALIZE DATA
plot (x=lon, y=lat, col=variable, pch=19, cex=0.1)
points (x=lon_wanted, y=lat_wanted, col="red", pch=4, cex=0.9)

## OBTAIN VALUE 
positions_available <- c (grep(lat_wanted, lat), grep(lon_wanted, lon))
data.frame (lat [positions_available], lon [positions_available], variable [positions_available])

##########################################################################################################
## Indicate here which row better represents your coordinates of interest (change the number in row 61)  #
row <- 286                                                                                               #
##########################################################################################################

position <- positions_available [row]

if ( length(position) == 0)
{print ("Please adjust a little the value of lon_wanted and lat_wanted in lines 14 & 15 and run again the script from the beginning")}


sigma.ref <- variable [position]
mu.ref <- variable.ref [position]
mu.fut2 <- variable.future85 [position]
mu.fut1 <- variable.future45 [position]


VI1 <- (mu.fut1 - mu.ref) / sigma.ref
VI2 <- (mu.fut2 - mu.ref) / sigma.ref


## VALUES TO PASTE IN THE INDICATOR CALCULATOR (sheet "H data"):
sigma.ref 
mu.ref 
mu.fut 

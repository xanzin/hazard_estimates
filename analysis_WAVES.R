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
lon_wanted <- -5.3                                                                               ##
lat_wanted <- 43.5                                                                               ##
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
file1 <- nc_open ("waves-historical-pp1d-p95-v0.0.nc")
file2 <- nc_open ("waves-rcp45_mid_century-pp1d-p95-v0.0.nc")

names(file1$var)                                                                 
###################################################################################################

## DEFINE VARIABLES FROM THE FILE
lat <- ncvar_get (file1, varid="station_y_coordinate")
lon <- ncvar_get (file1, varid="station_x_coordinate") 
variable <- ncvar_get (file1, varid="p95") ## adjust the name of the variable "chl"
variable.future <- ncvar_get(file2, varid="p95")
dim (variable)

## CORRECT LONGITUDE VALUES
lon [which (lon > 200)] <- lon [which (lon > 200)] - 360

## VISUALIZE DATA
plot (x=lon, y=lat, col=variable, pch=19, cex=0.1)
points (x=lon_wanted, y=lat_wanted, col="red", pch=4, cex=0.9)

## OBTAIN VALUE 
position <- intersect ( grep(lat_wanted, lat), grep(lon_wanted, lon) )

if ( length(position) == 0)
{print ("Please adjust a little the value of lon_wanted and lat_wanted in lines 14 & 15 and run again the script from the beginning")}

## INDICATOR VALUE
mu.ref <- variable [position]
sigma.ref <- sd (variable)
mu.fut <- variable.future [position]

VI1 <- (mu.fut - mu.ref) / sigma.ref

## VALUES TO PASTE INTO THE INDICATOR CALCULATOR
mu.ref 
sigma.ref 
mu.fut 
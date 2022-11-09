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
lon_wanted <- -9.66                                                                               ##
lat_wanted <- 36.24                                                                             ##
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

#############################################################################################################
## Read the file of interest (substitute the name of the file)                                              #
file1 <- nc_open ("pr_EUR-11_IPSL-IPSL-CM5A-MR_rcp45_r1i1p1_SMHI-RCA4_v1_3hr_200601010130-200612312230.nc") #
file2 <- nc_open ("pr_EUR-11_IPSL-IPSL-CM5A-MR_rcp45_r1i1p1_SMHI-RCA4_v1_3hr_204101010130-204112312230.nc") #
#############################################################################################################

## DEFINE VARIABLES FROM THE FILE
lat <- ncvar_get (file1, varid="lat")
lon <- ncvar_get (file1, varid="lon") 
variable.ref <- ncvar_get(file1, varid="pr") ## kg m-2 s-1
variable.future45 <- ncvar_get(file2, varid="pr")

dim (lat)
dim (variable.ref)
dim (variable.future45)

## VISUALIZE DATA
plot (x=lon, y=lat, col=variable.ref[,,1]*1000000, pch=19, cex=0.01)
points (x=lon_wanted, y=lat_wanted, col="red", pch=4, cex=10)

## OBTAIN VALUE 
## MAKE RASTER
r <- raster(t(variable.ref [,,1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot (r)

## EXTRACT DATA AT THE COORDINATES OF INTEREST (lon_wanted, lat_wanted) DEFINED ABOVE
r_present_brick <- brick (variable.ref, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_present_brick <- flip  (t (r_present_brick), direction='y')
variable.time.series    <- extract (r_present_brick, SpatialPoints(cbind(lon_wanted, lat_wanted)), method='simple')

r_fut_brick <- brick (variable.future45, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_fut_brick <- flip  (t (r_fut_brick), direction='y')
variable.time.series.fut    <- extract (r_fut_brick, SpatialPoints(cbind(lon_wanted, lat_wanted)), method='simple')


## CREATE OUTPUT DATA FRAME 
## TIME
time.series <- seq(
  from = as.POSIXct("2006-01-01 0:00", tz="UTC"),
  length.out = length(variable.time.series),
  by   = "3 hours" )  

time.series.fut1 <- seq(
  from = as.POSIXct("2041-01-01 0:00", tz="UTC"),
  length.out = length(variable.time.series),
  by   = "3 hours" )  

df.variable.time.series <- data.frame (date = time.series, variable = t(variable.time.series))
var <- df.variable.time.series$variable

df.variable.time.series.fut <- data.frame (date = time.series.fut1, variable = t(variable.time.series.fut))
var.fut <- df.variable.time.series.fut$variable

## PLOT OF TIME SERIES OF VARIABLE
plot (y=var, x=time.series, type="l", xlab="Time", ylab="Variable")

## NOW CREATE THE TIME SERIES TO CALCULATE THE VARIATION INDEX IN THE IndicatorCalculator.xls PROVIDED (SHEET "H data")

mu.ref <- mean (var)
sigma.ref <- sd (var)
mu.fut1 <- mean (var.fut)

VI1 <- ( (mu.fut1 - mu.ref) / sigma.ref )

## VALUES TO INTRODUCE IN THE INDICATOR CALCULATOR
mu.ref
sigma.ref
mu.fut1




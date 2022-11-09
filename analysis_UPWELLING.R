## FUTUREMARES WP5 - HAZARDS ANALYSIS
## Contact: Juan Bueno Pardo (jbuenopardo@gmail.com)

## CLEAN R MEMORY
rm(list = ls())
ls()

## LOAD LIBRARIES
library (ncdf4) ## if not installed run install.packages ("ncdf4")
library (raster) ## if not installed run install.packages ("raster")

##################################################################################################################
## DEFINE THE COORDINATES FROM WHERE YOU WANT TO OBTAIN THE TIME SERIES OF THE VARIABLE.                         #
## Be careful, do not set them too close to land, otherwise you will get NA values                               #
lon_wanted <- -8.9                                                                                               #
lat_wanted <- 40.5                                                                                               #
                                                                                                                 #
## THE UPWELLING INDEX IS CALCULATED BASED ON THE DIFFERENCES OF TEMPERATURE INSHORE - OFFSHORE                  #
## These coordinates refer to the offshore point to consider. It should be placed at the continental slope       #
lon_wanted_OFFSHORE <- -9.9                                                                                     #
lat_wanted_OFFSHORE <- 40.5                                                                                      #
##################################################################################################################

## SET WORKING DIRECTORY TO THE ONE WITH THE wp2-wp5-analysis.R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()

## READ ONE .nc FILE
## USE STATS FILES (NO PERC)
## CHOOSE THE DEPTH OF THE VARIABLE USING ONE FILE OR THE OTHER (depth_5 vs depth_25)

list.files()

file1 <- nc_open ("thetao_ensemble_sd+ba_surface_depth_25.0_stats_ssp245.nc")
names(file1$var)

## DEFINE VARIABLES FROM THE FILE
lat <- ncvar_get (file1, varid="lat")
lon <- ncvar_get (file1, varid="lon") 
time <- ncvar_get (file1, varid="time") # days since 1993-02-01

variable <- ncvar_get (file1, varid="thetao_mean") ## adjust the name of the variable "chl"
dim (variable)

## MAKE RASTER
r <- raster(t(variable [,,1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot (r)
points (x=lon_wanted, y=lat_wanted, pch=19, col="blue")
points (x=lon_wanted_OFFSHORE, y=lat_wanted_OFFSHORE, pch=19, col="red")

## EXTRACT DATA AT THE COORDINATES OF INTEREST (lon_wanted, lat_wanted) DEFINED ABOVE
r_present_brick <- brick (variable, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_present_brick <- flip  (t (r_present_brick), direction='y')
variable.time.series    <- extract (r_present_brick, SpatialPoints(cbind(lon_wanted, lat_wanted)), method='simple')
variable.time.series.offshore    <- extract (r_present_brick, SpatialPoints(cbind(lon_wanted_OFFSHORE, lat_wanted_OFFSHORE)), method='simple')

## CREATE OUTPUT DATA FRAME 
## TIME
time.series <- seq(
  from = as.POSIXct("1993-02-01 0:00", tz="UTC"),
  length.out = length(time),
  by   = "month" )  

df.variable.time.series <- data.frame (date = time.series, onshore = t(variable.time.series), offshore = t(variable.time.series.offshore))
var <- df.variable.time.series$onshore - df.variable.time.series$offshore

## PLOT OF TIME SERIES OF VARIABLE
plot (y=var, x=time.series, type="l", xlab="Time", ylab="Variable")

## NOW CREATE THE TIME SERIES TO CALCULATE THE VARIATION INDEX IN THE IndicatorCalculator.xls PROVIDED (SHEET "H data")
ref.period <- 1995:2014
fut1.period <- 2041:2060
fut2.period <- 2081:2100

## reference period (1995-2014)
df.variable.ref.period <- var [ format(df.variable.time.series$date, "%Y") %in% ref.period ]

## future 1 period (2041-2060)
df.variable.fut1.period <- var [ format(df.variable.time.series$date, "%Y") %in% fut1.period ]

## future 2 period (2081-2100)
df.variable.fut2.period <- var [ format(df.variable.time.series$date, "%Y") %in% fut2.period ]

## FROM HERE, TO OBTAIN THE VALUE OF THE HAZARD INDICATOR
mu.fut <-  mean (df.variable.fut1.period)
mu.ref <- mean (df.variable.ref.period)
sigma.ref <- sd (df.variable.ref.period)

VI1 <- ( mu.fut - mu.ref ) / sigma.ref

## VALUES TO COPY IN THE INDICATOR CALCULATOR
mu.fut 
mu.ref 
sigma.ref 

## In case of negative values you might want to consider absolute values to obtain the value of the indicator
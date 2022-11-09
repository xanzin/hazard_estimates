## FUTUREMARES WP5 - HAZARDS ANALYSIS
## Contact: Juan Bueno Pardo (jbuenopardo@gmail.com)

## CLEAN R MEMORY
rm(list = ls())
ls()

## LOAD LIBRARIES
library (ncdf4) ## if not installed run install.packages ("ncdf4")
library (raster) ## if not installed run install.packages ("raster")

#############################################################################################
## DEFINE THE COORDINATES FROM WHERE YOU WANT TO OBTAIN THE TIME SERIES OF THE VARIABLE.    #
## Be careful, do not set them too close to land, otherwise you will get NA values          #
lon_wanted <- -5.4                                                                          #
lat_wanted <- 44.5                                                                          # 
#############################################################################################

## SET WORKING DIRECTORY TO THE ONE WITH THE wp2-wp5-analysis.R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()
list.files()

#############################################################################################
## READ ONE .nc FILE                                                                        #
## USE STATS FILES (NO PERC)                                                                #
## CHOOSE THE DEPTH OF THE VARIABLE USING ONE FILE OR THE OTHER (depth_5 vs depth_25)       #
file1 <- nc_open ("thetao_ensemble_sd+ba_surface_depth_25.0_stats_ssp245.nc")               #
names(file1$var)                                                                            #
#############################################################################################

## DEFINE VARIABLES FROM THE FILE
lat <- ncvar_get (file1, varid="lat")
lon <- ncvar_get (file1, varid="lon") 
time <- ncvar_get (file1, varid="time") # days since 1993-02-01

variable <- ncvar_get (file1, varid="thetao_mean") ## adjust the name of the variable
dim (variable)

## MAKE RASTER
r <- raster(t(variable [,,1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')
plot (r)

## EXTRACT DATA AT THE COORDINATES OF INTEREST (lon_wanted, lat_wanted) DEFINED ABOVE
r_present_brick <- brick (variable, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r_present_brick <- flip  (t (r_present_brick), direction='y')
variable.time.series    <- extract (r_present_brick, SpatialPoints(cbind(lon_wanted, lat_wanted)), method='simple')

## CREATE OUTPUT DATA FRAME 
## TIME
time.series <- seq(
  from = as.POSIXct("1993-02-01 0:00", tz="UTC"),
  length.out = length(time),
  by   = "month" )  

df.variable.time.series <- data.frame (date = time.series, variable = t(variable.time.series))
var <- df.variable.time.series$variable

## PLOT OF TIME SERIES OF VARIABLE
plot (y=var, x=time.series, type="l", xlab="Time", ylab="Variable")

## NOW CREATE THE TIME SERIES TO CALCULATE THE VARIATION INDEX IN THE IndicatorCalculator.xls PROVIDED (SHEET "H data")
ref.period <- 1995:2014
fut1.period <- 2041:2060
fut2.period <- 2081:2100

## reference period (1995-2014)
df.variable.ref.period <- df.variable.time.series [ format(df.variable.time.series$date, "%Y") %in% ref.period, ]

## DEFINE THRESHOLD FOR EXTREME EVENTS
THRESHOLD <- quantile (df.variable.ref.period$variable, .975) ## percentile 97.5%
abline (h=THRESHOLD, col="red") # for the plot

## HOW MANY TIMES IN A MONTH THE THRESHOLD IS ATTAINED?
months.threshold.count <- rep(0, length(time.series))
names (months.threshold.count) <- format(df.variable.time.series$date, "%Y")

months.threshold <- format(df.variable.time.series$date, "%Y") [which (df.variable.time.series$variable >= THRESHOLD)]
months.threshold.table <- table (months.threshold)

months.threshold.count [names(months.threshold.table)] <- months.threshold.table

## TIME SERIES TO PASTE IN THE IndicatorCalculator.xls TEMPLATE PROVIDED
timeseries.ref <- months.threshold.count [names(months.threshold.count) %in% ref.period]
timeseries.fut1 <- months.threshold.count [names(months.threshold.count) %in% fut1.period]
timeseries.fut2 <- months.threshold.count [names(months.threshold.count) %in% fut2.period]

## CALCULATE HERE THE VARIATION INDEX
mu.ref  <- mean (timeseries.ref)
sigma.ref  <- sd (timeseries.ref)
mu.fut1 <- mean (timeseries.fut1)
mu.fut2 <- mean (timeseries.fut2 )

## CALCULATE THE VARIATION INDEX
VI1 <- (mu.fut1 - mu.ref) / sigma.ref
VI2 <- (mu.fut2 - mu.ref) / sigma.ref

## VALUES TO PASTE IN THE INDICATOR CALCULATOR
mu.ref 
sigma.ref 
mu.fut1 
mu.fut2

## VALUES TO INTRODUCE IN THE INDICATOR CALCULATOR:
mu.ref  
sigma.ref  
mu.fut1 
mu.fut2 

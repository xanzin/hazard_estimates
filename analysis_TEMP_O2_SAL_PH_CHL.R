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
lon_wanted <- -5.5                                                                    ##
lat_wanted <- 48                                                                            ##
                                                                                                 ##
## DEFINE HERE THE VARIABLE NAME (from one of these: chla, oxygen, salinity, temperature, ph)    ##
variable <- "temperature"                                                                                 ##
###################################################################################################

if (variable == "temperature") {var.name <- "thetao"}
if (variable == "chla")        {var.name <- "chl"}
if (variable == "oxygen")      {var.name <- "o2"}
if (variable == "salinity")    {var.name <- "so"}
if (variable == "ph")          {var.name <- "ph"}

## SET WORKING DIRECTORY TO THE ONE WITH THE wp2-wp5-analysis.R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()

## READ ONE .nc FILE
## USE "STATS" FILES (NO "PERC")
## CHOOSE THE DEPTH OF THE VARIABLE USING ONE FILE OR THE OTHER (depth_5 vs depth_25)

## List the names of the .nc files in the folder
list.files(pattern = "\\.nc$")

###################################################################################################
## Read the file of interest (substitute the name of the file)                                    #
file1 <- nc_open ("thetao_ensemble_sd+ba_surface_depth_5.0_stats_ssp585.nc")                         #
names(file1$var)                                                                                  #
###################################################################################################

## DEFINE VARIABLES FROM THE FILE
lat <- ncvar_get (file1, varid="lat")
lon <- ncvar_get (file1, varid="lon") 
time <- ncvar_get (file1, varid="time") # days since 1993-02-01

variable <- ncvar_get (file1, varid=paste(var.name, "mean", sep="_")) ## adjust the name of the variable "chl"
dim (variable)

## MAKE RASTER
r <- raster(t(variable [,,1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction='y')


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

#######################################################################################################
## CHOOSE SEASON TO CONSIDER                                                                         ##
## Write below the season wanted ("year", "spring", "summer", "autumn", "winter")                    ##
season.wanted.name <- "summer"                                                                       ##
## State below the numbers of the months corresponding to the season wanted.                         ##
## Consider spring as the season when ecosystem productivity is higher                               ##
## If season wanted = "year", state months 1,2,3,4,5,6,7,8,9,10,11,12                                ##
season.wanted.months <- c(6,7,8)                                                                     ##
#######################################################################################################

## Find the positions of the time series corresponding to the season of interest
positions.season <- which (as.numeric (format (df.variable.time.series$date, "%m")) %in% season.wanted.months)
df.variable.time.series <- df.variable.time.series [positions.season, ]

var <- df.variable.time.series$variable
time.series <- df.variable.time.series$date

## PLOT OF TIME SERIES OF VARIABLE
# plot (y=var, x=time.series, type="l", xlab="Time", ylab="Variable")

## NOW CREATE THE TIME SERIES TO CALCULATE THE VARIATION INDEX IN THE IndicatorCalculator.xls PROVIDED (SHEET "H data")
ref.period <- 1995:2014
fut1.period <- 2041:2060
fut2.period <- 2081:2100

## reference period (1995-2014)
df.variable.ref.period <- df.variable.time.series [ format(df.variable.time.series$date, "%Y") %in% ref.period, ]

## future 1 period (2041-2060)
df.variable.fut1.period <- df.variable.time.series [ format(df.variable.time.series$date, "%Y") %in% fut1.period, ]

## future 2 period (2081-2100)
df.variable.fut2.period <- df.variable.time.series [ format(df.variable.time.series$date, "%Y") %in% fut2.period, ]

## OBTAIN THE VALUE OF THE HAZARD INDICATOR
mu.ref <- mean (df.variable.ref.period$variable)
mu.fut1 <- mean (df.variable.fut1.period$variable)
mu.fut2 <- mean (df.variable.fut2.period$variable)
sigma.ref <- sd (df.variable.ref.period$variable)

VI1 <- ( mu.fut1 - mu.ref ) / sigma.ref ## VI1 is the value of the indicator during during future1
VI2 <- ( mu.fut2 - mu.ref ) / sigma.ref ## VI2 is the value of the indicator during during future2
## WHEN NEGATIVE VALUES (IF RELEVANT FROM AN ECOLOGICAL PERSPECTIVE), CONSIDER THE ABSOLUTE VALUE
  
## VALUES TO PASTE IN THE INDICATOR CALCULATOR (sheet "H data"):
mu.ref 
mu.fut1
mu.fut2
sigma.ref
VI1
VI2

#############################
plot (r)
mtext (side=3, "Surface temperature RCP 2.6", line=1)

## MED
points (y=43, x=3.6, col="red", pch=15)
points (y=35, x=35, col="red", pch=15)


## Bay of Biscay
points (y=44, x=-5, col="red", pch=15)
points (y=47.4, x=-3.13, col="red", pch=15)
points (y=43, x=-10, col="red", pch=15)
points (y=46, x=-7.5, col="red", pch=15)
points (y=39, x=-10, col="red", pch=15)
points (y=48, x=-5.5, col="red", pch=15)


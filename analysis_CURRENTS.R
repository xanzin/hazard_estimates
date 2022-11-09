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
lon_wanted <- -5.4                                                                               ##
lat_wanted <- 44.5                                                                               ##
                                                                                                 ##
###################################################################################################

## SET WORKING DIRECTORY TO THE ONE WITH THE wp2-wp5-analysis.R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
getwd()

## READ ONE .nc FILE
## USE "STATS" FILES (NO "PERC")
## CHOOSE THE DEPTH OF THE VARIABLE USING ONE FILE OR THE OTHER (depth_5 vs depth_25)

## List the names of the .nc files in the folder
files <- list.files(pattern = "\\.nc$")

files.ref <- files [grep ("200", files)]
files.fut1 <- files [grep ("204", files)]
files.fut2 <- files [grep ("208", files)]

files.ref.u <- files.ref [grep ("uo", files.ref)]
files.fut1.u <- files.fut1 [grep ("uo", files.ref)]
files.fut2.u <- files.fut2 [grep ("uo", files.ref)]

files.ref.v <- files.ref [grep ("vo", files.ref)]
files.fut1.v <- files.fut1 [grep ("vo", files.ref)]
files.fut2.v <- files.fut2 [grep ("vo", files.ref)]


## Read the file of interest (substitute the name of the file) 

extract.time.series <- function (want.to.analyze = "ref.u")
{
  time.series <- NA
  
  files.to.analyze <- get (paste("files.", want.to.analyze, sep=""))
  
  for (i in 1:length(files.to.analyze))
  {
    file1 <- nc_open (files.to.analyze [i])     
    names(file1$var)                        

    ## Define variables
    lat <- ncvar_get (file1, varid="lat_bnd")
    lon <- ncvar_get (file1, varid="lon_bnd") 
    if (want.to.analyze == "ref.u" | want.to.analyze == "fut1.u" | want.to.analyze == "fut2.u") {variable <- ncvar_get (file1, varid="uo")} ## water velocity in m s-1
    if (want.to.analyze == "ref.v" | want.to.analyze == "fut1.v" | want.to.analyze == "fut2.v") {variable <- ncvar_get (file1, varid="vo")} ## water velocity in m s-1
    
    dim (variable) ## 43 vertical layers, 31/30/28 days depending on the month
    n.days <- dim (variable) [4]

    ## MAKE RASTER
    r <- raster(t(variable [,,1,1]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    r <- flip(r, direction='y')
    plot (r)

    ## EXTRACT DATA AT THE COORDINATES OF INTEREST (lon_wanted, lat_wanted) DEFINED ABOVE
    r_present_brick <- brick (variable[,,1,], xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon), crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    r_present_brick <- flip  (t (r_present_brick), direction='y')
    variable.time.series    <- extract (r_present_brick, SpatialPoints(cbind(lon_wanted, lat_wanted)), method='simple')

    ## CREATE OUTPUT DATA FRAME 
    df.variable.time.series <- data.frame (date = seq(1:n.days), variable = t(variable.time.series))
    var <- df.variable.time.series$variable

    ## PLOT OF TIME SERIES OF VARIABLE
    plot (y=var, x=seq(1:n.days), type="l", xlab="Time", ylab="Variable")
    
    ## PASTE TO TOTAL TIME SERIES
    time.series <- c(time.series, var)
  }
  
  return (time.series)
}
    
      
## CALL THE FUNCTION
time.series.ref.u <- extract.time.series (want.to.analyze = "ref.u")
time.series.ref.v <- extract.time.series (want.to.analyze = "ref.v")
time.series.fut1.u <- extract.time.series (want.to.analyze = "fut1.u")
time.series.fut1.v <- extract.time.series (want.to.analyze = "fut1.v")
time.series.fut2.u <- extract.time.series (want.to.analyze = "fut2.u")
time.series.fut2.v <- extract.time.series (want.to.analyze = "fut2.v")

## CALCULATE WATER SPEED
time.series.ref.speed <- sqrt (time.series.ref.u^2 + time.series.ref.v^2)
plot (time.series.ref.speed, type="l")
mu.ref <- mean (time.series.ref.speed, na.rm=TRUE)
sigma.ref <- sd (time.series.ref.speed, na.rm=TRUE)
      
time.series.fut1.speed <- sqrt (time.series.fut1.u^2 + time.series.fut1.v^2)
plot (time.series.fut1.speed, type="l")
mu.fut1 <- mean (time.series.fut1.speed, na.rm=TRUE)

time.series.fut2.speed <- sqrt (time.series.fut2.u^2 + time.series.fut2.v^2)
plot (time.series.fut2.speed, type="l")
mu.fut2 <- mean (time.series.fut2.speed, na.rm=TRUE)

#### 1. Converting EMIT's satelite data into a csv file #
### adapted from Anna Schweiger's code - github.com/annakat #

# Setup #
library(RNetCDF) # netcdf files

# INput path for the EMIT tile
fp <- file.path("R_input", "EMIT_L2A_RFL_001_20220826T174251_2223812_006.nc") #path to the nedCDF file

# Preparing data #
dat <- ncdf4::nc_open(fp) # open datacube 
refl <- ncdf4::ncvar_get(dat, "reflectance") # extract reflectance
wvl <- ncdf4::ncvar_get(dat, dat$var$`sensor_band_parameters/wavelengths`) # extract wavelengths
lat <- ncdf4::ncvar_get(dat, "location/lat") # extract latitude
lon <- ncdf4::ncvar_get(dat, "location/lon") # extract longitude


#### info for ENVI header
# dat$var$`location/lon`$size


# Creating matrix #
pixels<-dim(refl)[2]*dim(refl)[3] # number of pixels in the datacube
spec<-matrix(nrow = pixels, ncol = length(wvl)+4) # outlining matrix
colnames(spec) <- c("dtrk","ctrk","lat","lon",wvl) # name the columns
idx<-1 # start at row one
for (j in 1:dim(refl)[2]) { # downtrack
  for (i in 1:dim(refl)[3]) { # crosstrack
    nlat<-lat[j,i] # latitude at one pixel
    nlon<-lon[j,i] # longitude at one pixel
    rfl<-refl[,j,i] # spectra at one pixel
    spec[idx,1]<-j # add downtrack index
    spec[idx,2]<-i # add crosstrack index
    spec[idx,3]<-nlat # add latitude
    spec[idx,4]<-nlon # add longitude
    spec[idx,5:length(spec[1,])]<-rfl # add reflectance data
    idx<-idx+1 # move to next row
  }
} # should take about a minute
spec <- data.frame(spec, check.names=F)

rm(dat, lat, lon, fp, i, idx, j, nlat, nlon, pixels, refl, rfl, wvl) # cleanup


# Save file #
points <- subset(spec, select = c("lon","lat")) # extract the coodinates only
points <- data.frame(points)
write.csv(points, file = file.path("R_output", "Points.csv"), row.names = FALSE)
write.csv(spec,   file = file.path("R_output", "full_spectra.csv"), row.names = FALSE)

# Cleanup! 
nc_close(dat)

### 3. Extracting class membership for XY coordinates #

# Setup #
library(sp) # for spatial data
library(raster) # for raster data
library(dplyr) # for consolidations

# Input/output paths
shp_fp   <- file.path("R_input", "RockPolys.shp") # load the shapefile for the Henry map 
points_fp <- file.path("R_output", "Points.csv") 

# Read shapefile
units <- raster::shapefile(shp_fp)
plot(units) # verify shapefile visually

# Labeling #
uni <- units["Code"] # class codes as factors
uni$Code <- as.factor(uni$Code)

points <- read.csv(points_fp) # test points

poi_sp <- SpatialPointsDataFrame(coords = cbind(points$lon,points$lat), # GPS points are in WGS84
                                 data=points,proj4string=CRS("EPSG:4326"))


### Transform coordinates of points to match geologic map
poi_UTM <- spTransform(poi_sp,CRSobj =CRS("EPSG:26911"))

### check if there's overlap
plot(uni)
plot(poi_UTM, cex=2, col="red", pch=16, add=T)


# Rasterize shapefile
exi <- extent(uni) # bounds of shapefile
empty_ras <- raster(exi, resolution=60, crs="EPSG:26911") # create empty raster based on extent
uni_ras <- rasterize(uni,empty_ras, field="Code") 

# Create lookup table for code names: check if ok
look <- as.data.frame(matrix(levels(uni$Code), nrow=length(levels(uni$Code)))) # code names from shapefile
names(look)<- "Code"
look$ID <- seq(1:nrow(look)) # assigen id to each code

#plot(uni_ras)
#plot(poi_UTM, add=T)

# Extract data
poi_classes <- raster::extract(uni_ras, poi_UTM, sp = TRUE)
dat <- as.data.frame(poi_classes)

# Data wrangling
names(dat)[3:5] <- c("ID", "X_EPSG_26911", "Y_EPSG_26911")
dada <- merge(dat,look, by="ID")

# Clean up 
rm("dat","empty_ras","exi","look","poi_classes","poi_sp","poi_UTM","points","uni","uni_ras","units")
dada<-dada[,-1]
dada<-dada[,c(5,2,1,3,4)]

# Save files #
write.csv(dada, file = file.path("R_output", "UnitCoord.csv"), row.names = FALSE)

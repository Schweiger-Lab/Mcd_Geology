## 2. NDVI Masking (Vegetation / Non-Vegetation) ##

library(RNetCDF)
library(raster)
library(ncdf4)
library(ggplot2)

# Input path 
spec <- read.csv(file.path("R_output", "full_spectra.csv"), check.names = FALSE)

# Identify bands to use for NDVI
red_band <- which.min(abs(as.numeric(colnames(spec)[-(1:4)]) - 660))
nir_band <- which.min(abs(as.numeric(colnames(spec)[-(1:4)]) - 860))

red <- spec[, 4 + red_band]
nir <- spec[, 4 + nir_band]
ndvi <- (nir - red) / (nir + red)

# Mask non-veg
spec_nonveg <- spec[ndvi <= 0.35, ]
spec_veg    <- spec[ndvi > 0.35, ]

# Save! 
write.csv(spec_nonveg, file.path("R_output", "full_spectra_nonveg.csv"), row.names = FALSE)
write.csv(spec_veg,    file.path("R_output", "full_spectra_veg.csv"), row.names = FALSE)

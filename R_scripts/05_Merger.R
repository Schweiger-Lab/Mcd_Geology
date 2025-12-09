### 5. Merger: Merge with EMIT spec and Remove Veg pixels #

# Setup #
library(dplyr)

# Input/output paths
unit_fp   <- file.path("R_output", "UnitCoord_FB_CHM.csv")
spec_nonveg_fp <- file.path("R_output", "full_spectra_nonveg.csv")
spec_veg_fp    <- file.path("R_output", "full_spectra_veg.csv")

out_full_fp <- file.path("R_output", "UnitCoordSpec.csv")
out_rand_fp <- file.path("R_output", "RandUnitCoord_200.csv")

# Read data
dada <- read.csv(unit_fp, check.names = FALSE)
spec_nonveg <- read.csv(spec_nonveg_fp)
spec_veg    <- read.csv(spec_veg_fp)

# Veg masking
spec_nonveg$VM <- "nonveg"
spec_veg$VM    <- "veg"
spec_all <- bind_rows(spec_nonveg, spec_veg)

# Combine data frames
fulldat <- merge(dada, spec_all, by = c("lon", "lat"))

# Reorder columns (lon, lat, Code, spectra)
fulldat <- fulldat %>% select(Code, lon, lat, everything(), -VM)

# Veg mask logic
fulldat$Code <- ifelse(fulldat$VM == "veg", paste0(fulldat$Code, "_VM"), fulldat$Code)
fulldat <- fulldat %>% select(-VM)

# Remove vegetation
fulldat <- fulldat %>% filter(!grepl("VEG", Code))
print(table(fulldat$Code)) # check

# Save full
write.csv(fulldat, file = out_full_fp, row.names = FALSE)

# Random selection
fd <- fulldat %>% select(-lon, -lat)

threshold <- 200
unit_counts <- table(fd$Code)
valid_units <- names(unit_counts[unit_counts >= threshold])
fd_filtered <- fd[fd$Code %in% valid_units, ]

set.seed(1080)
dati <- fd_filtered %>%
  group_by(Code) %>%
  sample_n(200, replace = FALSE) %>%
  ungroup()

print(table(dati$Code)) # verify

# Save random selection (in this case 200)
write.csv(dati, file = out_rand_fp, row.names = FALSE)

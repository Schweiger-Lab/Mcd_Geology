### 4. Consolidation of Units #

# Setup #
library(dplyr) # for consolidations

# Input/output paths
input_fp  <- file.path("R_output", "UnitCoord.csv")
output_fp <- file.path("R_output", "UnitCoord_FB_CHM.csv")

# Read data
dada <- read.csv(input_fp, check.names = FALSE)

# Consolidation 
# Define mapping of old codes to new consolidated codes
code_map <- list(
  Q    = c("Qay","Qct","Qfo","Qfp","Qfy","Qls","Qm","Qpl","Qps"),
  Tv   = c("Tv1","Tv2"),
  Tbrh = c("Tbrh1","Tbrh2","Tbrh3"),
  Tsb  = c("Ts","Tsb"),
  Tpi  = c("Tsi"),
  Tra  = c("Tra","Tra2"),
  Trae = c("Tram","Trau"),
  Tar  = c("Tar","Taro","Tarv"),
  Ti   = c("Tia","Til","Tiv")
)

# Apply mapping
for (new_val in names(code_map)) {
  old_vals <- code_map[[new_val]]
  dada <- dada %>% mutate(Code = ifelse(Code %in% old_vals, new_val, Code))
}

# Check consolidation
print(table(dada$Code))

# Save #
write.csv(dada, file = output_fp, row.names = FALSE)
### 8: Predictor/PLSDA Mapper #

library(caret)
library(dplyr)

# Input paths 
input_fp <- file.path("R_output", "UnitCoordSpec.csv")
model_fp <- file.path("R_output", "PLSDA_finmods.rds")

# Output path 
out_fp <- file.path("R_output", "PredUnitCoord.csv")

# Read data 
dati <- read.csv(input_fp, check.names = FALSE)

nsims <- 10 # should match the number of models saved
classi <- as.factor(dati$Code)

# Spectral matrix
wvl <- colnames(dati[, -(1:3)]) # exclude ID/coord columns
spec <- dati[, wvl]

# Load model
finmods <- readRDS(model_fp)

# Predicting all pixels 
for (nsim in seq(nsims)){
  print(nsim)
  flush.console()
  set.seed(nsim)
  testing <- spec[seq(1, length(wvl), by = 1)] # same step size as your model
  plsClasses <- predict(finmods[[nsim]], newdata = testing)
}

# Combine predictions with coordinates 
df <- cbind(Pred = plsClasses, dati)

# Keep only prediction + coordinates 
pred_df <- df[, c("Pred", "lon", "lat")]

# Save output 
write.csv(pred_df, out_fp, row.names = FALSE)

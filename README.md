# McDermitt Geological Predictor Using EMIT Imagery 

This repository includes scripts associated with mapping geological predictions from EMIT imagery in the McDermitt Caldera.  

Mapping of final predictions was done using ArcGIS Pro software and the output prediction csv. 

---

## Project Structure

- R_input/ : input data (EMIT tile, shapefile of geologic units from Henry et al.)
- R_output/ : generated outputs (CSVs, Plots, Models)
- R_scripts/ : analysis scripts
  - 01_Converter.R
  - 02_NDVI.R
  - 03_Extractor.R
  - 04_Consolidation.R
  - 05_Merger.R
  - 06_PLSDA_p1.R
  - 07_PLSDA_p2.R
  - 08_Predictor.R



## Workflow

**01_Converter.R**: Converts EMIT NetCDF into CSV. 

**02_NDVI**: Sets an NDVI threshold and marks coordinates as either veg or nonveg.

**03_Extractor**: Extracts geologic unit membership from Henry map for XY coordinates.

**04_Consolidation**: Consolidates Henry geologic codes into consolidated model.

**05_Merger**: Combines EMIT spectra with unit coordinates, applies vegetation masking, and samples data.

**06_PLSDA_p1**: Runs PLSDA simulations to determine optimal number of components using Kappa statistics.

**07_PLSDA_p2**: Builds final PLSDA models, evaluates probabilities, confusion matrices, band importance, and summary statistics.

**08_Predictor**: Applies trained PLSDA models to all pixels for mapping predictions.



## Dependencies

All scripts are written in R. 

Required packages include:

`ncdf4`
`sp`
`raster`
`dplyr`
`caret`
`reshape`
`corrplot`
`agricolae`
`ggplot2`

Install all of above with: 
```r
install.packages(c("ncdf4","sp","raster","dplyr","caret","reshape","corrplot","agricolae","ggplot2"))
```


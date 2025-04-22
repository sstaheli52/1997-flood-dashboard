library(terra)

# Load full raster
nc_file <- "data/E2P_EPs_only.nc"
r <- rast(nc_file)

# Crop to Europe
bbox <- ext(-25, 69, 34, 82)
cropped <- crop(r, bbox)

# Save rds for faster loading in shiny
saveRDS(cropped, file = "data_prep/E2P_EPs_cropped.rds")

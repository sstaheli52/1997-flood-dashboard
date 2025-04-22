library(terra)

# Load the NetCDF file as a SpatRaster where each layer is a day's data
nc_file <- "data/E2P_EPs_only.nc"
r <- rast(nc_file)

# View info
# print(r)
# names(r)

# Calculate the average evap for each day
daily_avg <- global(r, fun = "mean", na.rm = TRUE)

# Add data column
daily_avg$time <- seq(as.Date("1997-07-01"), as.Date("1997-07-31"), by = "day")

# Save to cvs
write.csv(daily_avg, "data_prep/E2P_EPs_timeseries.csv", row.names = FALSE)

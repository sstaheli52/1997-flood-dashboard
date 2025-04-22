library(terra)

# Load MSWEP July 1997 data
precip_file <- "data/MSWEP_1997_July_CZ.nc"
p <- rast(precip_file)

# Get daily spatial sum
precip_mean <- global(p, fun = "mean", na.rm = TRUE)

precip_mean$day <- 1:nrow(precip_mean)

names(precip_mean)[1] <- "precip"

write.csv(precip_mean, "data_prep/MSWEP_CZ_timeseries.csv", row.names = FALSE)

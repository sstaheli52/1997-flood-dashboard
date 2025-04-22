library(sf)
library(readxl)
library(dplyr)

#--- Load and prepare spatial data---

# Read shapefile for European basins
basins <- st_read("data/hydrobasins/hydrobasins_europe.shp")

# fix the invalid geometries
basins_valid <- st_make_valid(basins)

# Merge polygons by MAH_NAME field to get one geometry per basin
basins_dissolved <- basins_valid %>%
  group_by(MAJ_NAME) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  rename(basin_name = MAJ_NAME)


#--- Evaporation Data ---

# Load E2P_EPs sums per basin for July 7 and 18
evap_7 <- read_excel("data/ALL_E2P_EPs_basins_071997.xlsx", sheet = "July 7th") %>%
  select(basin_name = MAJ_NAME, E2P_EPs_1997 = `_sum`) %>%
  mutate(day = "July 7")

evap_18 <- read_excel("data/ALL_E2P_EPs_basins_071997.xlsx", sheet = "July 18th") %>%
  select(basin_name = MAJ_NAME, E2P_EPs_1997 = `_sum`) %>%
  mutate(day = "July 18")

# combine into one table
evap <- bind_rows(evap_7, evap_18)

#--- Join with geometry and compute area ---#

# Project to equal-area projection (EPSG:3035) for accurate area calculation
basins_proj <- st_transform(basins_dissolved, crs = 3035)

# Join evaporation values with geometries
basin_map_data <- left_join(basins_proj, evap, by = "basin_name") %>%
  st_as_sf()

# Calculate basin area km2
basin_map_data$area_km2 <- as.numeric(st_area(basin_map_data)) / 1e6

# Convert absolute E2P_EPs to mm/day by dividing by area
basin_map_data$E2P_EPs_mm <- basin_map_data$E2P_EPs_1997 / basin_map_data$area_km2

# Clean up geometries and reproject to WGS84 for leaflet
basin_map_data <- basin_map_data %>%
  st_make_valid() %>%
  st_transform(crs = 4326)

#--- Differences Data ---#

# Load differences between July 1997 and climatology
diffs <- bind_rows(
  read_excel("data/Dif_14days_31days.xlsx", sheet = "dif_14days") %>%
    select(basin_name = MAJ_NAME, abs_diff = Dif_14days, rel_diff = RD_14days) %>%
    mutate(window = "14-day"),
  read_excel("data/Dif_14days_31days.xlsx", sheet = "dif_31days") %>%
    select(basin_name = MAJ_NAME, abs_diff = Dif_31days, rel_diff = RD_31days) %>%
    mutate(window = "31-day")
)

#--- Exceedance Data ---#

# Load the fraction of basin areas in each exceedance category for both windows
exceed_full <- bind_rows(
  read_excel("data/areal_fractions_basins_exceedance_table.xlsx", sheet = "Areal Fractions 14 days") %>%
    select(basin_name = MAJ_NAME, category, fraction) %>%
    mutate(window = "14-day"),
  read_excel("data/areal_fractions_basins_exceedance_table.xlsx", sheet = "Areal Fractions 31 days") %>%
    select(basin_name = MAJ_NAME, category, fraction) %>%
    mutate(window = "31-day")
)

#--- Correlation Data ---#

# Load rolling correlation results
corr <- read_excel("data/rolling_correlation_summary_table.xlsx") %>%
  select(
    basin_name = basin_name,
    window,
    rho = mean_rho,
    sig_dates = significant_dates,
    nonsig_dates = zero_sig_dates
  )

#--- Save Outputs ---#
# these RDS file is used by app.R
saveRDS(basin_map_data, "data_prep/basin_map_data_evap.rds")
saveRDS(diffs, "data_prep/basin_diff_data.rds")
saveRDS(exceed_full, "data_prep/basin_exceed_data.rds")
saveRDS(corr, "data_prep/basin_corr_data.rds")


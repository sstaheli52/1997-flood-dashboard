# 1997 Czech Flood Dashboard

This Shiny dashboard explores the evaporation sources and precipitation patterns during the 1997 Central European flood, with a focus on extreme precipitation in the Czech Republic.
It combines global evaporation tracking with output data from the FLEXPART model and HAMSTER framework (E2P_EPs variable) and precipitation data (MSWEP) and interactive mapping to provide spatial and temporal insights into this extreme event.

## Features

-**Timeseries Plot**
Visualizes the daily mean evaporation (E2P_EPs) contributing to Czech rainfall alongside MSWEP precipitation in July 1997.

-**Time Lapse Map**
Displays the spatial distribution of E2P_EPs across Europe for each day in July 1997.

-**Basin Map & Info Panel**
Interactive leaflet map showing:
- Total and per-area E2P_EPs by basin (July 7 & 18)
- Precipitation differences between July 1997 and 1980-2020 climatology
- Exceedance probability categories and basin fractions
- Rolling Spearman correlations between evaporation and precipitation

---

## Folder Structure

- `app.R`: Main Shiny dashboard file
- `Flood_project.Rproj`: RStudio project file

### `data/`
Contains input datasets used in preprocessing:
- Excel files: `ALL_E2P_EPs_basins_071997.xlsx`, `Dif_14days_31days.xlsx`, etc.
- Shapefiles in `hydrobasins/` and optionally `dissolved_basins/`
- NetCDF files: `E2P_EPs_only.nc`, `MSWEP_1997_July_CZ.nc` (See note below)

### `data_prep/`
Preprocessing R scripts:
- `preprocess_E2P_EPs.R`
- `preprocess_E2P_EPs_maps.R`
- `preprocess_MSPEW_071997_CZ.R`
- `preprocess_basins_summary.R`

Output files from preprocessing R scripts:
- `.rds` files: `basin_corr_data.rds`, `E2P_EPs_cropped.rds`, etc.
- `.csv` files: `E2P_EPs_timeseries.csv`, `MSWEP_CZ_timeseries.csv`

  
> **Note:** The large NetCDF files `E2P_EPs_only.nc` and `MSWEP_1997_July_CZ.nc` are not included in this repository.  
> Please download them from this shared folder: [📂 Google Drive – Large Data Files](https://drive.google.com/drive/folders/1WrkVHYFcNda3IKggc0PKj9hoP6PBh5Za?usp=sharing)

---

## Usage
To launch the Shiny dashboard locally, simply run:

```r
shiny:runApp("app.R")
```

## Acknowledgments
- Dashboard created as part of the Interactive Web Visualization for Hydroclimatological Data course project (Spring 2025).
- Based on results and data analyzed for my Bachelor's Thesis: Evaluation of the Sources of Rainfall Over Czechia for Major Flood Events.
- For the full thesis repository (data processing, figures, and R scripts), see:  
[Thesis Repository](https://github.com/sstaheli52/Thesis)




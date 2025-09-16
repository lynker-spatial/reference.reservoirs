# --- Config ---
gpkg_path <- "/Users/mikejohnson/hydrofabric/v3.0/reference_fabric.gpkg"
wb_path   <- '/Users/mikejohnson/hydrofabric/CONUS/reference_CONUS.gpkg'
fl_layer  <- "flowpaths"
fl_idcol  <- "flowpath_id"   # HydroFabric flowline ID column

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(data.table)
  library(glue)
  library(purrr)
})

sf::sf_use_s2(FALSE)

# =========================
# Stitch tiles → final
# =========================
tiles_dir  <- "output/tiles_rds/"
tile_files <- list.files(tiles_dir, pattern = "\\.rds$", full.names = TRUE)

res <- data.table::rbindlist(lapply(tile_files, readRDS), fill = TRUE)

if (nrow(res)) {
  # if a dam shows up in multiple tiles (edge effects), keep the one with more context hits → closer snap
  data.table::setorderv(res, c("n","realization_snap_m"), c(-1L, 1L))
  res_final <- res[, .SD[1L], by = .(dam_id, dam.name)]
}

## COMPUTE DISTANCE TO FLOWPATH: 
#################################
# keep those that actually have a ref_fab_fp id
res_fp <- res_final %>%
  filter(!is.na(ref_fab_fp)) |> 
  filter(!is.na(X)) |> 
  mutate(network = TRUE)

res_non_fp <- res_final %>%
  filter(!dam_id %in% res_fp$dam_id) |> 
  mutate(network = FALSE,
         distance_to_fp_m = NA)


# Build dam points (EPSG:5070); keep dam_id for the join later
dam_pts <- st_as_sf(res_fp, coords = c("X", "Y"),crs = 5070)
# coerce flowline ids to a consistent type for SQL
ids <- unique(res_fp$ref_fab_fp)
# If they are character with commas or spaces, normalize:
ids <- ids[!is.na(ids)]
ids_chr <- as.character(ids)

flowlines <- hfutils::as_ogr(gpkg_path, fl_layer) |> 
  filter(flowpath_id %in% ids_chr) |> 
  st_as_sf()

# Reindex flowlines by id for a 1:1 geometry pull
fl_index <- match(dam_sf$ref_fab_fp, fl_sf$flowpath_id)
# Some ids may be missing in the flowline table
has_fl   <- !is.na(fl_index)

# Allocate result vector
dist_m <- rep(NA_real_, nrow(dam_sf))

# Compute distances only where we found the flowline
if (any(has_fl)) {
  dist_m[has_fl] <- as.numeric(st_distance(dam_sf[has_fl, ], fl_sf[fl_index[has_fl], ], by_element = TRUE))
}

# Attach distances back to the stitched result
res_fp$distance_to_fp_m <- dist_m

res <- bind_rows(res_fp, res_non_fp)


## ADD WATERBODY AREA
#################################

ids <- unique(res$ref_fab_wb)
length(ids)
# If they are character with commas or spaces, normalize:
ids <- ids[!is.na(ids)]
ids_chr <- as.character(ids)

wbs <- hfutils::as_ogr(wb_path, "reference_waterbody") |> 
  filter(comid %in% ids_chr) |> 
  st_as_sf() |> 
  select(ref_fab_wb = comid, wb_name = gnis_name) %>%
  mutate(wb_areasqkm = hfutils::add_areasqkm(.)) |>
  st_drop_geometry()

hist(wbs$wb_areasqkm)

res <- left_join(res, wbs, by = "ref_fab_wb", relationship = "many-to-many") 

saveRDS(res, "output/reference-reservoirs.rds")

res_final |> 
  dplyr::filter(!is.na(X) & !is.na(Y)) |>
  sf::st_as_sf(coords = c("X","Y"), crs = 5070, remove = FALSE) |> 
  sf::write_sf("output/reference-reservoirs.gpkg")

# Data Prep

# 1. USACE took down straight GPKG / CSV downloads in favor of a web map service (WMS).
## I used an old archive I have and a version is posted here as well: 
##  https://github.com/bcongdon/nid-data/blob/master/NID2019_U.csv

readr::read_csv('https://raw.githubusercontent.com/bcongdon/nid-data/refs/heads/master/NID2019_U.csv') |> 
  janitor::clean_names() |> 
  select(nidid, dam_name, dam_former_name, other_dam_name, river, state, county, longitude, latitude, dam_length, nid_storage,
         spillway_width, spillway_type, surface_area, structural_height, dam_height, nid_height, dam_type) |> 
  filter(!state %in% c('AK', 'HI', 'PR', 'GU')) |> 
  filter(!is.na(longitude) & !is.na(latitude)) |> 
  filter(latitude !=0, longitude !=0) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |> 
  st_transform(5070) |> 
  saveRDS("data/nid_cleaned_5070.rds")


# 2. Reference Fabric
#   - From lynker-spatial and shared with team
#   - Waterbodies are from v2.2

ref_fab = '/Users/mikejohnson/hydrofabric/v3.0/reference_fabric.gpkg'
wb_ref  = '/Users/mikejohnson/hydrofabric/CONUS/reference_CONUS.gpkg'

# 3. GOODD (now GDW) requires an account to download. Go here: https://docs.google.com/forms/d/e/1FAIpQLSdEo7TGDsjHTxkfmygHi5mfdUA9G2b4v2LhWZfdhJd4LJWUYw/viewform

# 4. ResOps: get the zip file here for everything! 
# https://zenodo.org/records/5893641

# 5. OSM
# 
# Run this to build the dataset: ~90GB
 
library(osmextract)

outdir <- "/Volumes/Transcend/OSM" # <-- SET YOUR DIRECTORY!!
n <- paste0("us/", tolower(state.name))
n = c(n, "us/washington dc")

refresh <- FALSE

for(i in 1:length(n)){
  
  message(n[i])
  
  lines <- oe_get(
    n[i],
    quiet = FALSE,
    force_download = refresh,
    download_directory = outdir,
    query = "SELECT * FROM 'lines' WHERE waterway = 'dam'"
  )
  
  polys <- oe_get(
    n[i],
    quiet = FALSE,
    download_directory = outdir,
    query = "SELECT * FROM 'multipolygons' WHERE natural = 'water'"
  )
}

# 6. Hillari: Data can be downloaded from: 
# https://www.osti.gov/dataexplorer/biblio/dataset/1960141-hydropower-infrastructure-lakes-reservoirs-rivers-hilarri-v2

# 7. NWM
nwm = read_sf(ref_fab, 'hydrolocations') |> 
  filter(hl_type %in% c('reservoir',  'rfc')) |> 
  st_transform(5070) |> 
  group_by(poi_id) |>
  summarize(hl_reference = paste(hl_reference, collapse = ","))

filter(wbs, comid %in% nwm$wbcomid) |> 
  slice_min(area_sqkm) |> 
  mapview::mapview()

wbs = read_sf(wb_ref, "reference_waterbody") |> 
  filter(area_sqkm > 20)

tmp = all.dams
idx = st_nearest_feature(tmp, wbs)
tmp$nearest_dist <- as.numeric(sf::st_distance(tmp, wbs[idx, ], by_element = TRUE))
tmp$wbcomid = as.integer(wbs$comid[idx])
tmp = tmp |> 
  group_by(wbcomid) |> 
  slice_min(nearest_dist) |>
  ungroup() |> 
  filter(nearest_dist < 20000)

tmp2 = nwm
idx2 = st_nearest_feature(tmp2, wbs)
tmp2$wbcomid = as.integer(wbs$comid[idx2])


tmp4 = st_set_geometry(tmp2, st_geometry(tmp)[match(tmp2$wbcomid,tmp$wbcomid)])

fixed = filter(tmp4, wbcomid %in% tmp$wbcomid)
remains = filter(tmp2, poi_id %in% filter(tmp4, !wbcomid %in% tmp$wbcomid)$poi_id)

bind_rows(fixed, remains) |> 
  st_write('data/nwm_corrected.gpkg', 'reservoirs')

# 8. GNIS - I honestly dont remember how I pulled the GNIS dataset! But Ive stashed it in data and the raw data is somewhere here: https://www.usgs.gov/tools/geographic-names-information-system-gnis

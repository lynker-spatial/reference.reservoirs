# =========================
# CONUS grid → per-tile processing → stitch (no parallel)
# =========================

# ---- my utils ----
source("R/utils_fin.R")  # all helpers live here (KNN/radius shortlist, scoring, rank/filter, logging, tiling, etc.)

# --- output plumbing ------------------------------------------------------
tiles_dir   <- "output/tiles_rds"
overwrite   <- FALSE                  # flip to TRUE to recompute an existing tile rds
manifest_fn <- "output/tiles_manifest.csv"
if (!dir.exists("output"))   dir.create("output", recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(tiles_dir))  dir.create(tiles_dir, recursive = TRUE, showWarnings = FALSE)

# =========================
# Config
# =========================
search_dist <- 2500     # default radius for candidate search (m); I use per-context overrides below
tile_km     <- 100      # grid cell size (km). 100km balances I/O and candidate set size pretty well
sample_n    <- NA       # smoke test knob: set e.g. 200 to cap dams per tile; NA = all dams in tile

# OSM *lines* are the slow kid. Leave TRUE only when I really need them.
use_osm_lines <- TRUE

# =========================
# Inputs
# =========================
# NID canonical set → project to 5070, synthesize a stable dam_id (since NIDID is duplicated in spots)
all.dams <-  readRDS("./data/nid_cleaned_5070.rds") |>
  st_transform(5070) |>
  mutate(dam_id = paste0("ls-", seq_len(n()))) |>
  select(dam_id, dam_name, dam_former_name, other_dam_name, river)

stopifnot(st_crs(all.dams)$epsg == 5070)

# CONUS frame
conus <- AOI::aoi_get(state = "conus") |> st_transform(5070)

# handy geometry predicate
is_line <- function(obj) {
  if (is.null(obj) || !nrow(obj)) return(FALSE)
  any(as.character(st_geometry_type(obj, by_geometry = TRUE)) %in% c("LINESTRING","MULTILINESTRING"))
}

# compat wrapper so I can call the grid KNN with either arg style
if (!exists("candidates_for_dams_knn_grid_compat", mode = "function")) {
  candidates_for_dams_knn_grid_compat <- function(...) {
    args <- list(...)
    if (!is.null(args$cellsize_m) && is.null(args$cellsize)) {
      args$cellsize <- args$cellsize_m
      args$cellsize_m <- NULL
    }
    do.call(candidates_for_dams_knn_grid, args)
  }
}

# =========================
# Make tiles and run
# =========================
tiles <- make_conus_grid(st_union(conus), cell_km = tile_km)     # simple equal-sized grid
tiles <- tiles[lengths(st_intersects(tiles, all.dams)) > 0, ]    # only tiles that actually have dams
if (!"tile_id" %in% names(tiles)) tiles$tile_id <- seq_len(nrow(tiles))  # ensure a stable id

# load ancillary once
gnis     <- readRDS("data/gnis_sf.rds")
hillari  <- read.csv("data/HILARRI_v3.csv") |>
  st_as_sf(coords = c('pt_lon', 'pt_lat'), crs = 4326) |>
  st_transform(5070)
resops   <- read.csv("data/ResOpsUS/attributes/reservoir_attributes.csv") |>
  st_as_sf(coords = c('LONG', 'LAT'), crs = 4326) |>
  st_transform(5070)
goodd    <- read_sf('data/GOODD/GDW_v1_0_shp/GDW_v1_0_shp/GDW_barriers_v1_0.shp') |>
  st_transform(5070)
nwm      <- read_sf('data/nwm_corrected.gpkg') |>
  st_transform(5070)
nid      <- readRDS("./data/nid_cleaned_5070.rds") |>
  st_transform(5070)

# --- context ranking ------------------------------------------------------
# Lower rank = higher priority.
# 
rank_map <- c(
  # anchors / derived joins
  ref_int = 0,  osm_int = 0,
  
  # curated / named contexts (secondary)
  gnis = 1,  resops = 1,  osm_dam_lines = 1,  hillari = 1,  goodd = 1,
  
  # direct evidence (primary)
  osm_ww_lines  = 2,  osm_ww_poly   = 2,
  ref_fab_fp    = 2,  ref_fab_wb    = 2,
  nwm           = 2,  nid = 2
)

all_contexts <- names(rank_map)

cli::cli_h2("Dam alignment runner (no parallel)")
cli::cli_text("Tiles: {nrow(tiles)} | tile size: {tile_km} km | default search radius: {search_dist} m")
cli::cli_rule()

pb <- cli::cli_progress_bar("Processing tiles", total = nrow(tiles))

# --- dev/test knobs -------------------------------------------------------
pieces    <- vector("list", length = nrow(tiles))
nn        <- 1:nrow(tiles)
#nn        <- sample(nn, 2)  # dev: run two random tiles; comment this for full run
overwrite <- FALSE           # dev: force re-compute those two tiles

for (ti in nn) {  # full run: for (ti in seq_len(nrow(tiles))) {
  cli::cli_progress_update(id = pb)
  tile <- tiles[ti, , drop = FALSE]
  
  # per-tile outputs
  tile_tag <- sprintf("tile-%05d", tile$tile_id)
  tile_fn  <- file.path(tiles_dir, sprintf("%s.rds", tile_tag))
  
  if (overwrite) unlink(tile_fn)                 # dev: rm existing file
  if (file.exists(tile_fn)) { log_tile(ti, "skipping (exists)"); next }
  
  dams_here <- st_filter(all.dams, tile)         # window dams to this tile
  if (!nrow(dams_here)) { log_tile(ti, "no dams"); next }
  if (!is.na(sample_n)) dams_here <- head(dams_here, sample_n)  # dev: cap sample
  
  log_tile(ti, glue("dams: {nrow(dams_here)}"))
  
  # read + clip state/tile layers once here (helpers handle on-disk gpkg reads & bbox filters)
  layers <- timed(read_state_layers(tile, conus), label = sprintf("[tile %d] read_state_layers", ti))
  aoi    <- if ("aoi" %in% names(layers)) layers$aoi else tile
  
  # per-tile clips for the static layers
  gnis_here    <- gnis[tile, ];       if (!is.null(gnis_here))    gnis_here$ID    <- gnis_here$FEATURE_ID
  resops_here  <- resops[tile, ];     if (!is.null(resops_here))  resops_here$ID  <- resops_here$DAM_ID
  hillari_here <- hillari[tile, ];    if (!is.null(hillari_here)) hillari_here$ID <- hillari_here$hilarriid
  goodd_here   <- goodd[tile, ];      if (!is.null(goodd_here))   goodd_here$ID   <- goodd_here$GDW_ID
  nwm_here     <- nwm[tile, ];        if (!is.null(nwm_here))     nwm_here$ID     <- nwm_here$hl_reference
  nid_here     <- nid[tile, ];        if (!is.null(nid_here))     nid_here$ID     <- nid_here$nidid
  
  fl           <- layers$fl[tile,]
  wb           <- layers$wb[tile,]
  
  osm_ww_poly   <- layers$osm_ww_poly[tile,]
  osm_ww_lines  <- if (isTRUE(use_osm_lines)) layers$osm_ww_lines[tile,]  else NULL
  osm_dam_lines <- if (isTRUE(use_osm_lines)) layers$osm_dam_lines[tile,] else NULL
  
  
  # mapview::mapview(dams_here, color = "red", layer.name = "dams") +
  #   mapview::mapview(fl, color = "blue", layer.name = "flowlines") +
  #   mapview::mapview(wb, color = "cyan", layer.name = "waterbodies") +
  #   mapview::mapview(osm_ww_poly, color = "green", layer.name = "osm_poly") +
  #   mapview::mapview(osm_ww_lines, color = "orange", layer.name = "osm_lines") +
  #   mapview::mapview(osm_dam_lines, color = "purple", layer.name = "osm_dam_lines") +
  #   mapview::mapview(gnis_here, color = "pink", layer.name = "gnis") +
  #   mapview::mapview(goodd_here, color = "gray", layer.name = "goodd") +
  #   mapview::mapview(nid_here, color = "white", layer.name = "nid")
  
  # intersections are strong signals; I compute them once per tile
  dam.names <- c('dam_name','dam_former_name','other_dam_name','river')
  obj.names <- c('name','alt_name','FEATURE_NAME','gnis_name')
  
  ref_int <- timed(
    wb_fl_intersect_fast(wb, fl, dams_here, obj.names, dam.names, type = "ref_fab"),
    label = sprintf("[tile %d] ref intersections", ti)
  )
  osm_int <- timed(
    wb_fl_intersect_fast(osm_ww_poly, osm_ww_lines, dams_here, obj.names, dam.names, type = "osm"),
    label = sprintf("[tile %d] osm intersections", ti)
  )
  
  # assemble context list (include what I care about on this pass)
  contexts <- list(
    osm_ww_poly   = osm_ww_poly,
    # osm_ww_lines = osm_ww_lines,       # enable only when needed (slow)
    osm_dam_lines = osm_dam_lines,
    hillari       = hillari_here,
    nwm           = nwm_here,
    gnis          = gnis_here,
    resops        = resops_here,
    goodd         = goodd_here,
    ref_fab_fp    = fl,
    ref_fab_wb    = wb,
    ref_int       = ref_int,
    osm_int       = osm_int,
    nid           = nid_here            # kept for traceability/fallback; never the first choice
  )
  
  # per-context search radii (tight on dense lines, looser on noisy points)
  search_dist_map <- list(
    ref_fab_fp    = 750,
    osm_ww_lines  = 750,
    osm_ww_poly   = 750,
    osm_dam_lines = 1500,
    ref_fab_wb    = 2000,
    
    nwm    = 2000,
    gnis   = 2000,
    hillari= 2000,
    goodd  = 2000,
    resops = 2000,
    
    ref_int = 2000,
    osm_int = 2000,
    nid     = 2000
  )
  
  # ---------- score each context ----------
  scored_ctx <- lapply(names(contexts), function(ctx) {
    obj <- contexts[[ctx]]
    if (is.null(obj) || !nrow(obj)) { log_ctx(ti, ctx, "empty"); return(NULL) }
    
    log_ctx(ti, ctx, glue("start (objects={nrow(obj)})"))
    
    # name prep (lowercased / stripped / numbers normalized when desired)
    dam_name_vec <- prep_names_once(st_drop_geometry(dams_here),
                                    setdiff(dam.names, "river"),
                                    use_qdap_numbers = FALSE)
    obj_name_vec <- prep_names_once(st_drop_geometry(obj),
                                    c("name","alt_name","FEATURE_NAME","gnis_name","water"),
                                    use_qdap_numbers = FALSE)
    
    # candidate shortlist: exact radius via GEOS index, with KNN fallback gated by same radius
    dist_here <- search_dist_map[[ctx]]
    cand_idx <- timed(
      candidates_for_dams_precise(
        dams            = dams_here,
        obj             = obj,
        max_search_dist = dist_here,
        line_mode       = is_line(obj),
        knn_fallback_K  = 10
      ),
      label = sprintf("[tile %d][%s] candidates (precise) - search=%d", ti, ctx, dist_here)
    )
    if (is.null(cand_idx)) { log_ctx(ti, ctx, "candidates → 0 pairs"); return(NULL) }
    
    # exact snap + similarity for shortlisted pairs
    dt <- timed(
      score_pairs_fast(
        dams          = dams_here,
        obj           = obj,
        dam_idx       = cand_idx$dam_idx,
        obj_idx       = cand_idx$obj_idx,
        dam_name_vec  = dam_name_vec,
        obj_name_vec  = obj_name_vec,
        dam.id        = "dam_id",     # keep synthetic dam_id all the way through
        obj.id        = "ID",
        context       = ctx,
        compute_similarity = TRUE
      ),
      label = sprintf("[tile %d][%s] score", ti, ctx)
    )
    if (is.null(dt) || !nrow(dt)) { log_ctx(ti, ctx, "scored → 0"); return(NULL) }
    
    # gate (jw, snap) + pick one per dam/context
    dt <- timed(
      rank_and_filter(
        dt,
        similarity_keep = 0.75,       # I may bump this per context later
        snap_gate       = 300,        # keep snaps within 300 m
        dam_key         = if ("dam.name" %in% names(dt)) "dam.name" else "dam_row"
      ),
      label = sprintf("[tile %d][%s] rank/filter", ti, ctx)
    )
    
    if (is.null(dt) || !nrow(dt)) { log_ctx(ti, ctx, "filtered → 0"); return(NULL) }
    
    # base rank + tributary deprioritization (push TR/OS/trib downstream)
    dt[, rank := unname(rank_map[[ctx]])]
    if ("on_trib" %in% names(dt)) dt[on_trib == TRUE, rank := rank + 5L]
    
    # quick roll-up for logs
    med_snap <- suppressWarnings(median(dt$snap_dist, na.rm = TRUE))
    med_jw   <- if ("jw" %in% names(dt)) suppressWarnings(median(dt$jw, na.rm = TRUE)) else NA_real_
    log_ctx(ti, ctx, glue("kept {nrow(dt)} (med snap={round(med_snap,1)} m; med jw={ifelse(is.na(med_jw),'NA',round(med_jw,3))})"))
    
    dt
  })
  
  
  # apply to all before binding:
  scored_ctx <- lapply(seq_along(scored_ctx), function(i) sanitize_ctx(scored_ctx[[i]], names(scored_ctx)[i]))
  scored <- data.table::rbindlist(Filter(Negate(is.null), scored_ctx), fill = TRUE)
  
  scored <- rbindlist(Filter(Negate(is.null), scored_ctx), fill = TRUE)
  if (!nrow(scored)) next
  
  # normalize contexts to my rank_map universe
  sc <- copy(scored)[context %in% all_contexts]
  sc[, context  := factor(context, levels = all_contexts)]
  sc[, jw_order := fifelse(is.na(jw), 1, jw)]  # missing jw = worst case for tie-break
  
  # one row per (dam, context) by rank → distance → jw
  sc_bestctx <- sc[order(rank, snap_dist, jw_order), .SD[1L], by = .(dam_id, dam.name, context)]
  
  # wide table: one column per context, value = obj_id
  wide_ids <- dcast(
    sc_bestctx,
    dam_id ~ context,
    value.var     = "obj_id",
    fun.aggregate = function(x) x[1],
    fill          = NA,
    drop          = FALSE
  )
  setcolorder(wide_ids, c("dam_id", all_contexts))
  
  # n = how many contexts hit this dam
  n_counts <- sc_bestctx[!is.na(obj_id), .(n = uniqueN(context)), by = .(dam_id, dam.name)]
  
  # best realization globally (respect rank, then snap, then jw)
  best <- sc_bestctx[order(rank, snap_dist, jw_order), .SD[1L], by = .(dam_id, dam.name)]
  best_keep <- best[, .(
    dam_id,
    realization              = as.character(context),
    realization_id           = obj_id,
    realization_snap_m       = snap_dist,
    realization_jw           = jw,
    realization_obj_name     = obj.name,
    X, Y
  )]
  
  # distance from original NID geometry (simple QA)
  nid_xy <- st_coordinates(st_geometry(dams_here))
  nid_lookup <- data.table(dam_id = dams_here$dam_id, nid_X = nid_xy[,1], nid_Y = nid_xy[,2])
  best_keep <- merge(best_keep, nid_lookup, by = "dam_id", all.x = TRUE)
  best_keep[, nid_offset_m := ifelse(is.na(X)|is.na(Y)|is.na(nid_X)|is.na(nid_Y),
                                     NA_real_, sqrt((X - nid_X)^2 + (Y - nid_Y)^2))]
  best_keep[, nid_flag_large_offset_500m := !is.na(nid_offset_m) & nid_offset_m > 500]
  
  # stitch per-tile output
  out <- merge(wide_ids, n_counts,  by = c("dam_id"), all.x = TRUE)
  out <- merge(out,     best_keep,  by = c("dam_id"), all.x = TRUE)
  out[is.na(n), n := 0L]
  out[, tile_id := tiles$tile_id[ti]]
  
  saveRDS(out, tile_fn)
  log_ctx(ti, "save", glue("wrote {basename(tile_fn)} ({nrow(out)} dams)"))
  
  # tiny manifest row for progress tracking
  data.table::fwrite(
    data.table(
      tile_id   = tile$tile_id,
      file      = basename(tile_fn),
      n_dams    = uniqueN(out$dam_id),
      n_rows    = nrow(out),
      median_n  = round(stats::median(out$n, na.rm = TRUE), 2),
      ts        = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    manifest_fn, append = file.exists(manifest_fn)
  )
}
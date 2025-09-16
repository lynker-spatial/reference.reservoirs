suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(data.table)
  library(glue)
  library(lwgeom)
  library(AOI)
  library(cli)
})

sf::sf_use_s2(FALSE)   # keep planar ops predictable (EPSG:5070, meters)


make_conus_grid <- function(conus_poly, cell_km = 150) {
  cell <- cell_km * 1000
  base_grid <- sf::st_make_grid(conus_poly, cellsize = cell, what = "polygons", square = TRUE)
  tiles <- sf::st_as_sf(base_grid) |>
    dplyr::mutate(tile_id = seq_len(dplyr::n()))
  tiles
}

read_state_layers <- function(tile,
                              conus,
                              paths = list(
                                ref_fabric = '/Users/mikejohnson/hydrofabric/v3.0/reference_fabric.gpkg',
                                ref_conus  = '/Users/mikejohnson/hydrofabric/CONUS/reference_CONUS.gpkg',
                                osm_root   = '/Volumes/Transcend/OSM'
                              )) {
  
  # state polygon AOI
  states_touched = conus[tile,]
  
  st_name <- gsub(" ", "-", tolower(states_touched$state_name))
  
  aoi <- tile
  aoi_bbox_4326 = st_bbox(st_transform(aoi, 4326))
  aoi_wkt <- sf::st_as_text(AOI::bbox_get(st_bbox(aoi))$geom)
  
  # reference flowpaths + names
  fl <- read_sf(paths$ref_fabric, layer = "flowpaths", wkt_filter = aoi_wkt) %>%
    left_join(nhdplusTools::get_vaa("gnis_name"), by = c("flowpath_id" = "comid")) %>%
    select(ID = flowpath_id, gnis_name) %>%
    st_transform(5070)
  
  # reference waterbodies
  wb <- read_sf(paths$ref_conus, layer = "reference_waterbody", wkt_filter = aoi_wkt) %>%
    select(ID = comid, gnis_name) %>%
    st_transform(5070)
  
  # OSM (read once per state)
  
  osm_gpkg <- file.path(paths$osm_root, glue::glue("geofabrik_{st_name}-latest.gpkg"))
  
  osm_lines <- osm_poly <-  list()
  
  for(i in 1:length(osm_gpkg)){
    message(osm_gpkg[i])
    osm_lines[[i]] <- st_read(
      osm_gpkg[i],
      query = glue::glue("
        SELECT geometry, osm_id AS ID, name, waterway
        FROM lines
        WHERE waterway IN ('river','stream','dam')
          AND ST_Intersects(
                geometry,
                BuildMbr({aoi_bbox_4326['xmin']}, {aoi_bbox_4326['ymin']}, {aoi_bbox_4326['xmax']}, {aoi_bbox_4326['ymax']})
              )
      "),
      quiet = TRUE
    ) |> 
      st_transform(5070)
    
    # water polygons
    osm_poly[[i]] <- osm_ww_poly <- st_read(
      osm_gpkg[i],
      query = glue::glue("
        SELECT geometry, osm_way_id AS ID, osm_id AS ID2, natural, other_tags, name
        FROM multipolygons
        WHERE natural IN ('water')
          AND ST_Intersects(
                geometry,
                BuildMbr({aoi_bbox_4326['xmin']}, {aoi_bbox_4326['ymin']}, {aoi_bbox_4326['xmax']}, {aoi_bbox_4326['ymax']})
              )
      "),
      quiet = TRUE
    ) |> 
      extract_osm_tags() %>%
      mutate(ID = ifelse(is.na(ID), ID2, ID), ID2 = NULL, other_tags = NULL) |> 
      st_transform(5070)
  }
  
  osm_lines <- do.call(rbind, osm_lines)
  osm_poly  <- do.call(rbind, osm_poly)
  osm_ww_lines  <- filter(osm_lines, waterway != "dam")
  osm_dam_lines <- filter(osm_lines, waterway == "dam")
  
  
  list(
    aoi=aoi, fl=fl, wb=wb,
    osm_ww_lines=osm_ww_lines,
    osm_dam_lines=osm_dam_lines,
    osm_ww_poly=osm_poly
  )
}

# Fast parse of common other_tags into columns (no pivot_wider)
extract_osm_tags <- function(df) {
  if (!"other_tags" %in% names(df)) return(df)
  ot <- df$other_tags
  df$gnis_feature_id <- stringr::str_match(ot, '"gnis:feature_id"=>"([^"]+)"')[,2]
  df$water           <- stringr::str_match(ot, '"water"=>"([^"]+)"')[,2]
  df
}

wb_fl_intersect_fast <- function(wb,
                                 fl,
                                 dams,
                                 obj.names,
                                 dam.names,
                                 type = "ref_fab",
                                 dist_limit = Inf,   # e.g., 1000 (meters) if you want a cap
                                 add_similarity = TRUE,
                                 use_qdap_numbers = FALSE) {
  if (is.null(wb) || is.null(fl) || nrow(wb) == 0L || nrow(fl) == 0L) return(NULL)
  
  wb  <- sf::st_transform(wb,  5070)
  fl  <- sf::st_transform(fl,  5070)
  dams<- sf::st_transform(dams,5070)
  
  # Cast WB to boundaries and intersect once (state-scale)
  ls_wb <- suppressWarnings(sf::st_cast(wb, "MULTILINESTRING"))
  ls_wb <- lwgeom::st_snap_to_grid(ls_wb, size = 1)
  
  xx <- suppressWarnings(sf::st_intersection(ls_wb, fl))
  if (nrow(xx) == 0L) return(NULL)
  
  # Ensure expected name columns exist (so coalesce works)
  for (nm in obj.names) if (!nm %in% names(xx)) xx[[nm]] <- NA_character_
  
  # 1) Pick nearest intersection feature per dam (vectorized)
  nearest_idx <- sf::st_nearest_feature(dams, xx)
  sel <- xx[nearest_idx, , drop = FALSE]
  
  # 2) Compute dam→selection distances (vectorized, by-element)
  dists <- as.numeric(sf::st_distance(sf::st_geometry(dams), sf::st_geometry(sel), by_element = TRUE))
  
  # 3) Optionally drop far-away intersections
  keep <- is.finite(dists) & (dists <= dist_limit)
  # If you want to keep all regardless of distance, set dist_limit = Inf
  if (!any(keep)) {
    # Return empty sf with expected cols
    out <- sel[FALSE, , drop = FALSE]
    out$dam.name <- character(0)
    out$snap_dist <- numeric(0)
    out$context <- character(0)
    out$ID <- character(0)
    return(out)
  }
  sel <- sel[keep, , drop = FALSE]
  dams_keep <- dams[keep, , drop = FALSE]
  dists <- dists[keep]
  
  # 4) Build combined ID like your original paste (ID.1 from wb boundary, ID from fl)
  id1 <- if ("ID.1" %in% names(sel)) sel[["ID.1"]] else if ("ID" %in% names(sel)) sel[["ID"]] else seq_len(nrow(sel))
  id2 <- if ("ID"   %in% names(sel)) sel[["ID"]]   else seq_len(nrow(sel))
  combo_id <- paste0(id1, "-", id2)
  
  # 5) Coalesce an object name from intersection attributes
  normalize_str <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x <- gsub("\\s*\\([^\\)]+\\)", "", x)
    x[x %in% c("", "unknown", "na", "n/a")] <- NA_character_
    x
  }
  coalesce_many <- function(df, cols) {
    has <- intersect(cols, names(df))
    if (!length(has)) return(rep(NA_character_, nrow(df)))
    mat <- lapply(df[has], normalize_str)
    out <- do.call(coalesce, c(mat, list(NA_character_)))
    as.character(out)
  }
  obj_name <- coalesce_many(sf::st_drop_geometry(sel), obj.names)
  
  # 6) Optional name similarity to dam names (one dam vs its chosen intersection)
  jw <- rep(NA_real_, nrow(sel))
  if (add_similarity) {
    dam_nm_all <- coalesce_many(sf::st_drop_geometry(dams_keep), setdiff(dam.names, "river"))
    mtd <- "jw"  # you can switch to cosine here if you prefer for short labels
    jw <- ifelse(is.na(dam_nm_all) | is.na(obj_name),
                 NA_real_,
                 stringdist::stringdist(dam_nm_all, obj_name, method = mtd))
  }
  
  # 7) Build output (sf) with one row per kept dam
  out <- sel
  out$dam.name   <- dams_keep$dam.name %||% dams_keep[[1]]
  out$snap_dist <- dists
  out$context   <- type
  out$ID        <- combo_id
  out$obj.name  <- obj_name
  out$jw        <- jw
  
  # Return *many rows* (one per dam that has an intersection)
  out
}

prep_names_once <- function(df, cols, use_qdap_numbers = FALSE) {
  has <- intersect(cols, names(df))
  if (!length(has)) return(rep(NA_character_, nrow(df)))
  out <- rep(NA_character_, nrow(df))
  for (nm in has) {
    v <- tolower(trimws(as.character(df[[nm]])))
    v <- gsub("\\s*\\([^\\)]+\\)", "", v)         # drop parentheticals
    if (use_qdap_numbers && requireNamespace("qdap", quietly = TRUE)) {
      v <- qdap::replace_number(v)
    }
    v[v %in% c("", "unknown", "na", "n/a")] <- NA_character_
    out <- dplyr::coalesce(out, v)
  }
  out
}

# ---
# Minimal helpers
# ---

# robust, minimal rep-points:
# - lines  -> endpoints (fast & good enough for shortlist)
# - polys  -> point-on-surface
# - points -> as-is
rep_points_simple <- function(obj, line_mode = FALSE) {
  g   <- sf::st_geometry(obj)
  crs <- sf::st_crs(g)
  
  if (line_mode) {
    # cast to LINESTRING parts; then take endpoints
    ls <- suppressWarnings(sf::st_cast(g, "LINESTRING", warn = FALSE))
    if (length(ls) == 0) {
      pts <- sf::st_point_on_surface(g)
      return(list(points = pts, map = seq_len(nrow(obj))))
    }
    # endpoints; if lwgeom not available, fall back to startpoints
    if (requireNamespace("lwgeom", quietly = TRUE)) {
      reps <- lwgeom::st_endpoint(ls)
    } else {
      reps <- sf::st_startpoint(ls)
    }
    # map each endpoint back to the original row (by nearest feature)
    # (fast & simple: match endpoints to their parent lines’ index)
    # Build index vector the length of 'reps'
    part_counts <- lengths(sf::st_cast(g, "MULTILINESTRING", warn = FALSE))
    # If counts are zero or weird, just recycle row index
    if (length(part_counts) == 0 || sum(part_counts) == 0) {
      return(list(points = sf::st_point_on_surface(g), map = seq_len(nrow(obj))))
    }
    # Safe map: repeat each row index by number of parts; trim/extend to length(reps)
    map <- inverse.rle(list(lengths = as.integer(pmax(1, part_counts)),
                            values  = seq_len(nrow(obj))))
    map <- map[seq_len(min(length(map), length(reps)))]
    return(list(points = sf::st_cast(reps, "POINT", warn = FALSE), map = map))
  }
  
  # non-line: one point per feature
  pts <- sf::st_point_on_surface(g)
  list(points = pts, map = seq_len(nrow(obj)))
}

# quick coordinate extractor with finite mask
coords_xy <- function(sfc) {
  m <- suppressWarnings(sf::st_coordinates(sfc))
  if (!is.matrix(m) || nrow(m) == 0) return(list(xy = matrix(numeric(0), ncol = 2), keep = logical(0)))
  keep <- is.finite(m[,1]) & is.finite(m[,2])
  list(xy = cbind(m[keep,1], m[keep,2]), keep = keep)
}

# ---
# Simplified grid + KNN shortlist
# ---
candidates_for_dams_knn_grid <- function(dams, obj,
                                         K = 8,
                                         max_search_dist = 3000,
                                         cellsize_m = 100000,
                                         margin_m  = 3000,
                                         line_mode = FALSE) {
  # basic guards
  if (is.null(dams) || is.null(obj) || !nrow(dams) || !nrow(obj)) return(NULL)
  stopifnot(sf::st_crs(dams) == sf::st_crs(obj))  # both already 5070 ideally
  sf::sf_use_s2(FALSE)
  
  # drop empties
  dams <- dams[!sf::st_is_empty(dams), , drop = FALSE]
  obj  <-  obj[!sf::st_is_empty(obj),  , drop = FALSE]
  if (!nrow(dams) || !nrow(obj)) return(NULL)
  
  # representative points for objects (lines → endpoints)
  rp <- rep_points_simple(obj, line_mode = line_mode)
  op_all <- rp$points
  obj_map <- rp$map
  
  # coordinates (finite only)
  dpc <- coords_xy(sf::st_geometry(dams))
  opc <- coords_xy(op_all)
  if (!length(dpc$keep) || !length(opc$keep)) return(NULL)
  
  dams   <- dams[dpc$keep, , drop = FALSE]
  op_all <- op_all[opc$keep]
  obj_map <- obj_map[opc$keep]
  dp_xy  <- dpc$xy
  op_xy  <- opc$xy
  
  if (!nrow(dp_xy) || !nrow(op_xy)) return(NULL)
  
  # make a coarse grid over dams bbox
  hull <- sf::st_as_sfc(sf::st_bbox(sf::st_geometry(dams)))
  grid <- sf::st_make_grid(hull, cellsize = cellsize_m, what = "polygons", square = TRUE)
  tiles <- sf::st_as_sf(grid)
  
  dam_idx_all <- integer(0)
  obj_idx_all <- integer(0)
  
  for (i in seq_len(nrow(tiles))) {
    tile <- tiles[i, , drop = FALSE]
    # dams inside tile
    dam_in <- lengths(sf::st_intersects(dams, tile)) > 0
    if (!any(dam_in, na.rm = TRUE)) next
    
    # objects inside tile buffer (to catch edge cases)
    tileb <- sf::st_buffer(sf::st_geometry(tile), margin_m)
    obj_in <- lengths(sf::st_intersects(op_all, tileb)) > 0
    if (!any(obj_in, na.rm = TRUE)) next
    
    dp <- dp_xy[dam_in, , drop = FALSE]
    op <- op_xy[obj_in, , drop = FALSE]
    if (!nrow(dp) || !nrow(op)) next
    
    k_eff <- min(K, nrow(op))
    if (k_eff < 1) next
    
    # KNN (RANN)
    nn <- RANN::nn2(data = op, query = dp, k = k_eff, searchtype = "standard")
    idx <- as.vector(nn$nn.idx)
    dst <- as.vector(nn$nn.dists)
    
    # gate by distance
    keep <- is.finite(dst) & (dst <= max_search_dist)
    if (!any(keep)) next
    
    # map local indices back to global row ids
    dams_ids <- which(dam_in)
    objs_ids <- which(obj_in)
    
    dam_loc  <- rep(dams_ids, each = k_eff)[keep]
    obj_rep  <- objs_ids[idx[keep]]        # indices into rep points
    obj_loc  <- obj_map[obj_rep]           # back to original obj rows
    
    # dedupe pairs within tile
    ded <- !duplicated(data.frame(d = dam_loc, o = obj_loc))
    dam_idx_all <- c(dam_idx_all, dam_loc[ded])
    obj_idx_all <- c(obj_idx_all, obj_loc[ded])
  }
  
  if (!length(dam_idx_all)) return(NULL)
  # dedupe across tiles
  keep2 <- !duplicated(data.frame(d = dam_idx_all, o = obj_idx_all))
  list(dam_idx = dam_idx_all[keep2], obj_idx = obj_idx_all[keep2])
}

score_pairs_fast <- function(dams, obj, dam_idx, obj_idx,
                             dam_name_vec, obj_name_vec,
                             dam.id, obj.id,
                             context,
                             compute_similarity = TRUE,
                             cosine_contexts = c("osm_ww_lines","osm_dam_lines","gnis")) {
  
  dam_sel <- dams[dam_idx, ]
  obj_sel <- obj[obj_idx, ]
  
  # Accurate snap (compute only on shortlisted K pairs)
  seg <- sf::st_nearest_points(sf::st_geometry(dam_sel), sf::st_geometry(obj_sel))
  snap_dist <- as.numeric(sf::st_length(seg))
  coords <- sf::st_coordinates(seg)
  # last point of each segment is the snap location
  # coords come concatenated; extract ends by grouping
  ends_idx <- cumsum(vapply(split(seq_len(nrow(coords)), coords[, "L1"]), length, 1L))
  XY <- coords[ends_idx, c("X","Y")] |> data.frame()
  
  # Names (already coalesced outside)
  dnm <- dam_name_vec[dam_idx]
  onm <- obj_name_vec[obj_idx]
  
  jw <- rep(NA_real_, length(dnm))
  if (compute_similarity) {
    method <- if (context %in% cosine_contexts) "cosine" else "jw"
    ok <- !is.na(dnm) & !is.na(onm)
    jw[ok] <- stringdist::stringdist(dnm[ok], onm[ok], method = method)
  }
  
  # polygon preference for ties
  gt <- as.character(sf::st_geometry_type(obj_sel, by_geometry = TRUE))
  area_rank <- ifelse(gt %in% c("POLYGON","MULTIPOLYGON"),
                      -as.numeric(sf::st_area(obj_sel)), 0)
  
  data.table::data.table(
    dam_id = sf::st_drop_geometry(dam_sel)[[dam.id]],
    dam_row = dam_idx,
    obj_row = obj_idx,
    context = context,
    obj_id  = sf::st_drop_geometry(obj_sel)[[obj.id]],
    dam.name = dnm,
    obj.name = onm,
    jw = jw,
    snap_dist = snap_dist,
    X = XY$X,
    Y = XY$Y,
    area_rank = area_rank,
    on_trib = grepl("TR|OS|TRIB", (sf::st_drop_geometry(dams)$river %||% "")[dam_idx], ignore.case = TRUE)
  )
}

rank_and_filter <- function(dt,
                            similarity_keep = 0.75,
                            snap_gate = 300,
                            dam_key = NULL) {
  if (is.null(dt) || !nrow(dt)) return(NULL)
  if (is.null(dam_key)) dam_key <- if ("dam.name" %in% names(dt)) "dam.name" else "dam_row"
  
  # similarity_keep is a "keep if >= X" on (1 - jw)
  if ("jw" %in% names(dt)) {
    dt[, sim_score := 1 - jw]
    dt <- dt[is.na(sim_score) | sim_score >= similarity_keep]
  }
  
  # distance gate
  if ("snap_dist" %in% names(dt)) {
    dt <- dt[is.na(snap_dist) | snap_dist <= snap_gate]
  }
  
  if (!nrow(dt)) return(NULL)
  
  # pick best per dam+context using (snap_dist, -area_rank, sim_score desc)
  ord_cols <- c("snap_dist", "area_rank")
  ord_dir  <- c(1, 1)  # area_rank already negative for bigger first
  if ("sim_score" %in% names(dt)) {
    ord_cols <- c(ord_cols, "sim_score")
    ord_dir  <- c(ord_dir, -1)
  }
  
  setorderv(dt, ord_cols, ord_dir)
  # one row per dam+context
  key_cols <- c(dam_key, "context")
  dt <- dt[, .SD[1L], by = key_cols]
  
  dt[]
}

candidates_for_dams <- function(dams, obj, search_dist = 2500) {
  if (is.null(obj) || nrow(obj) == 0) return(NULL)
  idx_list <- st_is_within_distance(st_geometry(dams), st_geometry(obj), dist = search_dist)
  if (all(lengths(idx_list) == 0)) return(NULL)
  dam_idx <- rep(seq_along(idx_list), lengths(idx_list))
  obj_idx <- unlist(idx_list, use.names = FALSE)
  
  list(dam_idx = dam_idx, obj_idx = obj_idx)
}


# =========================
# Logging helpers (cli)
# =========================
log_tile <- function(ti, msg, ...) {
  cli::cli_text("[tile {ti}] {sprintf(msg, ...)}")
}
log_ctx <- function(ti, ctx, msg, ...) {
  cli::cli_text(cli::col_blue(sprintf("[tile %d][%s] %s", ti, ctx, sprintf(msg, ...))))
}
log_warn <- function(msg, ...) cli::cli_alert_warning(sprintf(msg, ...))
log_done <- function(msg, ...) cli::cli_alert_success(sprintf(msg, ...))

# tiny timer wrapper
timed <- function(expr, label = NULL) {
  t0 <- proc.time()[["elapsed"]]
  val <- force(expr)
  t1 <- proc.time()[["elapsed"]]
  if (!is.null(label)) cli::cli_text("  {label} ({round(t1 - t0, 2)}s)")
  val
}

# Robust representative points (+ parent row map)
# Robust representative points for candidate search
# - line_mode = TRUE → use line endpoints (2 pts) for coverage
rep_points_strict <- function(obj, line_mode = FALSE) {
  g   <- sf::st_geometry(obj)
  crs <- sf::st_crs(g)
  
  pts <- vector("list", nrow(obj))
  map <- vector("list", nrow(obj))
  
  # helper: normalize any object (sfg/sfc/list) to a list of sfgs
  iter_sfg <- function(x) {
    if (inherits(x, "sfg")) {
      list(x)
    } else if (inherits(x, "sfc")) {
      lapply(seq_along(x), function(i) x[[i]])
    } else if (is.list(x)) {
      unlist(lapply(x, iter_sfg), recursive = FALSE)
    } else {
      list()
    }
  }
  
  # helper: safe endpoints as sfg POINTs (works w/ or w/o lwgeom)
  line_endpoints_sfg <- function(ls_sfg, crs) {
    stopifnot(inherits(ls_sfg, "sfg"))
    # wrap as sfc for functions that require sfc
    sfc_line <- sf::st_sfc(ls_sfg, crs = crs)
    out <- list()
    
    if (requireNamespace("lwgeom", quietly = TRUE)) {
      # lwgeom returns sfc; extract sfg with [[1]]
      sp <- lwgeom::st_startpoint(sfc_line)[[1]]
      ep <- lwgeom::st_endpoint(sfc_line)[[1]]
      out <- list(sp, ep)
    } else {
      # fallback using coordinates (first/last vertex)
      cc <- sf::st_coordinates(sfc_line)
      if (nrow(cc) >= 1) {
        sp <- sf::st_point(cc[1, 1:2])
        ep <- sf::st_point(cc[nrow(cc), 1:2])
        out <- list(sp, ep)
      }
    }
    out
  }
  
  for (i in seq_len(nrow(obj))) {
    gi <- g[i][[1]]
    if (is.null(gi) || sf::st_is_empty(gi)) next
    
    base_type <- as.character(sf::st_geometry_type(gi))
    
    # explode multi/collections into simpler pieces
    parts <- switch(
      base_type,
      "GEOMETRYCOLLECTION" = try(suppressWarnings(sf::st_collection_extract(gi)), silent = TRUE),
      "MULTILINESTRING"    = try(suppressWarnings(sf::st_cast(gi, "LINESTRING")),  silent = TRUE),
      "MULTIPOLYGON"       = try(suppressWarnings(sf::st_cast(gi, "POLYGON")),     silent = TRUE),
      "MULTIPOINT"         = try(suppressWarnings(sf::st_cast(gi, "POINT")),       silent = TRUE),
      gi
    )
    if (inherits(parts, "try-error") || sf::st_is_empty(parts)) next
    
    sfgs <- iter_sfg(parts)
    if (!length(sfgs)) next
    
    out_i <- list()
    for (gj in sfgs) {
      if (!inherits(gj, "sfg")) next
      t1 <- as.character(sf::st_geometry_type(gj))
      
      if (t1 == "POINT") {
        # already an sfg POINT
        out_i <- c(out_i, list(gj))
        
      } else if (t1 == "LINESTRING") {
        if (isTRUE(line_mode)) {
          # two endpoints as sfg POINTs
          ep <- line_endpoints_sfg(gj, crs)
          if (length(ep)) out_i <- c(out_i, ep)
        } else {
          # representative point on the line: midpoint sample → sfg
          sfc_line <- sf::st_sfc(gj, crs = crs)
          mid <- try(sf::st_line_sample(sfc_line, sample = 0.5), silent = TRUE)
          if (!inherits(mid, "try-error") && length(mid)) {
            out_i <- c(out_i, list(mid[[1]]))
          } else {
            # fallback to startpoint
            ep <- line_endpoints_sfg(gj, crs)
            if (length(ep)) out_i <- c(out_i, list(ep[[1]]))
          }
        }
        
      } else if (t1 == "POLYGON") {
        # guaranteed inside polygon
        out_i <- c(out_i, list(sf::st_point_on_surface(gj)))
        
      } else {
        # any remaining types → safe point
        out_i <- c(out_i, list(sf::st_point_on_surface(gj)))
      }
    }
    
    if (length(out_i)) {
      pts[[i]] <- out_i
      map[[i]] <- rep.int(i, length(out_i))
    }
  }
  
  # collapse to sfc + parent row map
  pts_vec <- unlist(pts, recursive = FALSE)
  if (!length(pts_vec)) {
    return(list(points = sf::st_sfc(crs = crs)[0], map = integer(0)))
  }
  pts_sfc <- sf::st_sfc(pts_vec, crs = crs)
  map_vec <- unlist(map, use.names = FALSE)
  
  list(points = pts_sfc, map = map_vec)
}

# Exact GEOS radius shortlist with KNN fallback (accurate & fast)
candidates_for_dams_precise <- function(dams, obj,
                                        max_search_dist = 2500,
                                        line_mode = FALSE,
                                        knn_fallback_K = 10) {
  if (is.null(dams) || is.null(obj) || !nrow(dams) || !nrow(obj)) return(NULL)
  stopifnot(sf::st_crs(dams) == sf::st_crs(obj))
  sf::sf_use_s2(FALSE)
  
  rp <- rep_points_strict(obj, line_mode = line_mode)
  op <- rp$points; map <- rp$map
  if (length(op) == 0) return(NULL)
  
  # GEOS spatial index → exact within-distance matches (no big distance matrix)
  # list of integer vectors: for each dam i, which op points are within radius
  hits <- sf::st_is_within_distance(op, sf::st_geometry(dams), dist = max_search_dist) # op → dams
  # flip to per-dam indexing
  dam_hits <- vector("list", length = nrow(dams))
  for (op_i in seq_along(hits)) {
    if (!length(hits[[op_i]])) next
    for (d in hits[[op_i]]) dam_hits[[d]] <- c(dam_hits[[d]], op_i)
  }
  
  dam_idx <- integer(0); obj_idx <- integer(0)
  
  # exact pairs from radius
  for (d in seq_along(dam_hits)) {
    oi <- dam_hits[[d]]
    if (length(oi)) {
      # map rep point → original obj row
      obj_rows <- map[oi]
      # dedupe per dam
      obj_rows <- unique(obj_rows)
      dam_idx <- c(dam_idx, rep(d, length(obj_rows)))
      obj_idx <- c(obj_idx, obj_rows)
    }
  }
  
  # KNN fallback where radius returned nothing (optional but helpful)
  miss <- setdiff(seq_len(nrow(dams)), unique(dam_idx))
  if (length(miss)) {
    dp <- suppressWarnings(sf::st_coordinates(sf::st_geometry(dams)[miss]))
    op_xy <- suppressWarnings(sf::st_coordinates(op))
    dp_ok <- is.finite(dp[,1]) & is.finite(dp[,2])
    op_ok <- is.finite(op_xy[,1]) & is.finite(op_xy[,2])
    if (any(dp_ok) && any(op_ok)) {
      nn <- RANN::nn2(data = op_xy[op_ok, , drop = FALSE],
                      query = dp[dp_ok, , drop = FALSE],
                      k = pmin(knn_fallback_K, sum(op_ok)),
                      searchtype = "standard")
      # map back to original rows
      miss_kept <- miss[dp_ok]
      op_map    <- which(op_ok)[as.vector(nn$nn.idx)]
      dst       <- as.vector(nn$nn.dists)
      keep      <- is.finite(dst) & (dst <= max_search_dist)
      if (any(keep)) {
        dam_idx <- c(dam_idx, rep(miss_kept, each = knn_fallback_K)[keep])
        obj_idx <- c(obj_idx, map[op_map[keep]])
      }
    }
  }
  
  if (!length(dam_idx)) return(NULL)
  # global dedupe
  keep <- !duplicated(data.frame(dam_idx, obj_idx))
  list(dam_idx = dam_idx[keep], obj_idx = obj_idx[keep])
}

sanitize_ctx <- function(x, ctx_name = NA_character_) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
  data.table::setDT(x)
  data.table::setindexv(x, NULL)
  
  # zap geometry/list/function/matrix columns
  if ("geometry" %in% names(x)) x[, geometry := NULL]
  bad_fun  <- vapply(x, is.function, logical(1))
  bad_list <- vapply(x, is.list,      logical(1))
  bad_mat  <- vapply(x, is.matrix,    logical(1))
  for (nm in names(x)[bad_mat]) x[[nm]] <- as.vector(x[[nm]])
  if (any(bad_fun | bad_list)) x[, (names(x)[bad_fun | bad_list]) := NULL]
  
  # enforce consistent types
  req <- list(
    dam_id     = character(), `dam.name` = character(), dam_row = integer(),
    context    = character(), obj_id = character(), `obj.name` = character(),
    jw         = numeric(),   snap_dist = numeric(),
    X          = numeric(),   Y = numeric(),
    area_rank  = numeric(),   on_trib = logical(), rank = integer()
  )
  for (nm in names(req)) if (!nm %in% names(x)) x[, (nm) := req[[nm]][NA_integer_]]
  x[, `:=`(
    dam_id     = as.character(dam_id),
    `dam.name` = as.character(`dam.name`),
    dam_row    = as.integer(dam_row),
    context    = as.character(context %||% ctx_name),
    obj_id     = as.character(obj_id),
    `obj.name` = as.character(`obj.name`),
    jw         = as.numeric(jw),
    snap_dist  = as.numeric(snap_dist),
    X          = as.numeric(X),
    Y          = as.numeric(Y),
    area_rank  = as.numeric(area_rank),
    on_trib    = as.logical(on_trib),
    rank       = as.integer(rank)
  )]
  
  x[, c("dam_id","dam.name","dam_row","context","obj_id","obj.name",
        "jw","snap_dist","X","Y","area_rank","on_trib","rank"), with = FALSE]
}
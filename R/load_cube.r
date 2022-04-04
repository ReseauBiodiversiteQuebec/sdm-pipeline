load_cube <- function(stac_path =
                        "http://io.biodiversite-quebec.ca/stac/",
                      limit = 5000,
                      collections = c('chelsa-clim'),
                      use.obs = T,
                      obs = NULL,
                      lon = "lon",
                      lat = "lat",
                      buffer_box = 0,
                      bbox = NULL,
                      layers = NULL,
                      srs.cube = "EPSG:32198", 
                      t0 = "1981-01-01", 
                      t1 = "1981-01-01",
                      left = -2009488, right = 1401061,  bottom = -715776, top = 2597757,
                      spatial.res = 2000,
                      temporal.res  = "P1Y", 
                      aggregation = "mean",
                      resampling = "near") {
  
  # Creating RSTACQuery  query
  s <- rstac::stac(stac_path)
  
  # use observations to create the bbox and extent
  if (use.obs) {
    
    if (inherits(obs, "data.frame")) {
      # Reproject the obs to the data cube projection
      proj.pts <- project_coords(obs, lon = lon, lat = lat, proj.from = srs.cube)
      
    } else {
      proj.pts <- obs
    }
    
    # Create the extent (data cube projection)
    bbox.proj <- points_to_bbox(proj.pts, buffer = buffer_box)

    
    # Create the bbxo (WGS84 projection)
    bbox.wgs84 <- points_to_bbox(proj.pts, buffer = buffer_box, proj.to ="+proj=longlat +datum=WGS84")
    
  } else {
    
    bbox.proj <- bbox
    if ( bbox.proj$xmin >  bbox.proj$xmax) stop("xmin and xmax seem reversed")
    if ( bbox.proj$ymin >  bbox.proj$ymax) stop("ymin and ymax seem reversed")
    if(!is.null(buffer_box)) {
  bbox.proj <-  sf::st_bbox(sf::st_buffer(sf::st_as_sfc( bbox.proj), dist =buffer_box))
    }


bbox.wgs84 <- sf::st_bbox(bbox.proj,  
            crs = srs.cube) %>%
  sf::st_as_sfc() %>%
  sf::st_transform(crs = 4326) %>% sf::st_bbox()

  }
  
  # Projected coords of bbox
    left <- bbox.proj$xmin
    right <- bbox.proj$xmax
    bottom <- bbox.proj$ymin
    top <- bbox.proj$ymax

  # Create datetime object
  datetime <- format(lubridate::as_datetime(t0), "%Y-%m-%dT%H:%M:%SZ")
  
  if (!is.null(t1) && t1 != t0) {
    datetime <- paste(datetime,
                      format(lubridate::as_datetime(t1), "%Y-%m-%dT%H:%M:%SZ"),
                      sep = "/")
    
  }
  
  
  RCurl::url.exists(stac_path)
  # CreateRSTACQuery object with the subclass search containing all search field parameters 
  it_obj <- s |>
    rstac::stac_search(bbox = bbox.wgs84, collections = collections, 
                       datetime = datetime,
                       limit = limit) |> rstac::get_request() # bbox in decimal lon/lat
  
  # If no layers is selected, get all the layers by default
  if (is.null(layers)) {
    layers <- unlist(lapply(it_obj$features, function(x){names(x$assets)}))
    
  }
  
  # Creates an image collection
  st <- gdalcubes::stac_image_collection(it_obj$features, asset_names = layers) 
  
  v <- gdalcubes::cube_view(srs = srs.cube,  extent = list(t0 = t0, t1 = t1,
                                                           left = left, right = right,  top = top, bottom = bottom),
                            dx = spatial.res, dy = spatial.res, dt = temporal.res, aggregation = aggregation, resampling = resampling)
  gdalcubes::gdalcubes_options(parallel = 4)
  cube <- gdalcubes::raster_cube(st, v)
  
  return(cube)
}

extract_cube_values <- function(cube, df, lon, lat, proj) {
  
  value_points <- gdalcubes::extract_geom(cube, sf::st_as_sf(df, coords = c(lon, lat),
                                                                      crs = proj)) 
  
  df <- df %>% dplyr::mutate(FID = as.integer(rownames(df)))
  df.vals <- dplyr::right_join(df, value_points, by = c("FID")) %>%
       dplyr::select(-FID, -time)
  return(df.vals)
  
}

cube_to_raster <- function(cube, format = "raster") {
  # Transform to a star object
  cube.xy <- cube %>%
    stars::st_as_stars()
  

  # We remove the temporal dimension
  cube.xy <- cube.xy %>% abind::adrop(c(F,F,T))
  
  # Conversion to a spatial object
  
  if (format == "raster") {
    # Raster format
    cube.xy <- raster::stack(as(cube.xy, "Spatial"))
    
  } else {
    # Terra format
    cube.xy <- terra::rast(cube.xy)
  }
  # If not, names are concatenated with temp file names
  names(cube.xy) <- names(cube)
  
  cube.xy
  
}

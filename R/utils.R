#' @param predictors, a raster, either from raster or terra format
#' @param mask, a vector file, either from raster or terra format
#' @return the predictors raster cropped and masked by mask, in terra format
#' @import terra

fast_crop <- function(predictors,
                       mask) {
  
  # convert into terra raster for increased speed
  if (!inherits(predictors, "SpatRaster")) {
    predictors <- terra::rast(predictors)
  }
  
  # convert into a SpatVector
 if (!inherits(mask, "SpatVectors")) {
    mask <- terra::vect(mask)
  }
predictors <- terra::crop(predictors, mask)
  predictors <- terra::mask(predictors, mask, touches = FALSE)

  # convert to raster format for later processing
     return(predictors)
}


#' @name create_projection
#' @param predictors, a raster, either from raster or terra format
#' @param mask, a vector file, either from raster or terra format
#' @return the predictors raster cropped and masked by mask, in terra format
#' @import dplyr
create_projection <- function(obs, lon, lat, proj.from, 
 proj.to, new.lon = NULL, new.lat = NULL) {
  
  if(is.null(new.lon)) {
    new.lon <- lon
  }
  
  if(is.null(new.lat)) {
    new.lat <- lat
  }
  
  new.coords <- project_coords(obs, lon, lat, proj.from, proj.to)
  new.coords.df <- data.frame(new.coords)%>% 
    setNames(c(new.lon, new.lat))
  
  suppressWarnings(obs <- obs %>%
                     dplyr::select(-one_of(c(new.lon, new.lat))) %>% dplyr::bind_cols(new.coords.df))
  
  return(obs)
}


#' @name project_coords
#' @param xy data frame, containing the coordinates to reproject
#' @param lon string, name of the longitude column
#' @param lat string, name of the latitude column
#' @param proj.from character, initial projection of the xy coordinates
#' @param proj.to character, target projection
#' @import sp dplyr
#' @return spatial points in the proj.to projection

project_coords <- function(xy, lon = "lon", lat = "lat", proj.from, proj.to) {
  xy <- dplyr::select(xy, dplyr::all_of(c(lon, lat)))
  sp::coordinates(xy) <-  c(lon, lat)
  sp::proj4string(xy) <- sp::CRS(proj.from)
  xy <- sp::spTransform(xy, sp::CRS(proj.to)) 
  xy
}

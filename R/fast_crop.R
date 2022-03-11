#' @param predictors, a raster, either from raster or terra format
#' @param mask, a vector file, either from raster or terra format
#' @return the predictors raster cropped and masked by mask, in terra format
#' @import terra

fast_crop <- function(predictors,
                       mask) {
  
  # convert into terra raster for increased speed
  if (!class(predictors) %in% c("SpatRaster")) {
    predictors <- terra::rast(predictors)
  }
  
  # convert into a SpatVector
  
  if (!class(predictors) %in% c("SpatVectors")) {
    mask <- terra::vect(mask)
  }
  
  predictors <- terra::crop(predictors, mask)
  predictors <- terra::mask(predictors, mask, touches = FALSE)
  
  # convert to raster format for later processing
  predictors <- raster::stack(predictors) 
  return(predictors)
}

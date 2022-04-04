#'@export
add_predictors <- function(obs, lon = "lon", lat = "lat", predictors){
  if (inherits(predictors, "cube")) {
    predictors <- cube_to_raster(predictors, format = "terra")
  }
  env.vals <- terra::extract(predictors, dplyr::select(obs, dplyr::all_of(c(lon, lat))))
  obs <- dplyr::bind_cols(obs,
                          env.vals) %>% dplyr::select(-ID)
  
  return(obs)
  }

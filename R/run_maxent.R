#' @name run_maxent
#' @param obs data frame, containing the coordinates to reproject
#' @param predictors, raster
#' @param lon string, name of the longitude column (same projection as predictor raster)
#' @param lat string, name of the latitude column (same projection as predictor raster)
#' @param proj character, initial projection of the xy coordinates
#' @return spatial points
#' @export

run_maxent <- function(presence.bg, background, with_raster = F,
                        algorithm = "maxnet",
                        factors = NULL,
                        predictors,
                        partition_type = "crossvalidation",
                        nfolds = 5,
                        orientation_block = "lat_lon",
                        fc = "L", rm =1,
                        parallel = T,
                        updateProgress = T,
                        parallelType = "doParallel"
) {
  
  presence <- presence.bg %>% dplyr::filter(pa == 1)
  background <- presence.bg %>% dplyr::filter(pa == 0)
  
  if (with_raster) {
    ENMmodel <- ENMevaluate(occs = presence[, c("lon", "lat")],
                            bg = background[, c("lon", "lat")], 
                            env = predictors, 
                            categoricals = factors,     
                            algorithm = algorithm,
                            partitions = partition_type, 
                            partition.settings = list(kfold = nfolds, orientation =  orientation_block),
                            tune.args = list(fc = fc, rm = rm),
                            parallel =  parallel,
                            updateProgress = updateProgress,
                            parallelType = )
    
    
  } else {
    covars <- names(presence)[-which(names(presence) %in% c("id", "scientific_name",  "pa"))]
    ENMmodel <- ENMevaluate(occs = presence[,covars], 
                            bg = background[,covars],  
                            algorithm = algorithm,
                            categoricals = factors,
                            partitions = partition_type, 
                            partition.settings = list(kfold = nfolds, orientation =  orientation_block),
                            tune.args = list(fc = fc, rm = rm),
                            parallel =  parallel,
                            updateProgress = updateProgress,
                            parallelType = parallelType)
    
  }
  
  return(ENMmodel)
}

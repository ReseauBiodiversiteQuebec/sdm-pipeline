#' @title Remove collinear variables
#' 
#' @name loadPredictors
#' 
#' @param layersDir, string, path to the folder containing the initial raster layers
#' @param subsetLayers, a vector, containing the name of layers to select. If NULL, all layers in dir.pred selected by default.
#' @param removeCollinear, boolean. If TRUE, an analysis of collinearity is performed. 
#' @param method, The correlation method to be used:"vif.cor", "vif.step", "pearson", "spearman"
#' or "kendall". "vif.cor" and "vif.step" use the Variance Inflation factor and the pearson correlation, more details
#' here https://www.rdocumentation.org/packages/usdm/versions/1.1-18/topics/vif. If your variables are skewed or have outliers 
#' (e.g. when working with precipitation variables) you should favour the Spearman or Kendall methods.
#' @param method.cor.vif, the correlation method to be used with "vif.cor" method. "pearson", "spearman"
#' or "kendall".
#' @param proj, a string , proj if raster layers has to be reprojected
#' @param mask, a polygon to crop the raster
#' @param sample, boolean value. If TRUE, sample a number of points equal to nb.points before evaluating collinearity
#' @param nb.points, a numeric value. Only used if sample.points = TRUE. The number of sampled points from the raster.
#' @param cutoff.cor, a numeric value corresponding to the maximum threshold of linear correlation (for "vif.cor", "pearson", "spearman").
#' @param cutoff.vif, a numeric value corresponding to the maximum threshold of VIF (only used for method "vif.step").
#' @param export, boolean value. If TRUE, the list of selected variables and the correlation matrix will be saved in nonCollinearDir.
#' @param loadNonCollinear, boolean, if TRUE, a list of non-collinear layers is used to subset the raster stack (loaded from nonCollinearDir)
#' @param nonCollinearDir, string, path to the folder export or impot the list of non-collinear layers (if loadNonCollinear = T)
#' @return a raster stack of variables not intercorrelated
#' @import terra raster
#' @export 

load_predictors <- function(layersDir,
                           subsetLayers = NULL,
                           removeCollinear = T,
                           method = "vif.cor",
                           method.cor.vif = NULL,
                           proj = NULL,
                           mask = NULL,
                           sample = TRUE,
                           nb.points = 5000,
                           cutoff.cor = 0.7,
                           cutoff.vif = 3,
                           export = TRUE,
                           loadNonCollinear = F,
                           nonCollinearDir = NULL) {
  
  
  if (!method %in% c("none", "vif.cor", "vif.step", "pearson", "spearman", "kendall")) {
    stop("method must be vif.cor, vif.step, pearson, spearman, or kendall")
  }
  
  if (method %in% c("vif.cor", "pearson", "spearman", "kendall") && is.null(cutoff.cor)) {
    cutoff.cor <- 0.8
  }

  # Load filenames from dir.pred
  files <- list.files(layersDir, pattern = "*.tif", full.names = TRUE)
  
  if (length(files) == 0) {
    stop(sprintf("No tif files found in the directory %s", layersDir))
    
  }
  
  #Load rasterss
  predictorsRaw <- lapply(files,
                          terra::rast)
  predictorsRaw <- terra::rast(predictorsRaw)
  
  if (!is.null(mask)) {
    predictorsRaw <- fast_crop(predictorsRaw, mask)
  }
  
  
  # Selecting specific layers 
  if (!is.null(subsetLayers)) {
    predictorsRaw <- terra::subset(predictorsRaw, subsetLayers)
    
  }
  
  if (!is.null(proj)) {
    predictorsRaw <- terra::project(predictorsRaw, proj)
    
  }
  
  # Selection of non-collinear predictors
  if (removeCollinear) {
    #If using an existing list of non-collinear
    if (loadNonCollinear) {
      if (file.exists(sprintf("%s/retained_predictor.csv",  nonCollinearDir))) {
        nonCollinear <-
          dplyr::pull(read_csv(
            sprintf("%s/retained_predictor.csv",  nonCollinearDir),
            col_names = FALSE
          ), X1)
        predictorsNC <- terra::subset(predictorsRaw, nonCollinear)
        
      } else {
        predictorsNC <- predictorsRaw
        warning("List of non-collinear predictors not found, using all predictors.")
      }
      
      
    } else {
      predictorsNC <- remove_collinearity(
        predictorsRaw,
        method = method,
        sample = sample,
        nb.points = nb.points,
        cutoff.cor = cutoff.cor,
        cutoff.vif = cutoff.vif,
        export = export,
        title = "Correlation plot of climatic and topographic variables.",
        path = nonCollinearDir
      )
      
    }
  } else {
    predictorsNC <- predictorsRaw
  }
  

  return(predictorsNC)
  
  
}


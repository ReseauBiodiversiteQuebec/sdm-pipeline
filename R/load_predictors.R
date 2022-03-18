
#' @name load_predictors
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

load_predictors <- function(from_tif = T,
                            from_cube = F,
                            cube_args = list(stac_path = "http://io.biodiversite-quebec.ca/stac/",
                                             limit = 5000, 
                                             collections = c("chelsa-clim"),     
                                             t0 = "1981-01-01",
                                             t1 = "1981-01-01",
                                             spatial.res = 1000, # in meters
                                             temporal.res = "P1Y",
                                             aggregation = "mean",
                                             resampling = "near",
                                             buffer_box = 200000),
                            predictors_dir = NULL,
                            subset_layers = NULL,
                            remove_collinear = T,
                            method = "vif.cor",
                            method_cor_vif = NULL,
                            new_proj = NULL,
                            mask = NULL,
                            sample = TRUE,
                            nb_points = 5000,
                            cutoff_cor = 0.7,
                            cutoff_vif = 3,
                            export = TRUE,
                            ouput_dir = NULL,
                            as.list = T) {
  
  
  if (!method %in% c("none", "vif.cor", "vif.step", "pearson", "spearman", "kendall")) {
    stop("method must be vif.cor, vif.step, pearson, spearman, or kendall")
  }
  
  if (method %in% c("vif.cor", "pearson", "spearman", "kendall") && is.null(cutoff_cor)) {
    cutoff_cor <- 0.8
  }
  
  if (from_tif) {
    
    
    # Load filenames from dir.pred
    files <- list.files(predictors_dir, pattern = "*.tif", full.names = TRUE)
    
    if (length(files) == 0) {
      stop(sprintf("No tif files found in the directory %s", predictors_dir))
      
    }
    
    #Load rasterss
    all_predictors <- lapply(files,
                             terra::rast)
    all_predictors <- terra::rast(all_predictors)
    
    if (!is.null(mask)) {
      all_predictors <- fast_crop(all_predictors, mask)
    }
    
    
    # Selecting specific layers 
    if (!is.null(subset_layers)) {
      all_predictors <- terra::subset(all_predictors, subset_layers)
      
    }
    
    if (!is.null(new_proj)) {
      all_predictors <- terra::project(all_predictors, new_proj)
      
    }
  } else {
    
    bbox <-  shp_to_bbox(mask)
    if (is.null(new_proj)) {
      srs.cube <- sf::st_crs(mask)$proj4string
    } else {
      srs.cube <- new_proj   
    }
    cube_args_c <- append(cube_args, list(layers = subset_layers, 
                                        srs.cube = srs.cube, use.obs = F, 
                                        bbox = bbox))
    
    all_predictors <- do.call(load_cube, cube_args_c)
    all_predictors <- gdalcubes::filter_geom(all_predictors,  sf::st_geometry(sf::st_as_sf(mask), srs = srs.cube))
     }
  nc_names <- names(all_predictors)
  # Selection of non-collinear predictors
  if (remove_collinear) {
    if (sample) {
      env_df <- sample_spatial_obj(all_predictors, nb_points = nb_points)
    }
    
    nc_names <-detect_collinearity(env_df,
                                   method = method ,
                                   method_cor_vif = method_cor_vif,
                                   cutoff_cor = cutoff_cor,
                                   cutoff_vif = cutoff_vif,
                                   export = export,
                                   title_export = "Correlation plot of climatic and topographic variables.",
                                   path = ouput_dir) 
    
  }                               

  
  if (as.list) {
    output <- nc_names
  
} else {
  if (from_tif) {
    output <- terra::subset(all_predictors, nc_names)
    
  } else {
    
    cube_args_nc <- append(cube_args, list(layers = nc_names, 
                                        srs.cube = srs.cube, use.obs = F, 
                                        bbox = bbox))
    output <- do.call(load_cube, cube_args_nc)
    output <- gdalcubes::filter_geom(output,  sf::st_geometry(sf::st_as_sf(mask), srs = srs.cube))
    
    
  }
}
    return(output)
}

extract_gdal_cube <- function(cube, n_sample = 5000, simplify = T) {
  
  x <- gdalcubes::dimension_values(cube)$x
  y <- gdalcubes::dimension_values(cube)$y
  
  all_points <- expand.grid(x,y) %>% setNames(c("x", "y"))
  
  if (n_sample >= nrow(all_points)) {
    value_points <- gdalcubes::extract_geom(cube, sf::st_as_sf(all_points, coords = c("x", "y"),
                                               crs = gdalcubes::srs(cube))) 
  } else {
    sample_points <- all_points[sample(1:nrow(all_points), n_sample),]
    value_points <- gdalcubes::extract_geom(cube, sf::st_as_sf(sample_points, coords = c("x", "y"),
                                                               crs = gdalcubes::srs(cube))) 
  }
  
  if (simplify) {
    value_points <- value_points %>% dplyr::select(-FID, -time)
  }
  value_points
}
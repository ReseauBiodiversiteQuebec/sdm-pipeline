#' @title Run maxEnt model
#'
#' @name run_maxent
#' @param presence.bg data frame, containing presence/background, output of setup_sdm_data
#' @param with_raster boolean, if F presence.bg must contain the predictors values.
#' @param algorithm string, "maxnet" or "maxent.jar"
#' @param predictors spatRaster, environmental predictor variables. Used if with_raster = T.
#' @param factors character vector, name or names of categorical environmental variables.
#' @param covars character vector, if with_raster = F, list of predictors variables (must match columns names in presence.bg)
#' @param partition_type string, name of partitioning technique: "randomkfold","jackknife","block", "checkerboard1", "checkerboard2", and "none"
#' @param nfolds int, number of folds (for "randomkfold" method)
#' @param orientation_block string, one of "lat_lon" (default), "lon_lat", "lat_lat", or "lon_lon"
#' @param fc vector of strings, feature classes to test, e.g. c("L", "LQ")
#' @param rm vector of float, regularisation multiplier values to test, e.g. c(1,2,3)
#' @param parallel boolean, if TRUE, run with parallel processing.
#' @param updateProgress boolean, if TRUE, use shiny progress bar. This is only for use in shiny apps.
#' @param parallelType character, either "doParallel" or "doSNOW" (default: "doSNOW") .
#' @return spatial points
#' @import ENMeval
#' @export

run_maxent <- function(presence.bg,
                       with_raster = F,
                       algorithm = "maxnet",
                       factors = NULL,
                       predictors = NULL,
                       covars = NULL,
                       partition_type = "crossvalidation",
                       nfolds = 5,
                       orientation_block = "lat_lon",
                       fc = "L",
                       rm = 1,
                       parallel = T,
                       updateProgress = T,
                       parallelType = "doParallel") {
  presence <- presence.bg %>%
    dplyr::filter(pa == 1) %>%
    data.frame()
  background <- presence.bg %>%
    dplyr::filter(pa == 0) %>%
    data.frame()

  if (with_raster) {
    ENMmodel <- ENMeval::ENMevaluate(
      occs = presence[, c("lon", "lat")],
      bg = background[, c("lon", "lat")],
      env = predictors,
      categoricals = factors,
      algorithm = algorithm,
      partitions = partition_type,
      partition.settings = list(kfolds = nfolds, orientation = orientation_block),
      tune.args = list(fc = fc, rm = rm),
      parallel = parallel,
      updateProgress = updateProgress,
      parallelType = parallelType
    )
  } else {
    # covars <- names(presence)[-which(names(presence) %in% c("id", "scientific_name",  "pa"))]
    covars <- c("lon", "lat", covars)
    ENMmodel <- ENMeval::ENMevaluate(
      occs = presence[, covars],
      bg = background[, covars],
      algorithm = algorithm,
      categoricals = factors,
      partitions = partition_type,
      partition.settings = list(kfolds = nfolds, orientation = orientation_block),
      tune.args = list(fc = fc, rm = rm),
      parallel = parallel,
      updateProgress = updateProgress,
      parallelType = parallelType
    )
  }

  return(ENMmodel)
}

#' @title Select best maxEnt parameters
#' 
#' @name select_param
#' @param res data frame, containing result slot of an ENMevaluation object
#' @param method string, "AIC" or "p10"
#' @param auc_min float, minimum value of the AUC
#' @param list boolean, if F the param are returned in the form "fc.L_rm.1", if T as a named list.
#' @return list or string
#' @import dplyr
#' @export
#' 
select_param <- function(res, method = "AIC", auc_min = 0, list = T) {
  if (nrow(res) > 1) {
    res <- res %>%
      filter(auc.val.avg >= auc_min)
    if (nrow(res) == 0) {
      stop(sprintf("All models have AUC lower than %f", auc_min))
    }
    if (method == "AIC") {
      if (nrow(res %>% dplyr::filter(delta.AICc <= 2)) > 0) {
        res <- res %>%
          dplyr::filter(delta.AICc <= 2) %>%
          dplyr::filter(or.10p.avg == min(or.10p.avg)) %>%
          dplyr::filter(auc.val.avg == max(auc.val.avg))
      } else {
        res <- res %>% dplyr::filter(delta.AICc == min(delta.AICc))
      }
    } else if (method == "p10") {
      res <- res %>% filter(or.10p.avg == min(or.10p.avg))
    }
  }
  if (list) {
    param <- list(res$fc, as.double(as.character(res$rm)))
  } else {
    param <- sprintf("fc.%s_rm.%s", res$fc, res$rm)
  }
  return(param)
}

#' @title Create response plot
#' 
#' @name response_plot
#' @param mod ENMevaluation object
#' @param param string, maxent select parameters e.g. "fc.L_rm.1"
#' @param type string, type of output "cloglog" or "raw"
#' @return a plot
#' @import dismo
#' @export

response_plot <- function(mod, param, type = "cloglog", path = NULL) {

  algorithm <- mod@algorithm

  if (algorithm == "maxnet") {
    r_plot <- plot(mod@models[[param]], type = type)
  } else if (algorithm == "maxent.jar") {
    r_plot <- dismo::response(ENMeval::eval.models(mod)[[param]])
  }

  if (!is.null(path)) {
    jpeg(path, width = 900, height = 900)
    # 2. Create the plot
    if (algorithm == "maxnet") {
      r_plot
    } else {
      dismo::response(ENMeval::eval.models(mod)[[param]])
    }
    # 3. Close the file
    dev.off()
  }
  return(r_plot)
}

#' @title Predict with a tuned maxEnt model
#'
#' @name predict_maxent
#' @param mod ENMevaluation object
#' @param param string, maxent select parameters e.g. "fc.L_rm.1"
#' @param predictors spat Raster, environmental predictor variables.
#' @param type string, type of output "cloglog" or "raw"
#' @param mask spatVector, a mask
#' @return raster
#' @import dplyr dismo
#' @export

predict_maxent <- function(mod, param, predictors, type = "cloglog", mask = NULL) {
  if (inherits(predictors, "cube")) {
    predictors <- cube_to_raster(predictors, format = "raster")
  }
  if (!is.null(mask)) predictors <- fast_crop(predictors, mask)

  if (inherits(predictors, "spatRaster")) {
    predictors <- raster::stack(predictors)
  }
algorithm <- mod@algorithm

  if (raster::nlayers(mod@predictions) > 0) {
    pred_raster <- mod@predictions[[param]]
  } else {
    if (algorithm == "maxnet") {
      pred_raster <- dismo::predict(predictors, mod@models[[param]], clamp = T, type = type)
    } else if (algorithm == "maxent.jar") {
      pred_raster <- dismo::predict(mod@models[[param]], predictors,
        args = sprintf("outputformat=%s", type)
      )
    }
  }
  return(pred_raster)
}
#' @title Find threshold to binarize an sdm output
#' 
#' @name find_threshold
#' @param sdm raster, sdm prediction
#' @param occs dataframe, presence points
#' @param bg dataframe, background points
#' @param type string, method to choose the threshold, one of "mtp", "kappa", "spec_sens", "no_omission", "prevalence", "equal_sens_spec", "sensitivity"
#' @return sa float
#' @import dplyr dismo raster
#' @export

find_threshold <- function(sdm, occs, bg, type = "mtp") {
  if (!inherits(sdm, "SpatRaster")) {
    sdm <- terra::rast(sdm)
  }

  if (type == "spse") {
    type <- "spec_sens"
  }
  # extract model estimated suitability for occurrence localities
  # occs_vals <- terra::extract(sdm, occs)
  occs_vals <- terra::extract(sdm, occs) %>%
    dplyr::select(-ID) %>%
    dplyr::pull(1)

  # extract model estimated suitability for background
  bg_vals <- terra::extract(sdm, bg) %>%
    dplyr::select(-ID) %>%
    dplyr::pull(1)

  # taken from https://babichmorrowc.github.io/post/2019-04-12-sdm-threshold/

  if (type == "mtp") {
    thresh <- min(na.omit(occs_vals))
  } else if (type == "p10") {
    if (length(occs_vals) < 10) {
      p10 <- floor(length(occs_vals) * 0.9)
    } else {
      p10 <- ceiling(length(occs_vals) * 0.9)
    }
    thresh <- rev(sort(occs_vals))[p10]
  } else if (type %in% c("kappa", "spec_sens", "no_omission", "prevalence", "equal_sens_spec", "sensitivity")) {

    # evaluate predictive ability of model
    ev <- dismo::evaluate(occs_vals, bg_vals)
    # detect possible thresholds
    thr.table <- dismo::threshold(ev)

    thresh <- thr.table[, type]
  }


  return(thresh)
}

#' @title Binarize a prediction raster
#' 
#' @name binarize_pred
#' @param obs data frame, containing the coordinates to reproject
#' @param predictors, raster
#' @param lon string, name of the longitude column (same projection as predictor raster)
#' @param lat string, name of the latitude column (same projection as predictor raster)
#' @param proj character, initial projection of the xy coordinates
#' @return spatial points
#' @import dplyr dismo raster
#' @export

binarize_pred <- function(sdm, threshold) {
  sdm[sdm < threshold] <- NA
  sdm[sdm >= threshold] <- 1
  return(sdm)
}
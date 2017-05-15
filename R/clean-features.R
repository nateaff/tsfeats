#' Extract set of features from a time series.
#'
#' @param data A time series, matrix of dataframe. 
#' @param features A list or vector of ts_features.
#' @param id Optional prefix for column headers.
#' @param ncores Parallelize feature extraction (Linux only).
#' @param verbose Print status to console.
#' 
#' @return A data frame with row of features for each time.
#'  series in data. 
#'  
#' @export 
get_features <- function(data, features, id = NULL, 
                                             ncores = NULL, 
                                             verbose = FALSE){

  # TODO: switch for system type
  df <- as.data.frame(data)
  if(anyNA(df)) stop("Data contains NA values")
  
  if(!is.null(ncores)){
   cores <- parallel::detectCores()
   if(verbose) cat("Using", cores, " cores ... \n")
  }
  if(!is.list(features)) features <- as.list(c(features))
  
  f <- function(feature){
    if (!is.null(ncores)) {
      ret <- parallel::mclapply(df, function(x) get_one_feature(x, feature), 
                                    mc.cores = ncores)
    } else {
      ret <- lapply(df, function(x) get_one_feature(x, feature))
    }
  do.call(rbind, ret)
  }

  ret <- do.call(cbind, lapply(features, f)) 
  if(!is.null(id)) ret$id <- id
  ret 
} 


get_one_feature <- function(tseries, feature){
  clean_feature(feature(tseries))
}

#' Returns dataframe with feature(s)
#'
#' @param feature The feature to clean
#' @importFrom plyr unrowname
clean_feature <- function(feature) UseMethod("clean_feature")


clean_feature.FractalDim <- function(feature){
  ret <- plyr::unrowname(data.frame(fd = feature$fd)) 
  names(ret) <- paste0("fd_", feature$methods)
  ret
}


clean_feature.fd_variogram <- function(feature){
  ret <- plyr::unrowname(data.frame(fd = feature$fd)) 
  names(ret) <- paste0("fd_", feature$methods)
  class(ret) <- "fd_variogram"
  ret
}

clean_feature.bandpower <- function(feature){
  plyr::unrowname(data.frame((t(unlist(feature)))))
}


clean_feature.ecomp_bspline <- function(feature){
  ret <- plyr::unrowname(data.frame(t(feature$fit$coefficients))) 
  names(ret) <- paste0("bspline_", c("A", "B"))  
  ret
}

clean_feature.ecomp_cspline <- function(feature){
 ret <- plyr::unrowname(data.frame(t(feature$fit$coefficients)))
 names(ret) <- paste0("cspline_", c("A", "B"))  
 ret
}

clean_feature.ecomp_cspline_mse <- function(feature){
 ret <- plyr::unrowname(data.frame(t(feature$fit$coefficients)))
 names(ret) <- paste0("cspline_mse_", c("A", "B"))  
 ret
}

clean_feature.ecomp_cspline_mae <- function(feature){
 ret <- plyr::unrowname(data.frame(t(feature$fit$coefficients)))
 names(ret) <- paste0("cspline_mae_", c("A", "B"))  
 ret
}

clean_feature.ecomp_cspline_max <- function(feature){
 ret <- plyr::unrowname(data.frame(t(feature$fit$coefficients)))
 names(ret) <- paste0("cspline_max_", c("A", "B"))  
 ret
}

clean_feature.ecomp_cspline_rand <- function(feature){
 ret <- plyr::unrowname(data.frame(t(feature$fit$coefficients)))
 names(ret) <- paste0("cspline_rand_", c("A", "B"))  
 ret
}

clean_feature.ecomp_lift <- function(feature){
  ret <- plyr::unrowname(data.frame(t(feature$fit$coefficients))) 
  names(ret) <- paste0("lift_", c("A", "B"))  
  ret
}

clean_feature.ecomp_all <- function(feature){
  ret <- plyr::unrowname(data.frame(t(feature$fit$coefficients))) 
  names(ret) <- paste0("ecomp_all_", c("A", "B"))  
  ret
}

clean_feature.sample_entropy <- function(feature){
  plyr::unrowname(data.frame(sample_entropy = feature[1])) 
}

clean_feature.hurst_pracma <- function(feature){
  # change to Hal = (R/S -AL)
  # plyr::unrowname(data.frame(hurst = feature$Hs))
  plyr::unrowname(data.frame(hurst = feature$Hal))

}

clean_feature.hurst <- function(feature){
  # change to Hal = (R/S -AL)
  # plyr::unrowname(data.frame(hurst = feature$Hs))
  plyr::unrowname(data.frame(hurst = feature@hurst$H))

}

clean_feature.variance <- function(feature){
   plyr::unrowname(data.frame(var = feature[1]))
}

clean_feature.permutation_entropy <- function(feature){
  plyr::unrowname(data.frame(p_entropy = feature[1]))
}

clean_feature.spectral_entropy <- function(feature){
    plyr::unrowname(data.frame(spec_entropy = feature[1]))
}

clean_feature.wvar <- function(feature){
  wav_var <- feature$variance[1:4]
  fnames <- paste0("wvar_", feature$scales[1:4])
  ret <- plyr::unrowname(data.frame(t(wav_var)))
  names(ret) <- fnames
  ret
}
 

clean_feature.default <- function(feature){
  plyr::unrowname(data.frame(feature = feature[1]))
}

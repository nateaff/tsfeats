
#' Compute the permutation entropy of a time series.
#'
#' Calculates the permutation entropy of a time 
#'  series using the pdc package. The miminimum
#'  entropy over the embedding dimensions 3:7 is
#'  returned.
#'
#' @param x A time series or vector.
#'
#' @return The permutation entropy of the time series.
#' @export
#' @importFrom pdc entropyHeuristic
permutation_entropy <- function(x){
  # cat("permutation entropy \n")
  x <- c(x)
  ret <- pdc::entropyHeuristic(x)
  row <- which(ret$entropy.values[, 2] == ret$m)
  ret <- ret$entropy.values[row, 3]
  class(ret) <- "permutation_entropy"
  ret
}

#' Compute the sample entropy of the data
#'
#' @param x The data 
#'
#' @return Sample entropy
#' @export
#' @importFrom pracma sample_entropy
sample_entropy <- function(x){
  # cat("sample entropy \n" )
  s <- c(x)
  ret <- pracma::sample_entropy(x)
  class(ret) <- "sample_entropy"
  ret
}

#' Corrected Hurst exponent
#'
#' The pracma implementation of the corrected Hurst
#'  exponent
#' 
#' @param x The data 
#'
#' @return The features
#' @export
#' @importFrom pracma hurstexp
hurst_pracma <- function(x){
  # if(verbose) cat("hurst \n")
  ret <- pracma::hurstexp(x, d = 50, display = FALSE)
  class(ret) <- "hurst_pracma"
  ret
}

#' Corrected Hurst exponent
#'
#' Wrappter for fArma diffvarFit. The Hurst exponent
#'  is slope of the log-log fit of differenced sample
#'  variances against block size.
#' 
#' @param x The data 
#'
#' @return The features
#' @export
#' @importFrom fArma diffvarFit
hurst <- function(x){
  # cat("hurst \n")
  x <- c(x)
  ret <- fArma::diffvarFit(x)
  class(ret) <- "hurst"
  ret
}

#' Variance wrapper
#'
#' @param x The data 
#'
#' @return The variance
#' @export
variance <- function(x){
  # cat("var \n")
  x <- c(x)
  ret <- var(x)
  class(ret) <- "variance"
  ret
}

#' Compute epsilon complexity using lifting 
#'
#' @param x The data 
#'
#' @return The features
#' @export
#' @importFrom ecomplex ecomplex
ecomp_lift <- function(x){
  # cat("ecomp lift \n")
  res <- ecomplex::ecomplex(x, ds = 5, method = "lift", max_degree = 5)
  class(res) <- "ecomp_lift"
  res
}

#' Compute epsilon complexity using bsplines
#'
#' @param x The data 
#'
#' @return The features
#' @export
#' @importFrom ecomplex ecomplex
ecomp_bspline <- function(x){
  # cat("ecomp bspline \n")
  x <- c(x)
  res <- ecomplex(x, ds = 5, method = "bspline", max_degree = 4)
  class(res) <- "ecomp_bspline"
  res
}

#' Compute epsilon complexity using bsplines
#'
#' @param x The data 
#'
#' @return The features
#' @export
#' @importFrom ecomplex ecomplex
ecomp_all <- function(x){
  # cat("ecomp all \n")
  x <- c(x)
  res <- ecomplex::ecomplex(x, ds = 5, method = "all", max_degree = 5)
  class(res) <- "ecomp_all"
  res
}

#' Compute epsilon complexity using csplines
#'
#' @param x The data 
#'
#' @return The features
#' @export
ecomp_cspline <- function(x){
  # cat("ecomp cspline \n")
  x <- c(x)
  res <- ecomplex::ecomplex(x, ds = 5, method = "cspline")
  class(res) <- "ecomp_cspline"
  res
}

#' Compute epsilon complexity using csplines
#'  with max error.
#'
#' @param x The data 
#'
#' @return The features
#' @export
ecomp_cspline_max <- function(x){
  # cat("ecomp cspline \n")
  x <- c(x)
  res <- ecomplex::ecomplex(x, ds = 5, method = "cspline", err_norm = "max")
  class(res) <- "ecomp_cspline_max"
  res
}

#' Compute epsilon complexity using csplines
#'  and random sampling.
#'
#' @param x The data 
#'
#' @return The features
#' @export
ecomp_cspline_rand <- function(x){
  # cat("ecomp cspline \n")
  x <- c(x)
  res <- ecomplex::ecomplex(x, ds = 5, method = "cspline", sample_type = "random")
  class(res) <- "ecomp_cspline_rand"
  res
}

#' Compute epsilon complexity using csplines with
#'  squared error.
#'
#' @param x The data 
#'
#' @return The features
#' @export
ecomp_cspline_mse <- function(x){
  # cat("ecomp cspline \n")
  x <- c(x)
  res <- ecomplex::ecomplex(x, ds = 5, method = "cspline", err_norm = "mse")
  class(res) <- "ecomp_cspline_mse"
  res
}

#' Compute epsilon complexity using csplines with 
#' mean absolute error.
#'
#'
#' @param x The data 
#'
#' @return The features
#' @export
ecomp_cspline_mae <- function(x){
  # cat("ecomp cspline \n")
  x <- c(x)
  res <- ecomplex::ecomplex(x, ds = 5, method = "cspline", err_norm = "mae")
  class(res) <- "ecomp_cspline_mae"
  res
}

#' Compute spectral entropy
#'
#' Computes the entropy of the binned spectrogram.
#'  Wrapper of ForeCA package function.
#'
#' @param  x Time series
#'
#' @return  return 
#' @export
#' @importFrom ForeCA spectral_entropy
spectral_entropy <- function(x){
  # cat("spectral entropy \n")
  x <- c(x)
  ret <- ForeCA::spectral_entropy(x)
  class(ret) <- "spectral_entropy"
  ret
}

#' Compute bandpower
#'
#' Computes the bandpower on a default set 
#'   
#' @param x The data
#' 
#' @return A data frame of power in each band
#' @export 
bandpower <- function(x){
  # cat("Bandpower \n")
  x <- c(x)
  freqs <- list(
  delta = c(0.5,4),
  theta = c(4,8),
  alpha = c(8, 12),
  beta = c(12, 30),
  gamma = c(30, 100))

  if(!is.ts(x)){
    fs <- 1220
  } else {
    fs <- frequency(x)
  }
  res <- bp_pgram(x, fs=fs, freqs=freqs)
  res <- lapply(res, log)
  class(res) <- "bandpower"
  res
}


#' Variogram-based estimate of fractal dimension. 
#' 
#' This function is a wrapper for the fd.estimate 
#' function of the fractaldim package.
#'
#' @param x The data
#'
#' @return The results from fd.estimate
#' @export
#' @importFrom fractaldim fd.estimate
fd_variogram <- function(x){
  # cat("fd_variogram \n")
  x <- c(x)
  fractaldim::fd.estimate(x, methods = "variogram") 
}

 
#----------------------------------------------------------
# Bandpower filter
#----------------------------------------------------------

#' Find the power within specified frequency bands.
#'
#' Estimates the power within the given frequency 
#'  bands using Welch's method. The periogram is 
#'  then smoothed using wavelet thresholding. The 
#'  sum of power in bins within the frequency band
#'  is used as the estimate of that band's power.
#'
#' @param x The input signal
#' @param fs The signal sampling rate
#' @param freqs A vector or list of vectors of the
#'               bandwidth's whose power should be computed
#' @param psd The output from stats::spec.pgram(). 
#' @param plot_pgram Plots default periodogram if TRUE 
#' @return  A double or list of doubles representing the
#'           signal's frequency in the given bandwidths
bp_pgram <- function(x, fs, freqs, 
                    psd = NULL, 
                    plot_pgram = FALSE){
  if(!is.list(freqs)){
    freqs <- list(freqs)
  }
  if(!is.ts(x)){ 
    x <- ts(x, frequency = fs)
  }
  if(is.null(psd)){ 
    pgram <- stats::spec.pgram(x, plot = plot_pgram, n.used = 100)
  }
  lapply(freqs, function(x) bandpower_one(pgram, x)) 
}


bandpower_one <- function(pgram, bin){
  ran <- which(pgram$freq > bin[1] & pgram$freq < bin[2])
  sum(pgram$spec[ran])
}

## Functions

clim_k_mod <- function(clim, min_clim, max_clim, min_K, max_K) 
{
  if (clim <= min_clim) { ## Too low
    K = min_K
  } else {
    if (clim > max_clim) { ## Too high
      K = max_K
    } else {
      ## climate range
      clim_range <- max_clim - min_clim
      ## climate range
      K_range <- max_K - min_K
      ## clim proportion
      clim_prop <- (clim - min_clim) / clim_range
      ## K_prop
      K = min_K + (K_range * clim_prop)
    }
  }
  return(K)
}

log_mod <- function(t, X0, r, K) {
  # X <- (K*X0) / ( X0-(X0-K)*exp(r*t)) ## From SEEM book
  X <- (K*X0) / ( X0+(K-X0)*exp(-r*t)) ## From maa.org
  # X <- 1 / (1 + (1/X0 -  1) * exp(-r*t)) ## From Wolfram
  return(data.frame(t,X))
}

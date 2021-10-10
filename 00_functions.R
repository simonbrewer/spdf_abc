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

log_mod_fixed <- function(t, X0, r, K) {
  
  X <- (K*X0) / ( X0+(K-X0)*exp(-r*t)) ## From maa.org
  
  return(data.frame(t,X))
}

log_mod_dyn <- function(t, X0, r, K) {
  
  if (length(K) < 2) {stop("K is a single value")}
  
  nt <- length(t)
  X <- rep(NA, nt) 
  X[1] <- X0
  
  for (i in 2:nt) {
    dX <- r * (1 - (X[(i-1)] / K[i])) * X[(i - 1)]
    X[i] <- X[(i-1)] + dX
  }

  return(X)
}

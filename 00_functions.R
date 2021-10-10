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


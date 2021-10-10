## Derive a population curve from the Trace data

dat <- read.csv("./data/AGU_Bonneville_cSPD_T21KClim.csv")

dat <- subset(dat, CalBP <= 2500)

plot(dat$CalBP, dat$AT, type ='l')

## Simple ST -> K model

min_K <- 100
max_K <- 500

min_clim <- 5.2
max_clim <- 6.0

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

## Plot to check
st <- seq(4.5, 6.5, length.out = 200)


clim_k_mod(st[100], min_clim, max_clim, min_K, max_K)

pred_K <- sapply(st, clim_k_mod, min_clim, max_clim, min_K, max_K)

plot(st, pred_K, type ='l')

## Now get set of `K` for the sytrace data
pred_K <- sapply(dat$AT, clim_k_mod, min_clim, max_clim, min_K, max_K)



## Derive a population curve from the Trace data

dat <- read.csv("./data/AGU_Bonneville_cSPD_T21KClim.csv")

dat <- subset(dat, CalBP <= 2500)

plot(dat$CalBP, dat$AT, type ='l')

## Simple ST -> K model

min_K <- 100
max_K <- 500

min_clim <- 5.2
max_clim <- 6.0

## Plot to check
st <- seq(4.5, 6.5, length.out = 200)


clim_k_mod(st[100], min_clim, max_clim, min_K, max_K)

pred_K <- sapply(st, clim_k_mod, min_clim, max_clim, min_K, max_K)

plot(st, pred_K, type ='l')

## Now get set of `K` for the sytrace data
pred_K <- sapply(dat$AT, clim_k_mod, min_clim, max_clim, min_K, max_K)



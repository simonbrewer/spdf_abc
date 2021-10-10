## Derive a population curve from the Trace data
set.seed(42)
source("00_functions.R")

dat <- read.csv("./data/AGU_Bonneville_cSPD_T21KClim.csv")

## Start poopn growth at 3000
dat <- subset(dat, CalBP <= 3000)

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
dat$pred_K <- sapply(dat$AT, clim_k_mod, min_clim, max_clim, min_K, max_K)
dat$t <- 1:nrow(dat)

dat$test_K <- 500
mysigma <- 5.5 ## Noise term
dat$P <- log_mod_dyn(dat$t, 2, r = 0.15, dat$pred_K, 
                     noise = TRUE, mysigma)

plot(dat$t, dat$P)

## Constant scaling to SPDF
spdf_fac <- 0.001 ## 0.1% of people leave a trace
dat$spdf <- dat$P * spdf_fac
plot(dat$t, dat$spdf)
# plot(dat$t, dat$AT)

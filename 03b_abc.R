## Simple ABC 
set.seed(42)
library(coda)
source("03_functions.R")

## Load data
dat <- read.csv("./data/test_spdf.csv")
# dat <- dat[dat$CalBP <= 2500, ]

## Flat priors for the two factors
## Minimum K value
draw_min_K <- function (n) {
  return (runif(n, min=10, max=1000))
}
## Difference to maximum K value
draw_d_K <- function (n) {
  return (runif(n, min=10, max=1000))
}
## Minimum clim value
draw_min_clim <- function (n) {
  return (runif(n, min=1, max=20))
}
## Difference to maximum clim value
draw_d_clim <- function (n) {
  return (runif(n, min=0.1, max=10))
}
## Scaling to spdf
draw_spdf_scale <- function (n) {
  return (runif(n, min=0.0001, max=0.01))
}

# Function to compute the quantiles.
# We choose to use 3 quantiles.
calc_distance <- function(true, simulated) {
  return(sum((true - simulated)^2))
}

## ----------------------------------------------------------------------------
## 'Observed SPDF'
obs <- dat$spdf
n <- length(data)

## Simulation parameters
nbit <- 10000
theta <- 4 ## Acceptance threshold
accept_reject <- rep(NA, nbit)
dists <- rep(NA, nbit)
spdf_sim_acc <- NULL
store_sims <- TRUE

## Get random draws of all parameters
min_K <- draw_min_K(nbit)
max_K <- min_K + draw_d_K(nbit)
min_clim <- draw_min_clim(nbit)
max_clim <- min_clim + draw_d_clim(nbit)
spdf_scale <- draw_spdf_scale(nbit)

## ----------------------------------------------------------------------------
## Run loop
for (i in 1:nbit){
  if (i %% 1000 == 0) {print(paste("Doing", i, "of", nbit))}
  
  ## Uses these to derive a time series of K
  tmp_K <- sapply(dat$AT, clim_k_mod, min_clim[i], max_clim[i], 
                  min_K[i], max_K[i])
  
  ## Use K values to simualte population size
  simulated <- log_mod_dyn(t = dat$t, X0 = 10, r = 0.15, 
                           K = tmp_K, noise = FALSE)
  
  ## Compare to observed (included spdf scaling)
  # dists[i] <- calc_distance(obs, simulated * spdf_scale[i])
  dists[i] <- calc_distance(obs, simulated * spdf_scale[i])
  
  ## Accept or reject
  accept_reject[i] <- ifelse(dists[i] < theta, TRUE, FALSE)
  
  if (accept_reject[i] & store_sims) {
    spdf_sim_acc <- rbind(spdf_sim_acc, simulated * spdf_scale[i])
  }
  
  
}

## Acceptance
sum(accept_reject)
sum(accept_reject) / nbit

param_vals <- data.frame(
  accept_reject,
  min_clim,
  max_clim,
  min_K, 
  max_K,
  spdf_scale
)
mcmc_samples <- 
  mcmc(param_vals[which(param_vals$accept_reject==1), 2:6])

summary(mcmc_samples)

plot(mcmc_samples)

## Plot simulations in ggplot (just when Brian thought he'd gotten away with it)
library(ggplot2)
spdf_mean = apply(spdf_sim_acc, 2, mean)
spdf_lo = apply(spdf_sim_acc, 2, quantile, 0.025)
spdf_hi = apply(spdf_sim_acc, 2, quantile, 0.975)

plot_df <- data.frame(time = dat$t,
                      spdf_mean, spdf_lo, spdf_hi)

ggplot(plot_df, aes(x = time)) +
  geom_ribbon(aes(ymin = spdf_lo, ymax = spdf_hi), fill = 'lightgray') +
  geom_line(aes(y = spdf_mean)) +
  geom_line(dat = dat, aes(x = t, y = spdf)) + 
  theme_bw()
            
            
